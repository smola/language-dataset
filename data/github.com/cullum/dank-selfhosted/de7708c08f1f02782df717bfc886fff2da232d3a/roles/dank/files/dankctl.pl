#!/usr/bin/env perl

# This is a massive script to ease the maintenance burden of editing LDAP
# users. It can be used to add/remove/modify user accounts, reset passwords,
# etc.
#
# It assumes you have a DN with R/W access to the ldap tree (configure this
# in /etc/ldapd.conf) and uses the environment variables LDAP_BINDDN and
# LDAP_BINDPW to authenticate. If these variables are unset it will guess
# your bind DN based on your username and prompt you for the password.
#
# Requirements:
#   - Net::LDAP (openbsd pkg: p5-ldap)
#   - LDAP Schemas:
#      - core
#      - inetOrgPerson
#      - nis
#      - openssh-lpk (https://code.google.com/archive/p/openssh-lpk/)
#      - a custom schema which provides the "role" attribute (I use a custom dankUser objectclass)
#
# Notes:
#   - OpenBSD ldapd(8) does not support deleting a specific instance of an
#     attribute - it's all or nothing. In some cases we have to save the
#     current values, munge them, and then restore.
#   - This script ended up way bigger than I expected. Turns out all the validation
#     and error checking required was very complex. I am sorry.

use strict;
use warnings;

use Getopt::Long qw(GetOptionsFromArray :config no_ignore_case);
use File::Basename;
use Net::Domain qw(hostdomain);
use IPC::Open2;
use List::Util qw(min max);
use Net::LDAP;

my $PROG      = basename $0;
my $domain    = hostdomain();
my $ldap      = Net::LDAP->new('localhost') or die "$@\n";
my $basedn    = join(',', map { "dc=$_" } split(/\./, $domain));
my $userdn    = "ou=users,$basedn";
my $groupdn   = "ou=groups,$basedn";
my $roleclass = 'dankAccount';
my $binddn    = $ENV{LDAP_BINDDN} // sprintf("uid=%s,%s", getlogin(), $userdn);
my $bound     = 0;

my $min_uid = 10000;
my $max_uid = 19999;
my $min_gid = 20000;
my $max_gid = 29999;

sub fatal {
  my $err = shift;
  die "$PROG: $err\n";
}

sub flatten {
  return map { ref $_ ? flatten(@{$_}) : $_ } @_;
}

sub prompt_password {
  my $prompt = shift;
  my $password;
  system '/bin/stty', '-echo';
  while (not $password) {
    print "$prompt: ";
    chomp($password = <STDIN>);
    print "\n";
  }
  system '/bin/stty', 'echo';
  return $password;
}

sub bindpw {
  return $ENV{LDAP_BINDPW} if $ENV{LDAP_BINDPW};
  print "Authenticating as $binddn\n";
  return prompt_password('Enter LDAP bind password');
}

sub ldap_bind {
  return { result => $binddn } if $bound == 1;
  my $r = $ldap->bind($binddn, password => bindpw());
  return { error => $r->error } if $r->is_error;
  $bound = 1;
  return { result => $binddn };
}

sub random_password {
  my $length = shift // 32;
  my @alnum = ('a'..'z', 'A'..'Z', 0..9);
  return join '', map $alnum[rand @alnum], 0..$length;
}

sub hash_password {
  my $password = shift;
  open2(my $out, my $in, '/usr/bin/encrypt', '-b', 'a');
  print $in "$password\n";
  close $in;
  chomp(my $hash = <$out>);
  return $hash ? "{CRYPT}$hash" : undef;
}

sub ldap_search {
  my $args = shift;

  my $r = $ldap->search(
    base   => $args->{base},
    scope  => $args->{scope} // 'children',
    filter => $args->{filter},
    attrs  => $args->{attrs}
  );
  return { error => $r->error } if $r->is_error;

  my @entries = map {
    my $e = { dn => $_->dn };
    for my $attr ($_->attributes) {
      my @vals = $_->get_value($attr);
      next unless @vals;
      $e->{$attr} = scalar(@vals) == 1 ? $vals[0] : [@vals];
    }
    $e;
  } $r->entries;
  return { result => [@entries] };
}

sub ldap_add {
  my $args = shift;
  my $r = ldap_bind();
  return $r if $r->{error};

  $r = $ldap->add($args->{dn}, attrs => $args->{attrs});
  return $r->is_error ? { error => $r->error } : { result => $args->{dn} };
}

sub ldap_delete {
  my $args = shift;
  my $r = ldap_bind();
  return $r if $r->{error};

  $r = $ldap->delete($args->{dn});
  return $r->is_error ? { error => $r->error } : { result => 1 };
}

sub ldap_modify {
  my $args = shift;
  my $r = ldap_bind();
  return { error => $r->{error} } if $r->{error};

  $r = $ldap->modify($args->{dn}, changes => $args->{changes});
  return $r->is_error ? { error => $r->error } : { result => 1 };
}

sub get_user {
  my $args = shift;

  my $r = ldap_search({
    base   => $userdn,
    filter => '(&'
      . ($args->{uid} ? "(uidNumber=$args->{uid})" : "(uid=$args->{username})")
      . '(objectClass=posixAccount))'
  });

  return $r->{error} ? $r : { result =>
    @{$r->{result}} == 0 ? undef : $r->{result}[0] };
}

sub get_users {
  return ldap_search({
    base   => $userdn,
    filter => '(objectClass=posixAccount)'
  });
}

sub get_group {
  my $args = shift;

  my $r = ldap_search({
    base   => $groupdn,
    filter => '(&'
      . ($args->{gid} ? "(gidNumber=$args->{gid})" : "(cn=$args->{groupname})")
      . '(objectClass=posixGroup))'
  });

  return $r->{error} ? $r : { result =>
    @{$r->{result}} == 0 ? undef : $r->{result}[0] };
}

sub get_groups {
  return ldap_search({
    base   => $groupdn,
    filter => '(objectClass=posixGroup)'
  });
}

sub next_id {
  my $args = shift;
  my $type = $args->{type};

  my $min  = $args->{min} // ($type eq 'uid' ? $min_uid : $min_gid);
  my $max  = $args->{max} // ($type eq 'uid' ? $max_uid : $max_gid);
  my $base        = $type eq 'uid' ? $userdn : $groupdn;
  my $objectclass = $type eq 'uid' ? 'posixAccount'     : 'posixGroup';
  my $attr        = $type eq 'uid' ? 'uidNumber'        : 'gidNumber';

  my $r = ldap_search({
    base   => $base,
    filter => "(objectClass=$objectclass)",
    attrs  => [$attr]
  });
  return $r if $r->{error};

  my @ids = map { $_->{$attr} } @{$r->{result}};
  return { result => $min } if !@ids;

  @ids = grep { $_ >= $min && $_ <= $max } @ids;
  return @ids
    ? { result => max(@ids) + 1 }
    : { error => "no available IDs in range $min..$max" };
}

sub add_group {
  my $args = shift;
  my $groupname = $args->{groupname};
  my $dn = "cn=$groupname,$groupdn";
  my $r = next_id({type => 'gid'});
  my $gid = $args->{gid} // ($r->{error} ? return $r : $r->{result});

  return { error => "gid $gid already exists" } if get_group({gid => $gid})->{result};
  return { error => "group '$groupname' already exists" } if get_group({groupname => $groupname})->{result};

  my @attrs = (
    objectclass => 'posixGroup',
    cn          => $groupname,
    gidNumber   => $gid
  );
  push @attrs, memberUid   => $args->{members}     if $args->{members};
  push @attrs, description => $args->{description} if $args->{description};

  return ldap_add({
    dn => $dn,
    attrs => [@attrs]
  });
}

sub add_user {
  my $args = shift;
  my $username = $args->{username};
  my $dn = "uid=$username,$userdn";
  my $r = next_id({type => 'uid'});
  my $uid = $args->{uid} // ($r->{error}
    ? return $r
    : $r->{result});

  return { error => "uid $uid already exists" } if get_user({uid => $uid})->{result};
  return { error => "user '$username' already exists" } if get_user({username => $username})->{result};

  for my $groupname (@{$args->{groups}}) {
    $r = get_group({ groupname => $groupname });
    return $r if $r->{error};
    return { error => "group '$groupname' does not exist" } unless $r->{result};
  }

  my $password_hash = $args->{password} =~ /^{CRYPT}/
    ? $args->{password}
    : hash_password($args->{password}) or return { error => "failed to create password hash" };

  $r = add_group({
    groupname => $username,
    gid       => $uid,
    members   => [$username]
  });
  return $r if $r->{error};

  my @attrs = (
    objectClass => [
      'inetOrgPerson',
      'posixAccount',
      'ldapPublicKey',
      $roleclass
    ],
    uid           => $username,
    cn            => $args->{fullname},
    givenName     => (split ' ', $args->{fullname})[0],
    sn            => (split ' ', $args->{fullname})[-1],
    mail          => $args->{mail},
    uidNumber     => $uid,
    gidNumber     => $uid,
    homeDirectory => $args->{homedir},
    loginShell    => $args->{shell},
    role          => $args->{role},
    userPassword  => $password_hash,
  );
  push @attrs, sshPublicKey => $args->{sshkeys} if @{$args->{sshkeys}};

  $r = ldap_add({
    dn    => $dn,
    attrs => [@attrs]
  });

  if ($r->{error}) {
    delete_group({ groupname => $username });
    return $r;
  }

  for my $groupname (@{$args->{groups}}) {
    $r = add_user_to_group({
      username  => $username,
      groupname => $groupname
    });

    if ($r->{error}) {
      delete_user({ username => $username });
      return $r;
    }
  }

  $r = create_homedir({ uid => $uid, dir => $args->{homedir} });
  if ($r->{error}) {
    delete_user({ username => $username });
    return $r;
  }

  return { result => $dn };
}

sub modify_user {
  my $args = shift;
  my $username = $args->{username};
  my $dn = "uid=$username,$userdn";

  my $r = get_user({ username => $username });
  return $r if $r->{error};
  my $user = $r->{result} or return { error => "user '$args->{username}' does not exist" };

  if (($args->{groups}[0] // '') eq 'add') {
    for my $groupname (@{$args->{groups}[1]}) {
      $r = add_user_to_group({
        username  => $username,
        groupname => $groupname
      });
      return $r if $r->{error};
    }
  } elsif (($args->{groups}[0] // '') eq 'replace') {
    $r = get_user_groups({ username => $username });
    return $r if $r->{error};

    for my $group (@{$r->{result}}) {
      $r = remove_user_from_group({
        username  => $username,
        groupname => $group->{cn}
      });
      return $r if $r->{error};
    }

    for my $groupname (@{$args->{groups}[1]}) {
      $r = add_user_to_group({
        username  => $username,
        groupname => $groupname
      });
      return $r if $r->{error};
    }
  }

  my @changes;
  push @changes, replace => [
    cn        => $args->{fullname},
    givenName => (split ' ', $args->{fullname})[0],
    sn        => (split ' ', $args->{fullname})[-1]
  ] if $args->{fullname};

  push @changes, replace => [ homeDirectory => $args->{homedir} ] if $args->{homedir};
  push @changes, replace => [ mail          => $args->{mail}    ] if $args->{mail};
  push @changes, replace => [ role          => $args->{role}    ] if $args->{role};
  push @changes, replace => [ loginShell    => $args->{shell}   ] if $args->{shell};

  if ($args->{password}) {
    my $password_hash = $args->{password} =~ /^{CRYPT}/
      ? $args->{password}
      : hash_password($args->{password}) or return { error => "failed to create password hash" };
    push @changes, replace => [ userPassword  => $password_hash ];
  }

  if (($args->{sshkeys}[0] // '') eq 'delete') {
    if (ref $args->{sshkeys}[1] eq 'ARRAY') {
      my @keys = flatten([$user->{sshPublicKey}]);
      my %delete;
      $delete{$_} = 1 for (flatten([$args->{sshkeys}[1]]));
      push @changes, replace => [ sshPublicKey => [ grep { not $delete{$_} } @keys] ];
    } elsif ($args->{sshkeys}[1] eq 'ALL') {
      push @changes, delete => [ sshPublicKey => [] ];
    }
  } elsif (($args->{sshkeys}[0] // '') eq 'add') {
    push @changes, add => [ sshPublicKey => $args->{sshkeys}[1] ];
  }

  return unless @changes;

  $r = ldap_modify({
    dn      => $dn,
    changes => [@changes]
  });
  return $r if $r->{error};

  print "warning: home directory for $username was changed, but the filesystem was not modifed.\n",
        "manual intervention may be required: $user->{homeDirectory} -> $args->{homedir}\n"
    if $args->{homedir};

  return {result => 1 };
}

sub create_homedir {
  my $args = shift;
  my $dir = $args->{dir};
  my $uid = $args->{uid};

  if (-d $dir) {
    print "warning: $dir already exists\n";
  } else {
    my $ok;
    if ($< == 0) {
      $ok   = mkdir($dir, 0700);
      $ok &&= chown($uid, $uid, $dir);
    } else {
      print "invoking doas to create home directory\n";
      my %ENV_COPY = %ENV;
      %ENV = ();
      $ok   = system('/usr/bin/doas', '/bin/mkdir', $dir);
      $ok ||= system('/usr/bin/doas', '/bin/chmod', '700', $dir);
      $ok ||= system('/usr/bin/doas', '/usr/sbin/chown', "$uid:$uid", $dir);
      $ok = !$ok;
      %ENV = %ENV_COPY;
    }

    if (not $ok) {
      my $error = "could not create $dir: $!";
      rmdir($dir);
      return { error => $error };
    }
  }

  return { result => $dir };
}

sub delete_group {
  my $args = shift;

  my $r = get_group($args);
  return $r if $r->{error};

  my $group = $r->{result} or return { error =>
    ($args->{gid} ? "gid $args->{gid}" : "group '$args->{groupname}'") . ' does not exist'
  };

  return ldap_delete({ dn => $group->{dn} });
}

sub modify_group {
  my $args = shift;
  my $groupname = $args->{groupname};
  my $dn = "cn=$groupname,$groupdn";

  my $r = get_group({ groupname => $groupname });
  return $r if $r->{error};
  my $group = $r->{result} or return { error => "group '$groupname' does not exist" };

  my @changes;
  push @changes, replace => [ description => $args->{description} ] if $args->{description};

  $r = ldap_modify({
    dn      => $dn,
    changes => [@changes]
  });

  return $r->{error} ? $r : { result => 1};
}

sub get_user_groups {
  my $args = shift;

  my $r = get_user($args);
  return $r if $r->{error};
  my $user = $r->{result} or return { error =>
    ($args->{uid} ? "uid $args->{uid}" : "user '$args->{username}'") . ' does not exist'
  };

  return ldap_search({
    base   => $groupdn,
    filter => "(&(objectClass=posixGroup)(memberUid=$user->{uid})(!(cn=$user->{uid})))"
  });
}

sub add_user_to_group {
  my $args = shift;

  my $r = get_group($args);
  return $r if $r->{error};
  my $group = $r->{result} or return { error =>
    ($args->{gid} ? "gid $args->{gid}" : "group '$args->{groupname}'") . ' does not exist'
  };

  $r = get_user($args);
  return $r if $r->{error};
  my $user = $r->{result} or return { error =>
    ($args->{uid} ? "uid $args->{uid}" : "user '$args->{username}'") . ' does not exist'
  };

  return { result => 1 } if grep { $_ eq $user->{uid} } flatten($group->{memberUid} // []);

  return ldap_modify({
    dn      => $group->{dn},
    changes => [ add => [ memberUid => $user->{uid} ] ]
  });
}

sub remove_user_from_group {
  my $args = shift;

  my $r = get_group($args);
  return $r if $r->{error};
  my $group = $r->{result} or return { error =>
    ($args->{gid} ? "gid $args->{gid}" : "group '$args->{groupname}'") . ' does not exist'
  };

  $r = get_user($args);
  return $r if $r->{error};
  my $user = $r->{result} or return { error =>
    ($args->{uid} ? "uid $args->{uid}" : "user '$args->{username}'") . ' does not exist'
  };

  return { result => 1 } unless grep { $_ eq $user->{uid} } flatten($group->{memberUid});
  my @members = grep { $_ ne $user->{uid} } flatten($group->{memberUid});

  return ldap_modify({
    dn      => $group->{dn},
    changes => [ replace => [ memberUid => [@members] ] ]
  });
}

sub delete_user {
  my $args = shift;
  my $user;

  my $r = get_user($args);
  return $r if $r->{error};
  $user = $r->{result} or return { error =>
    ($args->{uid} ? "uid $args->{uid}" : "user '$args->{username}'") . ' does not exist'
  };

  $r = get_group({ groupname => $user->{uid} });
  return $r if $r->{error};
  my $group = $r->{result};
  if ($group) {
    $r = delete_group({ groupname => $group->{cn} });
    return $r if $r->{error};
  } else {
    print "warning: user $user->{uid} does not have a corresponding group\n";
  }

  $r = get_user_groups({ username => $user->{uid} });
  return $r if $r->{error};
  for my $group (@{$r->{result}}) {
    $r = remove_user_from_group({
      username  => $user->{uid},
      groupname => $group->{cn}
    });
    return $r if $r->{error};
  }

  $r = ldap_delete({ dn => $user->{dn} });
  return $r if $r->{error};

  print "home directory left unmodified: $user->{homeDirectory}\n" if -d $user->{homeDirectory};
  return { result => 1 };
}


sub useradd {
  my $usage = <<EOF;
Usage: $PROG useradd [options] USERNAME

Options:
  -c, --fullname NAME      first and last name for the new account (required)
  -d, --homedir DIR        home directory of the new account (default: /home/username)
  -f, --keyfile            file containing SSH public keys for the new account
  -G, --groups GROUPS      secondary groups for the new account (comma separated)
  -h, --help               display this help message and exit
  -k, --key KEY            ssh public key for the new account (can be used multiple times)
  -l, --lock               lock the new account upon creation
  -m, --mail EMAIL         email address for the new account
  -P, --prompt-password    read the account's password from stdin
  -p, --password PASSWORD  password for the new account (default: randomly generated)
  -r, --role ROLE          role of the new account (default: default)
  -s, --shell SHELL        login shell of the new account (default: /sbin/nologin)
  -u, --uid UID            user ID of the of the new account (default: next available)
EOF

  my ($username, $fullname, $homedir, $mail, $keyfile, $uid, $password, $locked, @sshkeys, @groups);
  my $shell = '/sbin/nologin';
  my $role = 'default';
  my $prompt_password = 0;
  my $random_password = 0;

  GetOptionsFromArray(\@_,
    'c|fullname=s'       => \$fullname,
    'd|homedir=s'        => \$homedir,
    'f|keyfile=s'        => \$keyfile,
    'G|groups=s'         => \@groups,
    'h|help'             => sub { print $usage; exit },
    'k|key=s'            => \@sshkeys,
    'l|lock'             => \$locked,
    'm|mail=s'           => \$mail,
    'P|prompt-password'  => \$prompt_password,
    'p|password=s'       => \$password,
    'r|role=s'           => \$role,
    's|shell=s'          => \$shell,
    'u|uid=i'            => \$uid,
  ) or die $usage;
  $username = shift or die $usage;
  die $usage if @_;

  $mail    //= "$username\@$domain";
  $homedir //= "/home/$username";
  @groups = split(/,/,join(',',@groups));

  fatal 'invalid username' unless $username =~ /^[[:alnum:]_-]+$/;
  fatal 'invalid uid' if ($uid && $uid <= 0);
  fatal 'no fullname specified' unless $fullname;
  fatal 'invalid fullname, should be "first last"' unless $fullname =~ /^[\w-]+\s+[\w-]+$/;
  fatal 'parent of homedir does not exist' unless -d dirname($homedir);
  fatal 'invalid email address' unless $mail =~ /@/;
  fatal 'shell is not a valid executable' unless -x $shell;
  fatal '--lock (-l) and --prompt-password (-P) are mutually exclusive' if ($locked && $prompt_password);
  fatal '--lock (-l) and --password (-p) are mutually exclusive' if ($locked && $password);
  fatal '--password (-p) and --prompt-password (-P) are mutually exclusive' if ($password && $prompt_password);
  fatal '--key (-k) and --keyfile (-f) are mutually exclusive' if (@sshkeys && $keyfile);
  fatal 'keyfile is not readable' if ($keyfile && !(-f $keyfile && -r $keyfile));
  fatal 'invalid role' unless $username =~ /^[[:alnum:]_-]+$/;

  $password = '{CRYPT}*' if $locked;
  $password = prompt_password("Enter password for $username") if $prompt_password;
  unless (defined $password) {
    $password = random_password();
    $random_password = 1;
  }
  fatal 'you must specify a password' if $password =~ /^\s*$/;

  if ($keyfile) {
    open my $fh, "<", $keyfile or fatal "failed to open keyfile: $!\n";
    chomp (@sshkeys = <$fh>);
  }

  my $r = add_user({
    username => $username,
    uid      => $uid,
    fullname => $fullname,
    mail     => $mail,
    homedir  => $homedir,
    shell    => $shell,
    role     => $role,
    password => $password,
    sshkeys  => [@sshkeys],
    groups   => [@groups],
  });
  fatal $r->{error} if $r->{error};

  print "password: $password\n" if $random_password;
}

sub usermod {
  my $usage = <<EOF;
Usage: $PROG usermod [options] USERNAME

Options:
  -a, --append          add to, rather than replace, the account's secondary groups (use with -G)
  -c, --fullname NAME   first and last name for the account
  -d, --homedir DIR     home directory of the account
  -f, --keyfile         file containing SSH public keys to add for the account
  -G, --groups GROUPS   secondary groups for the account (comma separated, see -a option)
  -h, --help            display this help message and exit
  -K, --delete-key KEY  ssh key(s) to delete from the account, or "ALL"
  -k, --key KEY         ssh public key to add for the account (can be used multiple times)
  -l, --lock            lock the account
  -m, --mail EMAIL      email address for the account
  -r, --role ROLE       role for the account (default, admin)
  -s, --shell SHELL     login shell of the account

Notes:
  ldapd(8) does not currently support the modrdn operation. Therefore, usernames
  cannot be changed, since the DNs would have to change as a result. If you need
  to change a username, you must delete and re-add the account.
EOF

  my ($username, $fullname, $homedir, $mail, $role, $shell, $keyfile, $append, $locked, @sshkeys_add, @sshkeys_del, @groups);

  GetOptionsFromArray(\@_,
    'a|append'       => \$append,
    'c|fullname=s'   => \$fullname,
    'd|homedir=s'    => \$homedir,
    'f|keyfile=s'    => \$keyfile,
    'G|group:s'      => \@groups,
    'h|help'         => sub { print $usage; exit },
    'K|delete-key=s' => \@sshkeys_del,
    'k|key=s'        => \@sshkeys_add,
    'l|lock'         => \$locked,
    'm|mail=s'       => \$mail,
    'r|role=s'       => \$role,
    's|shell=s'      => \$shell,
  ) or die $usage;
  $username = shift or die $usage;
  die $usage if @_;
  ($fullname or $homedir or $mail or $role or $shell or $keyfile or @sshkeys_add or @sshkeys_del or @groups) or die $usage;

  fatal 'invalid fullname, should be "first last"' if ($fullname and $fullname !~ /^[\w-]+\s+[\w-]+$/);
  fatal 'parent of homedir does not exist' if ($homedir and ! -d dirname($homedir));
  fatal 'invalid email address' if ($mail and $mail !~ /@/);
  fatal 'shell is not a valid executable' if ($shell && ! -x $shell);
  fatal '--key (-k) and --keyfile (-f) are mutually exclusive' if (@sshkeys_add and $keyfile);
  fatal '--delete-key (-K) and --keyfile (-f) are mutually exclusive' if (@sshkeys_del and $keyfile);
  fatal '--delete-key (-K) and --key (-k) are mutually exclusive' if (@sshkeys_del and @sshkeys_add);
  fatal 'keyfile is not readable' if ($keyfile && !(-f $keyfile && -r $keyfile));
  fatal 'no groups specified' if ($append && scalar(@groups) == 1 && !$groups[0]);

  @groups = split(/,/,join(',',@groups));
  @groups = () if @groups && !$groups[0];

  if ($keyfile) {
    open my $fh, "<", $keyfile or fatal "failed to open keyfile: $!\n";
    chomp (@sshkeys_add = <$fh>);
  }

  my %changes ;
  $changes{username} = $username;
  $changes{fullname} = $fullname;
  $changes{homedir}  = $homedir;
  $changes{mail}     = $mail;
  $changes{role}     = $role;
  $changes{shell}    = $shell;
  $changes{groups}   = [ ($append ? 'add' : 'replace') => [@groups] ] if @groups;
  $changes{password} = '{CRYPT}*' if $locked;

  $changes{sshkeys} = [ add    => [@sshkeys_add] ] if @sshkeys_add;
  $changes{sshkeys} = [ delete => [@sshkeys_del] ] if @sshkeys_del;;
  $changes{sshkeys} = [ delete => 'ALL'          ] if grep { uc($_) eq 'ALL' } @sshkeys_del;

  my $r = modify_user(\%changes);
  fatal $r->{error} if $r->{error};
}

sub userdel {
  my $usage =  <<EOF;
Usage: $PROG userdel [options] USERNAME

Options:
  -h, --help  show this help message and exit
EOF

  GetOptionsFromArray(\@_,
    'h|help' => sub { print $usage; exit },
  ) or die $usage;
  my $username = shift or die $usage;
  die $usage if @_;

  my $r = delete_user({ username => $username });
  fatal $r->{error} if $r->{error};
}

sub resetpass {
  my $usage =  <<EOF;
Usage: $PROG resetpass [options] [USERNAME]

Options:
  -h, --help               show this help message and exit
  -P, --prompt-password    read the new password from stdin
  -p, --password PASSWORD  password for the new account (default: randomly generated)

Notes:
  Resetting an account's password will unlock the account.
EOF

  my $password = undef;
  my $prompt_password = 0;
  my $random_password = 0;

  GetOptionsFromArray(\@_,
    'h|help'        => sub { print $usage; exit },
    'P|prompt-password'  => \$prompt_password,
    'p|password=s'       => \$password,
  ) or die "$usage\n";
  my $username = shift // getlogin();
  die $usage if @_;

  fatal '--password (-p) and --prompt-password (-P) are mutually exclusive' if ($password && $prompt_password);

  $password = prompt_password("Enter password for $username") if $prompt_password;
  unless (defined $password) {
    $password = random_password();
    $random_password = 1;
  }
  fatal 'you must specify a password' if $password =~ /^\s*$/;

  my $r = modify_user({
    username => $username,
    password => $password
  });
  fatal $r->{error} if $r->{error};

  print "new password: $password\n" if $random_password;
}


sub userinfo {
  my $usage =  <<EOF;
Usage: $PROG userinfo [options] [USERNAME]

Options:
  -h, --help  show this help message and exit
EOF

  GetOptionsFromArray(\@_,
    'h|help' => sub { print $usage; exit },
  ) or die $usage;
  my $username = shift // getlogin();
  die $usage if @_;

  my $r = get_user({ username => $username });
  fatal $r->{error} if $r->{error};
  my $u = $r->{result} or die "$PROG: user '$username' does not exist\n";

  $r = get_user_groups({ username => $username });
  fatal $r->{error} if $r->{error};

  my $w = 8;
  printf "%${w}s: %s\n", 'username',  $u->{uid};
  printf "%${w}s: %d\n", 'uid',       $u->{uidNumber};
  printf "%${w}s: %d\n", 'gid',       $u->{gidNumber};
  printf "%${w}s: %s\n", 'realname',  $u->{cn};
  printf "%${w}s: %s\n", 'homedir',   $u->{homeDirectory};
  printf "%${w}s: %s\n", 'shell',     $u->{loginShell};
  printf "%${w}s: %s\n", 'mail',      $u->{'mail'};
  printf "%${w}s: %s\n", 'role',      $u->{'role'};
  printf "%${w}s: %s\n", 'groups',    (join(',', map { $_->{cn} } @{$r->{result}}) or '[none]');

  if ($u->{sshPublicKey}) {
    for my $key (flatten($u->{sshPublicKey})) {
      printf "%${w}s: %s\n", 'sshkey', $key;
    }
  } else {
    printf "%${w}s: %s\n", 'sshkey', '[none]';
  }
}

sub userlist {
  my $usage =  <<EOF;
Usage: $PROG userlist [options]

Options:
  -h, --help  show this help message and exit
EOF

  GetOptionsFromArray(\@_,
    'h|help' => sub { print $usage; exit },
  ) or die "$usage\n";
  die $usage if @_;

  my $r = get_users();
  fatal $r->{error} if $r->{error};

  for my $u (sort { $a->{uid} cmp $b->{uid} } @{$r->{result}}) {
    my $r = get_user_groups({ username => $u->{uid} });
    return $r if $r->{error};

    printf "%s:%d:%d:%s:%s:%s:%s:%s:%d:%s\n",
      $u->{uid},
      $u->{uidNumber},
      $u->{gidNumber},
      $u->{cn},
      $u->{homeDirectory},
      $u->{loginShell},
      $u->{mail},
      $u->{role},
      scalar(flatten($u->{sshPublicKey} // [])),
      join(',', map { $_->{cn} } @{$r->{result}});
  }
}

sub groupadd {
  my $usage = <<EOF;
Usage: $PROG groupadd [options] GROUP

Options:
  -c, --description DESC  description of the new group
  -g, --gid GID           group ID of the of the new gruop
  -h, --help              display this help message and exit
EOF

  my ($description, $gid);

  GetOptionsFromArray(\@_,
    'c|description=s'  => \$description,
    'g|gid=i'          => \$gid,
    'h|help'             => sub { print $usage; exit },
  ) or die $usage;
  my $groupname = shift or die $usage;
  die $usage if @_;

  fatal 'invalid group' unless $groupname =~ /^[[:alnum:]_-]+$/;
  fatal 'invalid gid' if ($gid && $gid <= 0);

  my $r = add_group({
    groupname   => $groupname,
    gid         => $gid,
    description => $description
  });

  fatal $r->{error} if $r->{error};
}

sub groupdel {
  my $usage =  <<EOF;
Usage: $PROG groupdel [options] GROUP

Options:
  -h, --help  show this help message and exit
EOF

  GetOptionsFromArray(\@_,
    'h|help' => sub { print $usage; exit },
  ) or die $usage;
  my $groupname = shift or die $usage;
  die $usage if @_;

  my $r = delete_group({ groupname => $groupname });
  fatal $r->{error} if $r->{error};
}

sub groupmod {
  my $usage = <<EOF;
Usage: $PROG groupmod [options] GROUP

Options:
  -c, --description DESC  description of the group
  -h, --help              show this help message and exit

Notes:
  ldapd(8) does not currently support the modrdn operation. Therefore, group names
  cannot be changed, since the DNs would have to change as a result. If you need
  to change a group name, you must delete and re-add the group.
EOF

  my $description;

  GetOptionsFromArray(\@_,
    'c|description=s'  => \$description,
    'h|help'           => sub { print $usage; exit }
  ) or die $usage;
  my $groupname = shift or die $usage;
  die $usage if @_;

  my $changes = { groupname => $groupname };
  $changes->{description} = $description;

  my $r = modify_group($changes);
  fatal $r->{error} if $r->{error};
}

sub grouplist {
  my $usage =  <<EOF;
Usage: $PROG grouplist [options]

Options:
  -h, --help  show this help message and exit
EOF

  GetOptionsFromArray(\@_,
    'h|help' => sub { print $usage; exit },
  ) or die "$usage\n";
  die $usage if @_;

  my $r = get_groups();
  fatal $r->{error} if $r->{error};

  for my $g (sort { $a->{cn} cmp $b->{cn} } @{$r->{result}}) {
    printf "%s:%d:%s:%s\n",
      $g->{cn},
      $g->{gidNumber},
      $g->{description} // '',
      join(',', flatten($r->{memberUid} // []));
  }
}

sub groupinfo {
  my $usage =  <<EOF;
Usage: $PROG groupinfo [options] GROUP

Options:
  -h, --help  show this help message and exit
EOF

  GetOptionsFromArray(\@_,
    'h|help' => sub { print $usage; exit },
  ) or die $usage;
  my $groupname = shift or die $usage;
  die $usage if @_;

  my $r = get_group({ groupname => $groupname });
  fatal $r->{error} if $r->{error};
  my $g = $r->{result} or die "$PROG: group '$groupname' does not exist\n";

  my $w = 11;
  printf "%${w}s: %s\n", 'group',       $g->{cn};
  printf "%${w}s: %d\n", 'gid',         $g->{gidNumber};
  printf "%${w}s: %d\n", 'description', $g->{gidNumber};

  if ($g->{memberUid}) {
    for my $u (flatten($g->{memberUid})) {
      printf "%${w}s: %s\n", 'member', $u;
    }
  } else {
    printf "%${w}s: %s\n", 'member', '[none]';
  }
}

my $usage = <<EOF;
Usage: $PROG COMMAND [options]

Commands:
  Help:
    help       display this help

  Users:
    useradd    add a new user
    userdel    delete a user account
    usermod    modify a user account
    userinfo   show a user account
    userlist   list all user accounts
    resetpass  reset an account password

  Groups:
    groupadd   add a new group
    groupdel   delete a group
    groupmod   modify a group
    groupinfo  show a group
    grouplist  list all groups
EOF

my %cmds = (
  help      => sub {print $usage; exit },
  useradd   => \&useradd,
  userdel   => \&userdel,
  usermod   => \&usermod,
  userinfo  => \&userinfo,
  userlist  => \&userlist,
  resetpass => \&resetpass,
  groupadd  => \&groupadd,
  groupdel  => \&groupdel,
  groupmod  => \&groupmod,
  groupinfo => \&groupinfo,
  grouplist => \&grouplist,
);

my $cmd = shift or die $usage;
($cmds{$cmd} || sub {die $usage})->(@ARGV);
