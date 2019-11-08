use Rex -feature => '0.45';

use Test::More;
use Data::Dumper;

do "auth.conf";

task "test",
  group => "test",
  sub {

  my $tmp_dir = "/tmp/report-$$";
  file $tmp_dir, ensure => "directory";

  file "$tmp_dir/test1.txt", content => "test1\n";

  file [ "$tmp_dir/test2.txt", "$tmp_dir/test3.txt" ], ensure => "present";

  append_if_no_such_line "$tmp_dir/test1.txt", line => "foobar";

  sed qr{foobar}, "FOOBAR", "$tmp_dir/test1.txt";

  symlink "$tmp_dir/test1.txt", "$tmp_dir/test1.lnk";

  cp "$tmp_dir/test1.lnk", "$tmp_dir/test1.copy";

  rename "$tmp_dir/test1.txt", "$tmp_dir/test1.ren";

  unlink "$tmp_dir/test1.ren";
  unlink "$tmp_dir/nofile1.txt", "$tmp_dir/nofile2.txt";

  upload "report.rex", "$tmp_dir";

  unlink "$tmp_dir/report.rex";

  rmdir $tmp_dir;

  account "foouser",
    ensure  => "present",
    home    => "/home/foouser",
    ssh_key => "ssh-rsa foobar";

  account "root", ensure => "present";

  account "foouser-del",
    name   => "foouser",
    ensure => "absent";

  cron_entry "foo",
    user    => "root",
    command => "ls -l",
    minute  => "*/5",
    ensure  => "present";

  cron_entry "foo-del",
    user    => "root",
    command => "ls -l",
    minute  => "*/5",
    ensure  => "absent";

  host_entry "www.rexify.org",
    ensure => "present",
    file   => "/etc/hosts",
    ip     => "127.0.0.1";

  host_entry "www.rexify.org-del",
    host   => "www.rexify.org",
    ensure => "absent",
    file   => "/etc/hosts";

  my $package = case operating_system, {
    qr{SuSE}i      => 'apache2',
      qr{CentOS}i  => 'httpd',
      qr{Fedora}i  => 'httpd',
      qr{Redhat}i  => 'httpd',
      qr{Ubuntu}i  => 'apache2',
      qr{Debian}i  => 'apache2',
      qr{FreeBSD}i => 'nginx',
      qr{OpenWrt}i => 'uhttpd',
      qr{Mageia}i  => 'apache',
      qr{Gentoo}i  => 'nginx',
      qr{Arch}i    => 'nginx',
  };

  my $service = case operating_system, {
    qr{SuSE}i      => 'apache2',
      qr{CentOS}i  => 'httpd',
      qr{Fedora}i  => 'httpd',
      qr{Redhat}i  => 'httpd',
      qr{Ubuntu}i  => 'apache2',
      qr{Debian}i  => 'apache2',
      qr{FreeBSD}i => 'nginx',
      qr{OpenWrt}i => 'uhttpd',
      qr{Mageia}i  => 'httpd',
      qr{Gentoo}i  => 'nginx',
      qr{Arch}i    => 'nginx',
  };

  pkg $package, ensure => "present";

  service $service => "stop";

  service $service, ensure => "running";

  service "$service-stop",
    ensure => "stopped",
    name   => $service;

  my $to_install = case operating_system, {
    qr{SuSE}i     => "vim",
      qr{CentOS}i => "vim-enhanced",
      qr{Redhat}i => "vim-enhanced",
      qr{Fedora}i => "nano",
      qr{Mageia}i => "vim-enhanced",
      qr{Debian}i => "vim",
      qr{Ubuntu}i => "vim",
      qr{Gentoo}i => "app-misc/screen",
      qr{Arch}i   => "zsh",
      default     => "vim",
  };

  pkg $to_install, ensure => "present";

  pkg "$to_install-del",
    package => $to_install,
    ensure  => "absent";

  run "ls -l";

  run "my-ls-command", command => "ls -l";

  run "my-command",
    command => "ls -l",
    creates => "/etc/hosts";

  my $report = report->{__reports__};
  #print Dumper $report;

  ok( $report->{"file[$tmp_dir]"}->{changed} == 1, "tmp dir created" );
  ok( $report->{"file[$tmp_dir/test1.txt]"}->{changed} == 1,
    "test file created" );
  ok( $report->{"append_if_no_such_line[$tmp_dir/test1.txt]"}->{changed} == 1,
    "appended new line" );
  ok( $report->{"sed[$tmp_dir/test1.txt]"}->{changed} == 1,
    "sed in test1.txt" );

  ok( $report->{"symlink[$tmp_dir/test1.lnk]"}->{changed} == 1,
    "symlink to test1.txt" );

  ok( $report->{"file[$tmp_dir/test2.txt]"}->{changed} == 1,
    "test file created" );

  ok( $report->{"file[$tmp_dir/test3.txt]"}->{changed} == 1,
    "test file created" );

  ok( $report->{"unlink[$tmp_dir/nofile1.txt]"}->{changed} == 0,
    "nofile1 up-to-date" );

  ok( $report->{"unlink[$tmp_dir/nofile2.txt]"}->{changed} == 0,
    "nofile2 up-to-date" );

  ok( $report->{"unlink[$tmp_dir/test1.ren]"}->{changed} == 1,
    "test1.ren unlink'ed" );

  ok( $report->{"rmdir[$tmp_dir]"}->{changed} == 1, "$tmp_dir removed." );

  ok(
    $report->{"rename[$tmp_dir/test1.txt -> $tmp_dir/test1.ren]"}->{changed} ==
      1,
    "$tmp_dir/test1.txt renamed."
  );

  ok(
    $report->{"cp[$tmp_dir/test1.lnk -> $tmp_dir/test1.copy]"}->{changed} == 1,
    "$tmp_dir/test1.lnk copied."
  );

  ok( $report->{"cron_entry[foo]"}->{changed} == 1,     "cron_entry added" );
  ok( $report->{"cron_entry[foo-del]"}->{changed} == 1, "cron_entry removed" );

  ok( $report->{"host_entry[www.rexify.org-del]"}->{changed} == 1,
    "host_entry www.rexify.org removed" );
  ok( $report->{"host_entry[www.rexify.org]"}->{changed} == 1,
    "host_entry www.rexify.org added" );

  ok( $report->{"pkg[$to_install]"}->{changed} == 1, "$to_install installed" );
  ok( $report->{"pkg[$to_install-del]"}->{changed} == 1,
    "$to_install removed" );

  ok( $report->{"run[my-command]"}->{changed} == 0, "my-command not executed" );
  ok( $report->{"run[my-ls-command]"}->{changed} == 1,
    "my-ls-command executed" );
  ok( $report->{"run[ls -l]"}->{changed} == 1, "ls -l executed" );

  ok( $report->{"service[$service]"}->{changed} == 1,      "$service started" );
  ok( $report->{"service[$service-stop]"}->{changed} == 1, "$service stopped" );

  ok( $report->{"upload[$tmp_dir/report.rex]"}->{changed} == 1,
    "$tmp_dir/report.rex uploaded" );

  ok( $report->{"account[root]"}->{changed} == 0, "root account not modified" );
  ok( $report->{"account[foouser]"}->{changed} == 1,
    "created foouser account" );

  done_testing();
  };
