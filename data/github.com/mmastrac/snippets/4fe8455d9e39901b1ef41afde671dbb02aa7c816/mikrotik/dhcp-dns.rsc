# Configuration
:local config [:toarray ""];
:set ($config->"zone")            "dynamic.mastracci.home";
:set ($config->"commentprefix")   "AUTO ";
:set ($config->"ttl")             00:05:00;

# Important: make sure the script has correct permission policy to read this list!
:global "static-hosts"

# Attempt to load static-hosts from disk
if ([:typeof $"static-hosts"] != "array") do={
  /import file-name=static-hosts.txt
  if ([:typeof $"static-hosts"] != "array") do={
    set "static-hosts" [:toarray ""];
    :log info "Initialized new static-hosts array"
  } else={
    :log info "Loaded static-hosts array from static-hosts.txt"
  }
}

####################################
# Global helper functions
####################################

# Install some helper functions in the global script environment

:global "static-dns" do={ 
  :global "static-hosts"; 

  if ([:typeof $1] != "str") do={
    :put "$0 \"add\" mac-addr \"hostname\"";
    :put "$0 \"remove\" mac-addr";
    :put "$0 \"print\"";
  }

  # $"static-dns" "add" mac-addr "hostname"
  if ($1="add") do={
    :set ($"static-hosts"->[:tostr $2]) $3; 
    :put ("Added mac = $2, host = $3"); 
  }

  # $"static-dns" remove mac-addr
  if ($1="remove") do={
    # Is there a better way to create an empty array?
    :local new [:toarray ""]
    :local found false
    
    :foreach k,v in=$"static-hosts" do={
      if ($k!=[:tostr $2]) do={
        :set ($new->$k) $v;
      } else={
        :put ("Removed mac = $k, host = $v");
        :set found true 
      }
    }

    :if (!$found) do={
      :put "Unable to locate mac $2";
    }

    :global "static-hosts" $new
  }

  # $"static-dns" print
  if ($1="print") do={
    :put "MAC              \tHostname";
    :foreach k,v in=$"static-hosts" do={
      :put ($k."\t".$v);
    }
  }

  :local filecontents ":global \"static-hosts\" [:toarray \"\"]\r\n"

  :foreach k,v in=$"static-hosts" do={
    :set filecontents ($filecontents.":set (\$\"static-hosts\"->\"$k\") \"$v\"\r\n");
  }

  :if ([/file get "static-hosts.txt" contents] != $filecontents) do={
    # Create the static-hosts.txt file using an interface list, then wait for it to appear
    /interface print file=static-hosts.txt
    :while ([:typeof [/file get "static-hosts.txt" name]] != "str") do={
      delay 25ms;
    }

    /file set "static-hosts.txt" contents=$filecontents
    :put "Wrote static-hosts.txt"
  }
} 

####################################
# Utility functions
####################################

# Cleans a hostname, removing invalid characters and converting to lowercase
:local cleanhostname do={
  # Valid DNS characters
  :local validcharsupper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  :local validcharslower "abcdefghijklmnopqrstuvwxyz0123456789-"

  :local hostname ""
  :local rawhostname $1

  # Replacement for weird built-in find that returns a sort-of nil object if it fails, instead returns a -1
  # $find "string" "substring"
  :local find do={
    :local idx [:find $1 $2]
    :if ([:typeof $idx] = "nil") do={
      :return -1;
    } else={
      :return $idx;
    }
  }

  # Clean the DNS name (lowercase, replace invalid chars with dash)
  :if ([:len $rawhostname] > 0) do={
    :local end ([:len $rawhostname]-1)
    # Stop at the first period, in case a client has provided a hostname with a full domain (.)
    :if ($end > [:find $rawhostname "."]) do={
      :set end [:find $rawhostname "."]
    }
    :for i from=0 to=$end do={
      :local char [:pick $rawhostname $i]
      :local idx [$find $validcharsupper $char];
      :if ($idx=-1) do={
        :set idx [$find $validcharslower $char];
        :if ($idx=-1) do={
          # invalid, don't add this character
        } else={
          :set hostname ($hostname.[:pick $validcharslower $idx]);
        }
      } else={
        :set hostname ($hostname.[:pick $validcharslower $idx]);
      }
    }
  }

  :return $hostname
}

# Remove colons from mac
:local cleanmac do={
  :local mac $1
  
  :while ($mac ~ ":") do={
    :local pos [ :find $mac ":" ];
    :set mac ( [ :pick $mac 0 $pos ] . [ :pick $mac ($pos + 1) [:len $mac] ] );
  };

  :return $mac
}


####################################
# Implementation
####################################


# Requires parameters: mac, ip, hostname
:local updatednsentry do={
  :local dnsip
  :local dnsnode
  :local dnsname
  :local comment ($config->"commentprefix".[:tostr $mac])

  /ip dns static;
  
  :set dnsnode [ find where comment~[:tostr $mac] ];
  :if ([:len $dnsnode ] > 0) do={
    :foreach i in=$dnsnode do={
      :set dnsname [ get $i name ]
      :if ($hostname != $dnsname) do={
        :log info ("Removing extra DNS entry: " . $dnsname . " (is now " . $hostname . ")");
        /ip dns static remove $i;
      }
    }
  }

  :set dnsnode [ find where name=$hostname ];
  :if ([:len $dnsnode ] > 0) do={
    # it exists. Is its IP the same?
    :set dnsip [ get $dnsnode address ];
    :if ( $dnsip = $ip ) do={
      :log debug ("DNS entry for " . $hostname . " does not need updating.");
      :return nil
    } else={
      :log info ("Replacing DNS entry for " . $hostname . ", mac: " . $mac . ", ip: " . $ip);
      /ip dns static remove $dnsnode;
    }
  } else={
    # it doesn't exist. Add it
    :log info ("Adding new DNS entry for " . $hostname . ", mac: " . $mac . ", ip: " . $ip);
  }  

  /ip dns static add name=$hostname address=$ip ttl=($config->"ttl") comment=$comment;
}

:local cleanup do={
  :local mac
  :local dhcpnode

  :log debug ("Starting dynamic update cleanup");

  /ip dns static;
  :foreach i in=[ find where comment~("^".$config->"commentprefix") ] do={
    # extract the mac from the comment
    :set mac [ get $i comment ];
    :set mac [ :pick $mac ([:len ($config->"commentprefix")]) ([:len $mac]) ];

    /ip dhcp-server lease;
    :set dhcpnode [ find where mac-address=$mac ];
    :if ( [ :len $dhcpnode ] > 0) do={
      :log debug ("Lease for ".$mac." still exists. Not deleting.");
    } else={
      # there's no lease by that name. Maybe this mac has a static name.
      :log info ("Lease expired for ".$mac.", deleting DNS entry.");
      /ip dns static remove $i;
    }
  }
}

:local update do={
  :global "static-hosts"; 

  :local hostname
  :local mac
  :local ip

  :log debug ("Starting dynamic update addition");

  /ip dhcp-server lease;
  :foreach i in=[find server="dhcp-main"] do={
    :set mac [ get $i mac-address ]

    # Some devices can end up with a nil mac-address for some reason -- just ignore those
    :if ([:typeof $mac] != "nil") do={
      :set ip [ get $i address ];

      # Allow the static host name to be overridden by global static-hosts
      :if (($"static-hosts"->[:tostr $mac]) != nil) do={
        :set hostname ($"static-hosts"->[:tostr $mac])
      } else={
        :set hostname [ get $i host-name ];
      }

      :set hostname [$cleanhostname $hostname];

      # No hostname, so use the mac
      :if ([ :len $hostname ] = 0) do={
        :local vms { "00163E"="xen"; "000C29"="vmware"; "005056"="vmware" }
        :local cleanedmac [$cleanmac $mac]
        :local type ($vms->([:pick $cleanedmac 0 6])) 
        :if ([:typeof $type]="str") do={
          :set hostname ($type . $cleanedmac);
        } else={
          :set hostname "mac$cleanedmac";
        }
      }

      :set hostname ( $hostname . "." . $config->"zone" );

      # Update the DHCP entry's comment if it's different
      :if ([get $i comment] != $hostname) do={
        set $i comment $hostname;
      };

      # Update DNS entry
      $updatednsentry config=$config mac=$mac ip=$ip hostname=$hostname;
    }
  }
}


:log debug ("DNS/DHCP sync. Static host count = ".[:len $"static-hosts"]);

$cleanup config=$config;
$update config=$config cleanhostname=$cleanhostname cleanmac=$cleanmac updatednsentry=$updatednsentry;

:log debug "Completed dynamic updates"
