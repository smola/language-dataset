module: deft-test
synopsis:
author: Bruce Mitchener, Jr.
copyright: See LICENSE file in this distribution.

define constant <test-project-vector> = limited(<stretchy-vector>, of: <project-object>);

define variable *tests* :: <test-project-vector> = make(<test-project-vector>);

define function ensure-tests-loaded () => ()
  if (empty?(*tests*))
    let config = deft-config();
    let tests = element(config, "tests", default: #f);
    if (tests & instance?(tests, <sequence>))
      for (test-entry in tests)
        if (instance?(test-entry, <string>))
          load-test-project(test-entry);
        else
          format-err("ERROR: test entries should be strings. Found %=\n", test-entry);
        end if;
      end for;
    elseif (tests)
      format-err("ERROR: 'tests' should be a sequence of strings.\n");
    end if;
    force-out();
    force-err();
  end if;
end;

define function load-test-project (project :: <string>)
  let p = deft-open-project(project);
  if (p)
    if (project-target-type(p) = #"executable")
      add!(*tests*, p);
    else
      format-err("ERROR: %s has a target-type of '%s' rather than 'executable'\n",
                 project-name(p), project-target-type(p));
    end if;
  end if;
end;

define function deft-run-test () => ()
  for (test-project in *tests*)
    block ()
      deft-build-project(test-project);
    exception (e :: <condition>)
      format-out("ERROR: %s\n", e);
    end block;
  end for;
  for (test-project in *tests*)
    format-out("Running %s...\n", test-project.project-executable-name);
    force-out();
    // We use format-out as write to *standard-output* doesn't expand the newline correctly.
    run-application(concatenate("_build/bin/", test-project.project-executable-name),
                    outputter: method (text, #key end: count)
                                 format-out("%s", text);
                               end);
  end for;
end;

define command show tests ($deft-commands)
  help "Show the current test projects.";
  implementation
    begin
      ensure-tests-loaded();
      format-out("Tests:\n");
      if (empty?(*tests*))
        format-out("  *** None ***\n");
      else
        for (test-project in *tests*)
          format-out("  %s (%s)\n", test-project.project-name, test-project.project-filename);
        end for;
      end if;
    end;
end;

define command test ($deft-commands)
  help "Build and run the current set of test projects.";
  implementation
    begin
      ensure-tests-loaded();
      deft-run-test();
    end;
end;
