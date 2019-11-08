
import org.codejive.ceylon.options { ... }
import ceylon.test { ... }

// Show output on success?
Boolean verbose = false;

shared test void testRun() {
    value opts = Options {
        usage = "Usage: ceylon acme.doodle <options> <things>";
        noArgsHelp = "use -h or --help for a list of possible options";
        options = [ Option("help", ["h","help"], "This help"),
        Option {
            name="file";
            matches=["f","file"];
            docs="The file to read, can be used multiple times";
            hasValue=true;
            required=true;
            multiple=true;
        },
        Option {
            name="out";
            matches=["o","out"];
            docs="The output file";
            hasValue=true;
        },
        Option {
            name="debug";
            matches=["d","debug"];
            docs="Enable debugging";
        },
        Option {
            name="logging";
            matches=["v","verbose"];
            docs="Enable logging";
            hasOptionalValue=true;
            defaultOptionalValue="all";
        } ];
    };

    void testError(String[] args, String[] msg) {
        process.write("---- Testing parse(``args``) -- ");
        try {
            value res = opts.parse(args);
            switch(res)
            case (is Options.Result) {
                value err = opts.validate(res);
                if (exists err) {
                    if (err.messages == msg) {
                        process.writeLine("Okay");
                        if (verbose) {
                            print(res);
                        }
                    } else {
                        process.writeLine("FAILED (validation failed with wrong message)");
                        print("Actual  : ``err.messages``");
                        print("Expected: ``msg``");
                    }
                } else {
                    process.writeLine("FAILED (expecting Error got Result)");
                    print(res);
                }
            }
            case (is Options.Error) {
                if (res.messages == msg) {
                    process.writeLine("Okay");
                    if (verbose) {
                        print(res);
                    }
                } else {
                    process.writeLine("FAILED (wrong message)");
                    print("Actual  : ``res.messages``");
                    print("Expected: ``msg``");
                }
            }
        } catch (Exception ex) {
            process.writeLine("FAILED (with Exception)");
            ex.printStackTrace();
        }
    }
    
    void testResult(String[] args, Entry<String, Sequence<String>>[] optmap, String[] rest) {

        Boolean compare(Map<String,Sequence<String>> options, Entry<String,Sequence<String>>[] optmap) {
            if (options.size != optmap.size) {
                return false;
            }
            
            for (entry in optmap) {
                if (exists v=options[entry.key], v != entry.item) {
                    return false;
                } 
            }
            
            return true;
        }
    
        process.write("---- Testing parse(``args``) -- ");
        try {
            value res = opts.parse(args);
            if (is Options.Result res) {
                if (compare(res.options, optmap) && res.arguments == rest) {
                    if (res.options.defines("help")) {
                        process.writeLine("Okay");
                        if (verbose) {
                            opts.printUsage();
                            opts.printHelp();
                        }
                    } else {
                        value err = opts.validate(res);
                        if (exists err) {
                            process.writeLine("FAILED (validation failed)");
                            print(err.messages);
                        } else {
                            process.writeLine("Okay");
                            if (verbose) {
                                print(res);
                            }
                        }
                    }
                } else {
                    process.writeLine("FAILED");
                    print("Actual  : ``res.options`` -- ``res.arguments``");
                    print("Expected: ``optmap`` -- ``rest``");
                }
            } else {
                process.writeLine("FAILED (expecting Result got Error)");
                print(res);
            }
        } catch (Exception ex) {
            process.writeLine("FAILED (with Exception)");
            ex.printStackTrace();
        }
    }

    // Check help/usage messages
    testError([], ["Usage: ceylon acme.doodle <options> <things>\nuse -h or --help for a list of possible options"]);
    testResult(["-h"], ["help"->["true"]], []);
    testResult(["--help"], ["help"->["true"]], []);
    // Check missing required opts
    testError(["aap"], ["Option -f or --file is required"]);
    testError(["-f"], ["Missing value for option -f"]);
    testError(["--file"], ["Missing value for option --file"]);
    // Check all short opt forms with required value
    testResult(["-f", "test"], ["file"->["test"]], []);
    testResult(["-f="], ["file"->[""]], []);
    testResult(["-f=test"], ["file"->["test"]], []);
    // Check all long opt forms with required value
    testResult(["--file", "test"], ["file"->["test"]], []);
    testResult(["--file="], ["file"->[""]], []);
    testResult(["--file=test"], ["file"->["test"]], []);
    // Check short flag forms
    testResult(["-f=test", "-d"], ["file"->["test"], "debug"->["true"]], []);
    testResult(["-f=test", "-d", "test"], ["file"->["test"], "debug"->["true"]], ["test"]);
    // Check long flag forms
    testResult(["-f=test", "--debug"], ["file"->["test"], "debug"->["true"]], []);
    testResult(["-f=test", "--debug", "test"], ["file"->["test"], "debug"->["true"]], ["test"]);
    // Check all short opt forms with optional value
    testResult(["-f=test", "-v"], ["file"->["test"], "logging"->["all"]], []);
    testResult(["-f=test", "-v="], ["file"->["test"], "logging"->[""]], []);
    testResult(["-f=test", "-v=foo"], ["file"->["test"], "logging"->["foo"]], []);
    testResult(["-f=test", "-v", "foo"], ["file"->["test"], "logging"->["all"]], ["foo"]);
    // Check all long opt forms with optional value
    testResult(["-f=test", "--verbose"], ["file"->["test"], "logging"->["all"]], []);
    testResult(["-f=test", "--verbose="], ["file"->["test"], "logging"->[""]], []);
    testResult(["-f=test", "--verbose=foo"], ["file"->["test"], "logging"->["foo"]], []);
    testResult(["-f=test", "--verbose", "foo"], ["file"->["test"], "logging"->["all"]], ["foo"]);
    // All kinds of combinations
    testResult(["-f", "test", "noot"], ["file"->["test"]], ["noot"]);
    testResult(["--file", "test", "noot"], ["file"->["test"]], ["noot"]);
    testError(["--file=test", "-f"], ["Missing value for option -f"]);
    testResult(["--file=test", "-f", "test2"], ["file"->["test", "test2"]], []);
    testError(["--file=test", "--file"], ["Missing value for option --file"]);
    testResult(["--file=test", "--file="], ["file"->["test", ""]], []);
    testResult(["--file=test", "--file=test2"], ["file"->["test", "test2"]], []);
    testError(["--file=test", "--file=test2", "-o"], ["Missing value for option -o"]);
    testResult(["--file=test", "--file=test2", "--out="], ["file"->["test", "test2"], "out"->[""]], []);
    testResult(["--file=test", "--file=test2", "--out=test3"], ["file"->["test", "test2"], "out"->["test3"]], []);
    testError(["--file=test", "--file=test2", "--out=test3", "--out=test4"], ["Multiple values not allowed for option -o or --out"]);
    testResult(["--file=test", "--file=test2", "--out=test3", "mies"], ["file"->["test", "test2"], "out"->["test3"]], ["mies"]);
    // Check cases (short opts are case sensitive, long opts are not)
    testError(["-F", "test"], ["Unknown option -F"]);
    testResult(["--FILe", "test"], ["file"->["test"]], []);
    // Check completely unknown option
    testError(["-x"], ["Unknown option -x"]);
    // Check that we don't parse anymore after the first non-opt argument
    testResult(["-f", "test", "noot", "-f", "test", "-x"], ["file"->["test"]], ["noot", "-f", "test", "-x"]);
}