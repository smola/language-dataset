import os/Terminal
import structs/LinkedList


Reporter: abstract class {
    instance: static This = ConsoleReporter new() as This

    testStarted: abstract func(name: String)
    testCompleted: abstract func

    assertFailed: abstract func(errorMsg: String)

    suiteFinished: abstract func 
}

// Register an at exit callback to invoke the Reporter suiteFinished()
atexit: extern func(Func) -> Int
atexit(func { Reporter instance suiteFinished() })

ConsoleReporter: class extends Reporter {
    passes, failures: Int
    failedAsserts: LinkedList<String>
    currentTestPassed: Bool

    init: func {
        passes = 0; failures = 0
        failedAsserts = LinkedList<String> new()
    }

    testStarted: func(name: String) {
        currentTestPassed = true
        "%s: Running..." format(name) print()
    }

    testCompleted: func {
        "\b" times(10) print()

        if(currentTestPassed) {
            passes += 1

            Terminal setFgColor(Color green)
            "[Passed]  " println()
            Terminal reset()
        }
        else {
            failures += 1

            Terminal setFgColor(Color red)
            "[Failed]  " println()
            Terminal reset()

            // Print out any failed assert messages
            for(msg: String in failedAsserts) {
                "    - %s" format(msg) println()
            }
            failedAsserts clear()
        }
    }

    assertFailed: func(errorMsg: String) {
        currentTestPassed = false
        failedAsserts add(errorMsg)
    }

    suiteFinished: func {
        "\n%d " format(passes) print()
        Terminal setFgColor(Color green)
        "passes " print()
        Terminal reset()
        "%d " format(failures) print()
        Terminal setFgColor(Color red)
        "failures" println()
        Terminal reset()
    }
}
