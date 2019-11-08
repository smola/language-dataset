#!/usr/bin/env lua

require 'Test.More'
local scope = require "scope"

is(type(scope), 'function')

subtest("no exceptions, no on_exit",
        function()
           local success = false
           scope(
              function(guard)
                 success = true
              end
           )
           ok(success)
        end
)

subtest("no exceptions, on_exit is invoked in correct order",
        function()
           local success = 1
           scope(
              function(guard)
                 ok(guard)
                 guard:on_exit(function() success = (success+1) * 2 end)
                 guard:on_exit(function() success = (success+1) * 3 end)
                 success = 2
              end
           )
           is(success, 7*3)
        end
)

subtest("exception, on_exit is invoked in correct order",
        function()
           local success, msg = pcall(
              function()
                 local success = 1
                 scope(
                    function(guard)
                       guard:on_exit(function() success = (success+1) * 2 end)
                       guard:on_exit(function() success = (success+1) * 3 end)
                       error("some exception")
                       success = 2
                    end
                 )
                 is(success, 5*3)
              end
           )
           ok(not success);
           like(msg, "some exception");
        end
)

subtest("exception, on_exit is invoked in correct order",
        function()
           local success, msg = pcall(
              function()
                 scope(
                    function(guard)
                       guard:on_exit("x")
                    end
                 )
                 is(success, 5*3)
              end
           )
           ok(not success);
           like(msg, "argument should be a function");
        end
)

done_testing()
