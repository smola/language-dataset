% first implementation of a runner object to run all registered test suites
%
:- object(test_runner).

    % we use the structured printing mechanism in order to allow unit tests
    % results to be intercepted for alternative reporting by e.g. GUI IDEs
    :- uses(logtalk, [print_message/3]).
    :- uses(os, [cpu_time/1]).
    :- uses(list, [member/2]).

    :- private(starttime_/1).
    :- dynamic(starttime_/1).

    :- public(starttime/1).
    starttime(Time):-
        starttime_(Time).

    :- private(set_starttime/1).
    set_starttime(Time):-
        retractall(starttime_(_)),
        assertz(starttime_(Time)).

    :- private(testObject_/1).
    :- dynamic(testObject_/1).

    :-public(registered_tests/1).
	registered_tests(TestObject) :-
		testObject_(TestObject).

    :-public(register_tests/1).
	register_tests(TestObject) :-
        not(var(TestObject)),
        %TODO throw exception if false.
        is_test_object(TestObject),
        unregister_tests(TestObject),
		assertz(testObject_(TestObject)).

    :-public(unregister_tests/1).
	unregister_tests(TestObject) :-
		retractall(testObject_(TestObject)).

    :-public(autoregister_tests/0).
    autoregister_tests :-
        forall(is_test_object(TestObject), register_tests(TestObject)).

    %TODO fixe the two levels inheritance limit
    :-public(is_test_object/1).
    is_test_object(TestObject) :-
        extends_object(TestObject, lgtunit).
    is_test_object(TestObject) :-
        extends_object(TestObject, Parent),
        extends_object(Parent, lgtunit).

    :- private(result_/5).
    :- dynamic(result_/5).

    :-public(test_result/5).
	test_result(Total,Skipped,Passed,Failed, EndAtTime) :-
		result_(Total,Skipped,Passed,Failed, EndAtTime).

    :-public(add_test_result/5).
	add_test_result(Total,Skipped,Passed,Failed, EndAtTime) :-
		assertz(result_(Total,Skipped,Passed,Failed, EndAtTime)).

    :-public(run_tests/0).
    %run all registered tests
    run_tests :-
        findall(
            TestObject,
            registered_tests(TestObject),
            TestObjects
        ),
        run_tests_(TestObjects).

    :-public(run_tests/1).
    run_tests(Tests) :-
        (is_list(Tests)
    		->
    		Testlist = Tests
    		;
    		Testlist = [Tests]
    	),
        findall(
            TestObject,
            (member(TestObject, Testlist), is_test_object(TestObject)),
            TestObjects
        ),
        run_tests_(TestObjects).

    run_tests_(TestObjects) :-
        cpu_time(StartTime),
        set_starttime(StartTime),
        print_start_banner,
        forall(member(TestObject, TestObjects), TestObject::run),
        print_end_banner,
        print_tests_report,
        logtalk::retractall(message_hook(_, _, lgtunit, _)).

    :- private(is_list/1).
    is_list(X) :-
        var(X), !,
        fail.
    is_list([]).
    is_list([_|T]) :-
        is_list(T).

    print_tests_report :-
        %maybe find a best alternative to swi specific aggregate_all(sum(_), ..)
        findall(Total, test_result(Total,_,_,_,_),Totals),
        numberlist::sum(Totals, TotalsSum),
        findall(Skipped, test_result(_,Skipped,_,_,_),Skippeds),
        numberlist::sum(Skippeds, SkippedsSum),
        findall(Passed, test_result(_,_,Passed,_,_),Passeds),
        numberlist::sum(Passeds, PassedsSum),
        findall(Failed, test_result(_,_,_,Failed,_),Faileds),
        numberlist::sum(Faileds, FailedsSum),
        %TODO check Sum Total is Skipped + Passed + Failed,
        (FailedsSum>0 -> Type=error ; Type=information),
        starttime(StartTime),
        cpu_time(EndTime),
        Duration=EndTime-StartTime,
		print_message(Type, test_runner, tests_results_summary(TotalsSum, SkippedsSum, PassedsSum, FailedsSum, Duration)).

    print_start_banner :-
        date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		print_message(information, test_runner, tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds)).

    print_end_banner :-
        date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		print_message(information, test_runner, tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds)).


    :- multifile(logtalk::message_hook/4).
    :- dynamic(logtalk::message_hook/4).

    logtalk::message_hook(tests_results_summary(Total,Skipped,Passed,Failed,_), _, lgtunit, _) :-
        cpu_time(EndAtTime),
        test_runner::add_test_result(Total,Skipped,Passed,Failed, EndAtTime).

    logtalk::message_hook(tests_start_date_time(_,_,_,_,_,_),_, lgtunit, _).

    logtalk::message_hook(tests_end_date_time(_,_,_,_,_,_),_, lgtunit, _).

    logtalk::message_hook(running_tests_from_object_file(Object, File),_, lgtunit, _) :-
        print_message(information, test_runner, running_tests_from_object_file(Object, File)).


    :- multifile(logtalk::message_tokens//2).
    :- dynamic(logtalk::message_tokens//2).

    logtalk::message_tokens(tests_start_date_time(Year, Month, Day, Hours, Minutes, Seconds), test_runner) -->
        [nl, ' TEST RUNNER STARTED - ~w/~w/~w, ~w:~w:~w'-[Year, Month, Day, Hours, Minutes, Seconds], nl,nl].

    logtalk::message_tokens(tests_end_date_time(Year, Month, Day, Hours, Minutes, Seconds), test_runner) -->
		[nl, 'TEST RUNNER ENDED - ~w/~w/~w, ~w:~w:~w'-[Year, Month, Day, Hours, Minutes, Seconds], nl].

    logtalk::message_tokens(tests_results_summary(Total, Skipped, Passed, Failed, Duration), test_runner) -->
		[nl, '~d tests : ~d passed, ~d skipped, ~d failed  (~2f sec)'-[Total, Passed, Skipped, Failed, Duration], nl, nl].

    logtalk::message_tokens(running_tests_from_object_file(Object, File), test_runner) -->
		['[ ~q ] ~w'-[Object, File], nl].

	logtalk::message_tokens(running_tests_from_object(Object), test_runner) -->
		['[ ~q ] '-[Object], nl].

:- end_object.
