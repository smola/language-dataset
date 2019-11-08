module: postgresql-test-suite
synopsis: Test suite for postgresql connection management.

define test establish-connection-test ()
  let conn = pg-connect("");
  assert-equal($CONNECTION-OK, pg-connection-status(conn));
  assert-no-errors(pg-finish(conn));
end test establish-connection-test;

define test establish-async-connection-test ()
  let conn = pg-connect-async("");
  assert-not-equal($CONNECTION-BAD, pg-connection-status(conn));
  assert-no-errors(
    begin
      let i = 0;
      while ((i < 100) & (pg-connect-poll(conn) ~= $PGRES-POLLING-OK))
        i := i + 1;
        sleep(0.01);
      end while;
    end);
  assert-equal($CONNECTION-OK, pg-connection-status(conn));
  assert-no-errors(pg-finish(conn));
end test establish-async-connection-test;

define test reset-connection-test ()
  let conn = pg-connect("");
  assert-equal($CONNECTION-OK, pg-connection-status(conn));
  assert-no-errors(pg-reset(conn));
  assert-no-errors(pg-finish(conn));
end test reset-connection-test;

define test reset-async-connection-test ()
  let conn = pg-connect("");
  assert-equal($CONNECTION-OK, pg-connection-status(conn));
  assert-equal(1, pg-reset-async(conn));
  assert-not-equal($CONNECTION-BAD, pg-connection-status(conn));
  assert-no-errors(
    begin
      let i = 0;
      while ((i < 100) & (pg-reset-poll(conn) ~= $PGRES-POLLING-OK))
        i := i + 1;
        sleep(0.01);
      end while;
    end);
  assert-equal($CONNECTION-OK, pg-connection-status(conn));
  assert-no-errors(pg-finish(conn));
end test reset-async-connection-test;

define test ping-test ()
  assert-equal($PQPING-OK, pg-ping(""));
  assert-equal($PQPING-NO-RESPONSE, pg-ping("port=1234"));
  assert-equal($PQPING-NO-ATTEMPT, pg-ping("host=does-not-exist"));
end test ping-test;

define test socket-test ()
  let conn = pg-connect("");
  assert-true(pg-socket(conn) > 2);
end test socket-test;

define suite connection-test-suite ()
  test establish-connection-test;
  test establish-async-connection-test;
  test reset-connection-test;
  test reset-async-connection-test;
  test ping-test;
  test socket-test;
end suite;
