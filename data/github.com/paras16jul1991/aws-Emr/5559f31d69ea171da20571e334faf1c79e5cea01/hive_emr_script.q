create table temp_drivers2 (col_value STRING);

LOAD DATA INPATH '${INPUT}/drivers.csv' OVERWRITE INTO TABLE temp_drivers2;

CREATE TABLE drivers2 (driverId INT, name STRING, ssn BIGINT, location STRING, certified STRING, wageplan STRING);

insert overwrite table drivers2
SELECT
  regexp_extract(col_value, '^(?:([^,]*),?){1}', 1) driverId,
  regexp_extract(col_value, '^(?:([^,]*),?){2}', 1) name,
  regexp_extract(col_value, '^(?:([^,]*),?){3}', 1) ssn,
  regexp_extract(col_value, '^(?:([^,]*),?){4}', 1) location,
  regexp_extract(col_value, '^(?:([^,]*),?){5}', 1) certified,
  regexp_extract(col_value, '^(?:([^,]*),?){6}', 1) wageplan

from temp_drivers2;


CREATE TABLE temp_timesheet2 (col_value string);

LOAD DATA INPATH '${INPUT}/timesheet.csv' OVERWRITE INTO TABLE temp_timesheet2;

CREATE TABLE timesheet2 (driverId INT, week INT, hours_logged INT , miles_logged INT);

insert overwrite table timesheet2
SELECT
  regexp_extract(col_value, '^(?:([^,]*),?){1}', 1) driverId,
  regexp_extract(col_value, '^(?:([^,]*),?){2}', 1) week,
  regexp_extract(col_value, '^(?:([^,]*),?){3}', 1) hours_logged,
  regexp_extract(col_value, '^(?:([^,]*),?){4}', 1) miles_logged

from temp_timesheet2;

INSERT OVERWRITE DIRECTORY '${OUTPUT}/result/'
SELECT d.driverId, d.name, t.total_hours, t.total_miles from drivers2 d
JOIN (SELECT driverId, sum(hours_logged)total_hours, sum(miles_logged)total_miles FROM timesheet2 GROUP BY driverId ) t
ON (d.driverId = t.driverId);