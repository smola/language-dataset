DROP TABLE IF EXISTS master;
CREATE EXTERNAL TABLE IF NOT EXISTS master(id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master';


select rankdobid
from (select mstr.dobid as rankdobid, DENSE_RANK() OVER(ORDER BY mstr.dobcount desc) as rankval 
	  from
		(select master.weight as dobid, count(master.id) as dobcount 
 		from master
 		where master.weight is not null
 	  group by master.weight ) mstr 
 ) rankmaster
 where rankval = 2;