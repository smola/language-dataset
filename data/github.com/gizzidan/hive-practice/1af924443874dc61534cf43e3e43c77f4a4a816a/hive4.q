DROP TABLE IF EXISTS batting;
CREATE EXTERNAL TABLE IF NOT EXISTS batting(id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

DROP TABLE IF EXISTS master;
CREATE EXTERNAL TABLE IF NOT EXISTS master(id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, bcity STRING, dyear INT, dmonth INT, 
    dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, name STRING, weight INT, height INT, bats STRING, 
    throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master';

DROP VIEW IF EXISTS specs;
CREATE VIEW IF NOT EXISTS specs AS SELECT id, bmonth, dyear, bats FROM master WHERE bmonth=10 AND dyear=2011 AND bats="R";

DROP VIEW IF EXISTS final;
CREATE VIEW final AS SELECT b.id, sum(b.hits) AS MaxHits FROM batting b JOIN specs m ON (m.id=b.id) GROUP BY b.id ORDER BY MaxHits DESC LIMIT 1;

SELECT id FROM final;