-- select `time`, `user`, `page` from wikiticker limit 10;
CREATE EXTERNAL TABLE wikiticker(
  `time` string,
  `channel` string,
  `cityName` string,
  `comment` string,
  `countryIsoCode` string,
  `countryName` string,
  `isAnonymous` boolean,
  `isMinor` boolean,
  `isNew` boolean,
  `isRobot` boolean,
  `isUnpatrolled` boolean,
  `metroCode` string,
  `namespace` string,
  `page` string,
  `regionIsoCode` string,
  `regionName` string,
  `user` string,
  `delta` bigint,
  `added` bigint,
  `deleted` bigint
)
ROW FORMAT SERDE
'org.apache.hive.hcatalog.data.JsonSerDe'
STORED AS TEXTFILE
LOCATION
  '/hive-data/warehouse/wikiticker'
;
