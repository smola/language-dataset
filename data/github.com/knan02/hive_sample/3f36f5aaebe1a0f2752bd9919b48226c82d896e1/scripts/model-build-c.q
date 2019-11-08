add file split_user_agent.py;

CREATE EXTERNAL TABLE IF NOT EXISTS raw_logs (
 request_begin_time STRING,
 ad_id STRING,
 impression_id STRING, 
 page STRING,
 user_agent STRING,
 user_cookie STRING,
 ip_address STRING,
 clicked BOOLEAN )
PARTITIONED BY (
 day STRING,
 hour STRING )
STORED AS SEQUENCEFILE
LOCATION '${hiveconf:INPUT}';


MSCK REPAIR TABLE raw_logs;


CREATE EXTERNAL TABLE IF NOT EXISTS feature_index (
 feature STRING,
 ad_id STRING,
 clicked_percent DOUBLE )
STORED AS SEQUENCEFILE
LOCATION '${hiveconf:OUTPUT}';


INSERT OVERWRITE TABLE feature_index
SELECT
 temp.feature,
 temp.ad_id,
 sum(if(temp.clicked = 'true', 1, 0)) / cast(count(1) as DOUBLE) as clicked_percent
FROM (
 SELECT concat('ua:', trim(lower(ua.feature))) as feature, ua.ad_id, ua.clicked
 FROM (
  MAP raw_logs.user_agent, raw_logs.ad_id, raw_logs.clicked
  USING 'split_user_agent.py' as feature, ad_id, clicked
  FROM raw_logs
 ) ua

 UNION ALL

 SELECT concat('ip:', regexp_extract(ip_address, '^([0-9]{1,3}\.[0-9]{1,3}).*', 1)) as feature, ad_id, cast(clicked as STRING) as clicked
 FROM raw_logs

 UNION ALL

 SELECT concat('page:', lower(page)) as feature, ad_id, cast(clicked as STRING) as clicked
 FROM raw_logs
) temp
GROUP BY temp.feature, temp.ad_id;
