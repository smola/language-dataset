CREATE EXTERNAL TABLE tweets_avro_table ( 
	id                  	string,              	   
	user_friends_count  	int   ,              	
	user_location       	string,              	
	user_description    	string,              	
	user_statuses_count 	int   ,              	
	user_followers_count	int   ,              	
	user_name           	string,              	
	user_screen_name    	string,              	
	created_at          	string,              	
	text                	string,              	
	retweet_count       	bigint,              	
	retweeted           	boolean,             	
	in_reply_to_user_id 	bigint,              	
	source              	string,              	
	in_reply_to_status_id	bigint,              	
	media_url_https     	string,              	
	expanded_url        	string 
	)
STORED AS AVRO
LOCATION '/user/externaltables/';
 
INSERT OVERWRITE TABLE tweets_avro_table SELECT * FROM tweets LIMIT 2500;
