#!/usr/bin/pike

//this file should very rarely ever need to be updated, so we don't need to worry about exitting it upon update

import redis_client;

string db_prefix = "greenit:";
int db_index = 2;
string scriptrunner_location = "/usr/local/bin/ScriptRunner_SCGI.pike";
Redis db;

int main(int argc, array(string) argv)
{
	if(Process.create_process(({scriptrunner_location})))
	{
		write("Started scriptrunner script");
		
		handle_events();
		
	}
	else
		write("Unable to start the scriptrunner script. Make sure it exists at the location chosen AND has execution enabled\n");
	
	
	return 0;
}

string key_filter(string key)
{
	return replace(replace(key, "\r", ""), "\n", "");
}

//TODO make a sorting function
//TODO BE SURE TO MAKE CLOBAL POPULAR AND CONTROVERSIAL SORTERS ASWELL AS DAILY SORTERS

void handle_events() //TODO also handle dangling connections to redis from 404 pages or other unimportant ones
{
	int connected = 0;
	
	
	do
	{
		delay(1);
		write("connecting to redis db\n");
		db = Redis(0, 0, "/var/run/redis/redis.sock");
		db->connect();
		
		if(db->ping() && db->select(db_index))
		{
			connected++;
			write("redis db conection successful\n");
		}
		else
			write("unable to connecto to redis db\n");
	} while(!connected);
	
	
	while(1) //will run forever so...
	{
		//handles ranking posts in the viewing menu types. Would make a lua script for this, but if the db gets large enough, that could be slow
		array(string) subgreenits = db->lrange(db_prefix + "variable:subgreenits", 0, -1);
		array(string) posts;
		
		
		foreach(subgreenits, string sg_key)
		{
			//string popular_key = 
			array(string) key_parts = key_filter(sg_key) / ":";
			posts = db->lrange(db_prefix + "variable:" + key_parts[2] + ":posts", 0, -1);
			
			/*
			foreach(posts, string post_key)
			{
				//sort each post in the popular/controversial lists
				//write("heyyyy %s\n", key_filter(post_key));
			}
			*/
			
			//SORT HERE
			
			
			
			
			
			delay(1); //more delay so it doesnt get bogged down
		}
		
		//delay a little more
		delay(10);
	}
	
}