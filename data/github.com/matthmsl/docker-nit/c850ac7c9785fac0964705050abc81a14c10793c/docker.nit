# Matthieu S. LE GUELLAUT (@matthmsl)
#
#
# EXIA/UQÀM 2018 - INF7845
#
# This program uses unlicensed opensource libdocker program from Daniel Suo (@danielsuo)
#

module docker is pkgconfig "libcurl"

intrude import core::file
import core
import json::dynamic

# Import C headers
in "C Header" `{
	// Constants

        #define DOCKER_DOCKER_H
        #define DOCKER_API_VERSION v1.25

        // Imports

        #include <stdlib.h>
        #include <string.h>
        #include <curl/curl.h>

        // Structures

        struct buffer {
          char *data;
          size_t size;
        };

        struct docker {
          CURL *curl;
          char *version;
          struct buffer *buffer;
        };

        typedef struct docker DOCKER;

        // Function signatures

        DOCKER *docker_init(char *version);
        int docker_destroy(DOCKER *docker_client);
        char *docker_buffer(DOCKER *docker_client);
        CURLcode docker_post(DOCKER *docker_client, char *url, char *data);
        CURLcode docker_get(DOCKER *docker_client, char *url);
	void malloc_fail();
`}

# Import C-only functions (functions that won't be called from nit)

in "C Body" `{

	static DOCKER *client;

	static size_t write_function(void *data, size_t size, size_t nmemb, void *buffer) {
		size_t realsize = size * nmemb;
		struct buffer *mem = (struct buffer *)buffer;

		mem->data = realloc(mem->data, mem->size + realsize + 1);
		if(mem->data == NULL) {
		malloc_fail();
		}

		memcpy(&(mem->data[mem->size]), data, realsize);
		mem->size += realsize;
		mem->data[mem->size] = 0;

		return realsize;
	}

	static void curl_init(){
		curl_easy_setopt(client->curl, CURLOPT_UNIX_SOCKET_PATH, "/var/run/docker.sock");
		curl_easy_setopt(client->curl, CURLOPT_WRITEFUNCTION, write_function);
		curl_easy_setopt(client->curl, CURLOPT_WRITEDATA, client->buffer);
	}

	void malloc_fail() {
  		fprintf(stderr, "ERROR: Failed to allocate memory.");
  		exit(-1);
	}

	void init_buffer() {
		client->buffer->data = (char *) malloc(1);
		client->buffer->size = 0;
	}

	CURLcode perform(char *url) {
		init_buffer();
		curl_easy_setopt(client->curl, CURLOPT_URL, url);
		CURLcode response = curl_easy_perform(client->curl);
		curl_easy_reset(client->curl);
		return response;
	}

`}

# Handle C structures for nitlang operations

extern class Response `{ CURLcode`}
	redef fun to_s import CString.to_s `{
		return CString_to_s(client->buffer->data);
	`}
end


# Handle C functions

# Implementation adapted from @danielsuo's libdocker
# DockerCLib is the low level implementation of cURL and libdocker
class DockerClient

	var version = "1.37"
	var baseURL = "http:/v{version}/" is lazy

	init
	do
		docker_init("v{version}")
		baseURL="http:/v.{version}/"
	end

	protected fun docker_init(api_version : String) import String.to_cstring`{
		char *version;
		version = String_to_cstring(api_version);
		size_t version_len = strlen(version);
		client = (DOCKER *) malloc(sizeof(struct docker));
		client->buffer = (struct buffer *) malloc(sizeof(struct buffer));
		client->version = (char *) malloc(sizeof(char) * version_len);
		memcpy(client->version, version, version_len);
		client->curl = curl_easy_init();
		if (client->curl) {
	    		curl_init();
	  	}
	`}

	protected fun destroy `{
		curl_easy_cleanup(client->curl);
		free(client->buffer->data);
		free(client->buffer);
		free(client->version);
		free(client);
		client = NULL;
	`}

        protected fun post(url : String, data : String) : Response
        do
                return cpost(baseURL+url,data)
        end

	private fun cpost(url : String, data : String) : Response import String.to_cstring`{
		curl_init();
		struct curl_slist *headers = NULL;
		headers = curl_slist_append(headers,"Content-Type: application/json");
		curl_easy_setopt(client->curl, CURLOPT_HTTPHEADER,headers);
		curl_easy_setopt(client->curl, CURLOPT_POSTFIELDS, (void *)String_to_cstring(data));
		CURLcode response = perform(String_to_cstring(url));
		curl_slist_free_all(headers);
		return response;
	`}

        protected fun get(url:String) : Response
        do
                return self.cget(self.baseURL+url)
        end

	private fun cget(url : String) : Response import String.to_cstring `{
		curl_init();
		return perform(String_to_cstring(url));
	`}


	protected fun delete(url: String) : Response
        do
                return self.cdelete(self.baseURL+url)
        end

	private fun cdelete(url: String) : Response import String.to_cstring `{
		curl_init();
		curl_easy_setopt(client->curl,CURLOPT_CUSTOMREQUEST,"DELETE");
                return perform(String_to_cstring(url));
	`}

end

public class DockerEngine
	super DockerClient

	init
	do
		super
		updateImageList
		updateContainerList
	end

	var containers = new Array[Container] is lazy
	var images = new Array[Image] is lazy


	fun checkConnectivity:Bool do
		var response = self.get("info")
		if response.to_s!="" then
			return true
		else
			return false
		end
	end

	fun createContainer(imageName:String,name:String) : String do
                var data="""
                {
                        "Image":""""+imageName+""""
                }"""
                var response = self.post("containers/create?name={name}",data)
                if response.to_s.has("No such image".to_re) then
                        # DOWNLOAD IMAGE
                        print "Oups ! Downloading missing image..."
                        var secRes=post("images/create?fromImage={imageName}&tag=latest","")
                        if secRes.to_s.has("message".to_re) then
                                print "Operation failed : {secRes.to_s}"
				return ""
                        else
                                updateImageList
                                return createContainer(imageName,name)
                        end
                else
                        updateContainerList
			return response.to_s.to_json_value["Id"].to_s
                end
	end

	fun destroyContainer(container:Container) do
                var response = self.delete("containers/{container.id}")
                print response.to_s
                updateContainerList
	end

        fun destroyImage(image:Image)do
                var response = self.delete("images/{image.id}")
                print response.to_s
                updateImageList
        end

	fun pruneContainers do
                var response = self.post("containers/prune","")
                print response.to_s
                updateContainerList
	end

	fun pruneImage do
                var response = self.post("images/prune","""
                        {
                                "dangling"=false
                        }
                        """
                )
                print response.to_s
                updateImageList
	end

	private fun updateImageList do
                self.images=new Array[Image]
                var response = self.get("images/json")
                var json = response.to_s.to_json_value
                for x in json do
                        var image = new Image(x["RepoTags"].to_s,x["Id"].to_s)
                        self.images.add(image)
                end
	end

	private fun updateContainerList do
                self.containers=new Array[Container]
                var response = self.get("containers/json?all=true")
                var json = response.to_s.to_json_value
                for x in json do
                        var container = new Container(new Image(x["Image"].to_s,x["ImageID"].to_s),x["Names"].to_s,x["Id"].to_s)
                        self.containers.add(container)
                end
	end
end

public class Container
	super DockerClient

        # Attributes
        serialize

        var image : Image
	var name : String is serialize_as("Name")
	var id : String is serialize_as("Id")

	fun start do
                var response = self.post("containers/{id}/start","")
                print response.to_s
	end

	fun stop do
                var response = self.post("containers/{id}/stop","")
                print response.to_s
	end

        fun restart do
                var response = self.post("containers/{id}/restart","")
                print response.to_s
	end

	fun kill do
                var response = self.post("containers/{id}/kill","")
                print response.to_s
	end

	fun rename(name:String)	: Bool do
                var response = self.post("containers/{id}/rename?name={name}","")
                print response.to_s
                if response.to_s=="" then
                        self.name = name
			return true
		else
			return false
                end
	end

	fun pause do
                var response = self.post("containers/{id}/pause","")
                print response.to_s
	end

	fun resume do
                var response = self.post("containers/{id}/unpause","")
                print response.to_s
	end

	fun getStatus : Response do
                var response = self.get("containers/{id}/json")
                print response.to_s
                return response
	end
end

public class Image

	super DockerClient

        # Redefinitions for serializations

        serialize

	var name:String is serialize_as("Image")
        var id:String is serialize_as("Id")

	fun getInfo : Response do
		var response=get("image/{name}/history")
		print response.to_s
		return response
	end
end
