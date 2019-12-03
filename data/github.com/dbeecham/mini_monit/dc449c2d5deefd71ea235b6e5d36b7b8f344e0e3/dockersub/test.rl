#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <curl/curl.h>
#include <err.h>


%%{
    machine docker_json;

    action debug {
        write(1, p, 1);
    }

    action ddebug {
        write(1, ">", 1);
        write(1, p, 1);
    }

    action hello {
        write(1, p, 1);
    }

    action set_id {
        if (127 > id_i) {
            id[id_i++] = *p;
        }
    }

    action set_name {
        if (127 > name_j && name_i >= 0) {
            names[name_i][name_j] = *p;
        }
        name_j++;
    }

    action next_name {
        if (3 > name_i) {
            name_i++;
        } else {
            name_i = -1;
        }
        name_j = 0;
    }

    action print_container {
        printf("container with id: %.*s\n", (int)id_i, id);
        for (int n = 0; n <= name_i; n++) {
            printf("name: %s\n", names[n]);
        }
        memset(names, 0, 128*4);
        memset(id, 0, 128);
        name_i = 0;
        name_j = 0;
        id_i = 0;
    }

    id = '"Id":"' alnum+ @set_id '"';

    name = '"' [/A-Za-z_\-0-9]+@set_name '"'@next_name;

    names = '"Names":[' name (',' name){0,3} ']';

    json_string = '"' [A-Za-z0-9/.,_\-0-9: ]* '"';

    json_object := (
                        [A-Za-z"/.,_\-0-9: ] | 
                        '[' @{fcall json_array;} |
                        '{' @{fcall json_object;}
                   )* 
                   '}' @{fret;}
                   ;

    json_array := (
                    [A-Za-z"/.,_\-0-9: ] |
                    '[' @{fcall json_array;} |
                    '{'@{fcall json_object;}
                  )*
                  ']' @{fret;}
                  ;

    json_number = [0-9]+;

    other = '"' (([A-Za-z/.,_\-0-9: ]+) -- 'Id' -- 'Names') '":' 
            (
                json_string | 
                json_number |
                '{' @{fcall json_object;} |
                '[' @{fcall json_array;}
            )
            ;

    cproperty = id | names | other; 

    container = '{' (cproperty (',' cproperty)*)? '}' %from(print_container);

    main := '[' container (',' container)* ']';

    write data;
}%%

size_t write_data(void *buffer, size_t size, size_t nmemb, void *userp)
{
    char * p = buffer;
    char * pe = buffer + size * nmemb;
    char id[128] = {0};
    char names[4][128];
    char name_i = 0, name_j = 0;
    int stack[8];
    int top;
    int cs;
    memset(names, 0, 128 * 4);

    uint_fast16_t id_i = 0;
    %% write init;
    %% write exec;

    return size * nmemb;
}

int main(int args, char * argv[])
{

    CURL *curl = curl_easy_init();
    if (NULL == curl) {
        err(1, "curl_easy_init");
    }

    curl_easy_setopt(curl, CURLOPT_UNIX_SOCKET_PATH, "/var/run/docker.sock");
    curl_easy_setopt(curl, CURLOPT_URL, "http://localhost/containers/json");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    int ret = curl_easy_perform(curl);
    if (CURLE_OK != ret) {
        err(1, "curl_easy_perform");
    }

}
