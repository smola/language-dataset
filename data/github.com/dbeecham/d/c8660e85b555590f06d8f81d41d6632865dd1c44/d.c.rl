#include <stdio.h>
#include <term.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <curl/curl.h>

#define BOLD_CYAN "\033[1;36m"
#define CYAN "\033[0;36m"
#define BOLD_MAGENTA "\033[1;35m"
#define MAGENTA "\033[0;35m"
#define BOLD_BLUE "\033[1;34m"
#define BLUE "\033[0;34m"
#define BOLD_YELLOW "\033[1;33m"
#define YELLOW "\033[0;33m"
#define BOLD_GREEN "\033[1;32m"
#define GREEN "\033[0;32m"
#define BOLD_RED "\033[1;31m"
#define RED "\033[0;31m"
#define BOLD_BLACK "\033[1;30m"
#define BLACK "\033[0;30m"
#define NORMAL "\033[0m"

#define C_256_ID "\033[38;5;245m"
#define C_256_DASH "\033[38;5;244m"
#define C_256_NAME "\033[38;5;255m"
#define C_256_PORT "\033[38;5;146m"
#define C_256_IMAGE "\033[38;5;86m"

const static char * d_ps_color_id = "";
const static char * d_ps_color_dash = "";
const static char * d_ps_color_name = "";
const static char * d_ps_color_port = "";
const static char * d_ps_color_image = "";

%%{
    machine p_ps;

    action debug {
        write(1, p, 1);
    }

    action init {
        memset(d_ps_s->id, 0, sizeof(d_ps_s->id));
        memset(d_ps_s->image, 0, sizeof(d_ps_s->image));
        memset(d_ps_s->name, 0, sizeof(d_ps_s->name));
        memset(d_ps_s->ports, 0, sizeof(d_ps_s->ports));
        d_ps_s->id_i = 0;
        d_ps_s->image_i = 0;
        d_ps_s->name_i = 0;
        d_ps_s->ports_i = 0;
    }

    action init_port {
        d_ps_s->ports_ip_i = 0;
        d_ps_s->ports_private_port_i = 0;
        d_ps_s->ports_public_port_i = 0;
    }

    action copy_id {
        d_ps_s->id[d_ps_s->id_i++] = fc;
    }

    action copy_image {
        if (d_ps_s->image_i < (sizeof(d_ps_s->image) - 2)) {
            d_ps_s->image[d_ps_s->image_i++] = fc;
        }
    }

    action copy_name {
        if (d_ps_s->name_i < (sizeof(d_ps_s->name) - 2)) {
            d_ps_s->name[d_ps_s->name_i++] = fc;
        }
    }

    action port_type_tcp {
        if (d_ps_s->ports_i < (sizeof(d_ps_s->ports)/sizeof(struct d_ps_port))) {

            d_ps_s->ports[d_ps_s->ports_i].port_type = PORT_TYPE_TCP;

        }
    }

    action port_type_udp {
        if (d_ps_s->ports_i < (sizeof(d_ps_s->ports)/sizeof(struct d_ps_port)) &&
            d_ps_s->ports_ip_i < (sizeof(d_ps_s->ports[d_ps_s->ports_i].ip) - 2)) {

            d_ps_s->ports[d_ps_s->ports_i].port_type = PORT_TYPE_UDP;

        }
    }

    action copy_public_port {
        if (d_ps_s->ports_i < (sizeof(d_ps_s->ports)/sizeof(struct d_ps_port)) &&
            d_ps_s->ports_public_port_i < (sizeof(d_ps_s->ports[d_ps_s->ports_i].public_port) - 2)) {

            d_ps_s->ports[d_ps_s->ports_i].public_port[d_ps_s->ports_public_port_i++] = fc;

        }
    }

    action copy_private_port {
        if (d_ps_s->ports_i < (sizeof(d_ps_s->ports)/sizeof(struct d_ps_port)) &&
            d_ps_s->ports_private_port_i < (sizeof(d_ps_s->ports[d_ps_s->ports_i].private_port) - 2)) {

            d_ps_s->ports[d_ps_s->ports_i].private_port[d_ps_s->ports_private_port_i++] = fc;

        }
    }

    action copy_port_ip {
        if (d_ps_s->ports_i < (sizeof(d_ps_s->ports)/sizeof(struct d_ps_port)) &&
            d_ps_s->ports_ip_i < (sizeof(d_ps_s->ports[d_ps_s->ports_i].ip) - 2)) {

            d_ps_s->ports[d_ps_s->ports_i].ip[d_ps_s->ports_ip_i++] = fc;

        }
    }

    action status_running {
        d_ps_s->status = STATUS_RUNNING;
    }

    action status_restarting {
        d_ps_s->status = STATUS_RESTARTING;
    }

    action print_container {
        printf("%s%.6s %s-%s %.*s %s(%.*s)" NORMAL,
                d_ps_color_id,
                d_ps_s->id,
                d_ps_color_dash,
                d_ps_color_name,
                (int)sizeof(d_ps_s->name),
                d_ps_s->name,
                d_ps_color_image,
                (int)sizeof(d_ps_s->image),
                d_ps_s->image
        );

        if (d_ps_s->ports_i > 0) {

            for (int i = 0; i < d_ps_s->ports_i;) {
                printf("\n%s  %.*s:%.*s -> %.*s" NORMAL, 
                        d_ps_color_port,
                        (int)sizeof(d_ps_s->ports[i].ip),
                        d_ps_s->ports[i].ip,
                        (int)sizeof(d_ps_s->ports[i].public_port),
                        d_ps_s->ports[i].public_port,
                        (int)sizeof(d_ps_s->ports[i].private_port),
                        d_ps_s->ports[i].private_port
                );

                if (++i < d_ps_s->ports_i) {
                } else {
                    break;
                }
            }

        }
        printf("\n");
    }

    action ports_inc {
        d_ps_s->ports_i++;
    }

    access d_ps_s->;

    time_specifier = ('days' | 'hours' | 'weeks');

    mount_name = '"Name":"' [A-Za-z0-9]* '"';
    mount_propagation = '"Propagation":' ('""' | '"rprivate"');
    mount_rw = '"RW":' ('true' | 'false');
    mount_driver = '"Driver":"' [A-Za-z0-9]+ '"';
    mount_mode = '"Mode":' ('""' | '"ro"');
    mount_destination = '"Destination":"' [A-Za-z0-9._\- /]+ '"';
    mount_source = '"Source":"' [A-Za-z0-9._\- /]* '"';
    mount_type = '"Type":' ('"bind"' | '"volume"');
    mount_attributes = mount_type (',' mount_name)? ',' mount_source ',' mount_destination (',' mount_driver)? ',' mount_mode ',' mount_rw ',' mount_propagation;
    mount = '{' mount_attributes '}';
    mounts = '"Mounts":[' (mount (',' mount)*)? ']';
    networksettings_networks_network = '"' [A-Za-z0-9_]+ '":{' (any -- '}')* '}';
    networksettings_networks = '"Networks":{' networksettings_networks_network? '}';
    networksettings = '"NetworkSettings":{' networksettings_networks '}';
    hostconfig = '"HostConfig":{' (any -- '}')* '}';
    status_status_up = '"Up ' digit+ ' ' time_specifier '"';
    status_status_restarting = '"Restarting (' digit+ ') ' digit+ ' ' time_specifier ' ago"';
    status_status = (status_status_up | status_status_restarting);
    status = '"Status":' status_status;
    state_state = ('"running"' @status_running | '"restarting"' @status_restarting);
    state = '"State":' state_state;

    label = '"'
            [A-Za-z0-9.\-]+
            '":"'
            [A-Za-z0-9.]+
            '"';

    labels = '"Labels":{' (label (',' label)*)? '}';
    port_type = '"Type":"' ('tcp' @port_type_tcp | 'udp' @port_type_udp) '"';
    port_publicport = '"PublicPort":' [0-9]+ $copy_public_port;
    port_privateport = '"PrivatePort":' [0-9]+ $copy_private_port;
    port_ip = '"IP":"' [0-9.]+ $copy_port_ip '"';
    port = '{' @init_port
            (port_ip ',')?
            port_privateport ',' 
            (port_publicport ',')?
            port_type
            '}';
    ports = '"Ports":[' (port@ports_inc (',' port@ports_inc)*)? ']';
    created = '"Created":' digit+;
    command = '"Command":"' [A-Za-z0-9./_\-: \'=]+ '"';
    imageid = '"ImageID":"' [A-Za-z0-9./_\-:]+ '"';
    image = '"Image":"'
            [A-Za-z0-9./_\-:]+ $copy_image
            '"';
    id = '"Id":"'
         alnum{64} $copy_id
         '"';

    name = '"' [A-Za-z0-9/_\-]+ $copy_name '"';
    names = '"Names":[' (name (',' $copy_name name)*)? ']';


    attributes = id ','
                 names ','
                 image ','
                 imageid ','
                 command ','
                 created ','
                 ports ','
                 labels ','
                 state ','
                 status ','
                 hostconfig ','
                 networksettings ','
                 mounts;

    container = '{' @init 
                attributes
                '}' @print_container;


    main := ('[' (container (',' container)*)? ']' [\r\n]* ) $err{ printf("\nerror parsing: 0x%02x, in state %i\n", fc, fcurs); };


}%%

enum STATUS {
    STATUS_UNKNOWN = 0,
    STATUS_RUNNING,
    STATUS_RESTARTING
};

enum PORT_TYPE {
    PORT_TYPE_UNKNOWN = 0,
    PORT_TYPE_TCP,
    PORT_TYPE_UDP
};


struct d_ps_port {
    char ip[17];
    char public_port[8];
    char private_port[8];
    enum PORT_TYPE port_type;
};

struct d_ps_s {
    int cs;
    char id[65];
    char image[65];
    char name[65];
    struct d_ps_port ports[8];
    uint_fast8_t ports_public_port_i;
    uint_fast8_t ports_private_port_i;
    uint_fast8_t ports_ip_i;
    uint_fast8_t ports_i;
    uint_fast8_t id_i;
    uint_fast8_t image_i;
    uint_fast8_t name_i;
    enum STATUS status;
};


static size_t docker_ps_wf (
 char* ptr,
 size_t size,
 size_t nmemb,
 void* userdata
) {
    struct d_ps_s * d_ps_s = userdata;
    char * p = ptr;
    char * pe = ptr + size * nmemb;
    char * eof = NULL;
    %% write init nocs;
    %% write exec;
    return size*nmemb;
}


void docker_ps(void) {
    if (max_colors == 8) {
        d_ps_color_id = BLUE;
        d_ps_color_dash = NORMAL;
        d_ps_color_name = NORMAL;
        d_ps_color_port = CYAN;
        d_ps_color_image = BLUE;
    }
    else if (max_colors == 256) {
        d_ps_color_id = C_256_ID;
        d_ps_color_dash = C_256_DASH;
        d_ps_color_name = C_256_NAME;
        d_ps_color_port = C_256_PORT;
        d_ps_color_image = C_256_IMAGE;
    }


    struct d_ps_s d_ps_s = {0};
    d_ps_s.cs = %%{ write start; }%%;


    CURL * curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_UNIX_SOCKET_PATH, "/var/run/docker.sock");
    curl_easy_setopt(curl, CURLOPT_URL, "http://localhost/containers/json");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, docker_ps_wf);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &d_ps_s);
    curl_easy_perform(curl);

    //docker_ps_wf(str, 1, strlen(str), &d_ps_s);
}


int main (
 int argc, 
 char *argv[]
) {

    %%{

        machine d;

        action abc {
            printf("hi\n");
        }

        action debug {
            write(1, p, 1);
        }

        action docker_ps {
            docker_ps();
        }

        main := ( 'ps'@docker_ps | 'images' | 'bar' );
        write data;
    }%%

    // Initialize terminfo
    char * term = getenv("TERM");
    if (0 == strncmp(term, "", 1)) {
        term = "linux";
    }
    setupterm(term, 0, NULL);

    char * p, * pe;
    int cs = 0;
    %% write init;
    for (int i = 1; i < argc; i++) {
        p = argv[i];
        pe = p + strlen(argv[i]);
        %% write exec;
    }
    return 0;
}
