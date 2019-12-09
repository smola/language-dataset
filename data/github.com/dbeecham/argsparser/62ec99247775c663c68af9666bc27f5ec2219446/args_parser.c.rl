#include <unistd.h>
#include <stdio.h>

#include "args_parser.h"

%%{

    machine args_parser;

    #########
    # UTILS #
    #########

    action debug {
        printf("%c", fc);
    }


    ##########
    # ERRORS #
    ##########

    action no_file_name {
        fprintf(stderr, "no file name\n");
        return -1;
    }

    action not_a_valid_short_command {
        fprintf(stderr, "no such flag: -%c\n", fc);
        return -1;
    }

    action not_a_valid_long_command {
        fprintf(stderr, "no such flag: %s\n", argv[i]);
        return -1;
    }



    ###########
    # ACTIONS #
    ###########

    action verbose {
        opts->verbose = true;
    }

    action test_end {
        if (++i >= argc) {
            fbreak;
        }
    }

    action copy_pid {
        opts->pid[opts->pid_len++] = fc;
    }


    ##########
    # PARSER #
    ########## 

    # Filenames are all printable characters, but they can also contain spaces
    # (which is ascii number 32).
    filename_chrs = (32 .. 126);

    filename = ((32 .. 126) - '-')
               filename_chrs+
               0 @test_end;

    # Printable characters are all "normal" ascii characters, excluding
    # whitespace. It includes A-Z, a-z, 0-9, and all common special characters
    # such as !"'%(){}`.
    printable = (33 .. 126);

    shorts = 'v'@verbose @err(not_a_valid_short_command);

    longs = 'verbose'@verbose @err(not_a_valid_long_command);

    args = ('-' shorts+ | '--' longs)
            0 @test_end;

    pid = [0-9]{1,6} @copy_pid
          0 @test_end;

    main := (args*
             pid) @err{ printf("parse error at %c\n", fc); };

    write data;

}%%

int parse_args(
    const int argc,
    const char * const * const argv,
    struct args_opts_s * opts
)
{
    if (argc == 0) {
        return -1;
    }
    if (argc == 1) {
        return -1;
    }

    int cs;
    
    %% write init;

    int i = 1;
    const char * p = argv[i];
    const char * eof = 0;

    %% write exec noend;

    if (cs < %%{ write first_final; }%%) {
        return -1;
    } else {
        return 0;
    }

}
