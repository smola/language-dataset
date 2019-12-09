#include <string>
#include <stdexcept>
#include "VprPlacementParser.hpp"

%%{
    machine VprPlacementParser;

    action start_block_name { ts = fpc; }
    action end_block_name { current_block.name = std::string(ts, fpc - ts); }
    action start_x { ts = fpc; }
    action end_x {
        temp_str = std::string(ts, fpc - ts);
        current_block.x = atoi(temp_str.c_str());
    }
    action start_y { ts = fpc; }
    action end_y {
        temp_str = std::string(ts, fpc - ts);
        current_block.y = atoi(temp_str.c_str());
    }
    action start_subblock { ts = fpc; }
    action end_subblock {
        temp_str = std::string(ts, fpc - ts);
        current_block.subblock = atoi(temp_str.c_str());
    }
    action start_block_number {
        ts = fpc;
    }
    action end_block_number {
        temp_str = std::string(ts, fpc - ts);
        current_block.number = atoi(temp_str.c_str());
    }
    action end_block {
        blocks.push_back(current_block);
    }
    action end_comment {}
    action end_emptyline {}
    action end_firstline {}
    action end_arraysizeline {}
    action start_x_dim { ts = fpc; }
    action start_y_dim { ts = fpc; }
    action start_netlist { ts = fpc; }
    action start_architecture { ts = fpc; }
    action end_x_dim {
        temp_str = std::string(ts, fpc - ts);
        x_dim = atoi(temp_str.c_str());
    }
    action end_y_dim {
        temp_str = std::string(ts, fpc - ts);
        y_dim = atoi(temp_str.c_str());
    }
    action end_netlist { netlist_file = std::string(ts, fpc - ts); }
    action end_architecture { architecture_file = std::string(ts, fpc - ts); }

    # Words in a line.
    word = ^[ \t\n]+;

    # The whitespace separating words in a line.
    whitespace = [ \t];

    block_name = word >start_block_name %end_block_name; 
    x = digit+ >start_x %end_x;
    y = digit+ >start_y %end_y;
    subblock = digit+ >start_subblock %end_subblock;
    block_number = digit+ >start_block_number %end_block_number;
    x_dim = digit+ >start_x_dim %end_x_dim;
    y_dim = digit+ >start_y_dim %end_y_dim;
    netlist = word >start_netlist %end_netlist; 
    architecture = word >start_architecture %end_architecture;

    separator = ('\\\n' | whitespace);

    comment = ( '#' (whitespace* word)** ) %end_comment;
    endofline = ( comment? whitespace* '\n' );
    emptyline = whitespace* endofline %end_emptyline;
    firstline = ('Netlist file:' whitespace+ netlist whitespace+ 'Architecture file:' whitespace+ architecture endofline) %end_firstline;
    arraysizeline = ('Array size:' whitespace+ x_dim whitespace+ 'x' whitespace+ y_dim whitespace+ 'logic blocks' endofline) %end_arraysizeline;

    block = (block_name whitespace+ x whitespace+ y
             whitespace+ subblock whitespace+ '#' block_number '\n') %end_block;

    # Any number of lines.
    main := (emptyline | firstline | arraysizeline | comment | block)+;
}%%


/* Regal data ****************************************/
%% write data nofinal;
/* Regal data: end ***********************************/

void VprPlacementParser::init() {
    x_dim = 0;
    y_dim = 0;
    buf = &buf_vector[0];
    _BUFSIZE = buf_vector.size() - 1;

    %% write init;
}


void VprPlacementParser::ragel_parse(std::istream &in_stream) {
    bool done = false;
    bool final_pass = false;
    int i = 0;
    have = 0;
    while (!done) {
        /* How much space is in the buffer? */
        int space = _BUFSIZE - have;
        if (space == 0) {
            /* Buffer is full. */
            cerr << "TOKEN TOO BIG" << endl;
            exit(1);
        }
        /* Read in a block after any data we already have. */
        char *p = buf + have;
        in_stream.read(p, space);
        int len = in_stream.gcount();
        char *pe = p + len;
        char *eof = 0;

        if (len == 0) {
            if (!final_pass) {
                /* After last character has been read from stream, insert two
                 * additional new-line characters to make sure the most
                 * recently parsed line is parsed to completion.  This prevents
                 * missing the last block line when there is no blank line at
                 * the end of the input stream. */
                len = 2;
                *p = '\n';
                *(p + 1) = '\n';
                pe = p + len;
                final_pass = true;
            } else {
                /* We've reached the end of the input stream, so indicate EOF. */
                eof = pe;
                done = true;
            }
        }
        if (!done) {
            %% write exec;

            if ( cs == VprPlacementParser_error ) {
                /* Machine failed before finding a token. */
                cerr << "PARSE ERROR" << endl;
                /*exit(1);*/
                throw std::runtime_error("PARSE ERROR");
            }
            if ( ts == 0 ) {
                have = 0;
            } else {
                /* There is a prefix to preserve, shift it over. */
                have = pe - ts;
                memmove(buf, ts, have);
                ts = buf;
            }
        }
    }
}
