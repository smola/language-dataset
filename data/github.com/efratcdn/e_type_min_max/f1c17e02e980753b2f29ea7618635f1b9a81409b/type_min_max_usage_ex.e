type_min_max_usage_ex.e
  
  Usage example of the Type Min / Max

  
  
  Running the demo:
  
  (add this directory to SPECMAN_PATH )
  
  specman -c 'load type_min_max_usage_ex.e; test; show cover monitor.ended; sys.print_types()'


  The packet struct has two fields:
      addr, of type addr_t (int of 8 bits)
      data, of type uint (bits : 8)   
  
  
     The cover group contains items for these two fields, using ranges to define 
     buckets for the edge values.
  
     Run the example, note the edge buckets, named Lowest/Highest data.
  
     Note how both fields have different buckets - the min/max of int (bits : 8) 
     is different from min/max of uint (bits: 8).

  
<'

import e_util_type_min_max.e;

// The types have to defined in a file compiled/loaded before this file 
import types_def_usage_ex.e;

                    
struct transfer {
    addr : addr_t;
    data : uint (bits : 8);
};

unit monitor {
    cur_transfer : transfer;
    
    event ended;

    cover ended is {
        item address :  addr_t = cur_transfer.addr  using ranges = {
            range([MIN_VALUE(addr_t)], "Lowest address");
            range([MIN_VALUE(addr_t)+1..MAX_VALUE(addr_t)-1], "",
                  RANGE_SIZE(addr_t)/10, 10);
            range([MAX_VALUE(addr_t)], "Highest address");  
        };
        
        item data :  uint (bits : 8) = cur_transfer.data  using ranges = {
            range([MIN_VALUE(uint (bits : 8))], "Lowest data");
            range([MIN_VALUE(uint (bits : 8))+1..MAX_VALUE(uint (bits : 8))-1], "", 
                  RANGE_SIZE(uint (bits : 8))/10, 10);
            range([MAX_VALUE(uint (bits : 8))], "Highest data");     
        };
    };
            
    // dummy scenario, create some random items
    scenario() @sys.any is {
        raise_objection(TEST_DONE);
        
        for i from 0 to 20 {
            wait cycle;
            gen cur_transfer;
            emit ended;
        };
        wait cycle;
        drop_objection(TEST_DONE);
    };
    run() is also {
        start scenario();
    };


};

extend sys {
    monitor : monitor is instance;

    post_generate() is also {
        print_types();
    };
    print_types() is {

        print MIN_VALUE(int);
        print MAX_VALUE(int);
        print MIN_VALUE(uint);
        print MAX_VALUE(uint);
        print MIN_VALUE(longint);
        print MAX_VALUE(longint);
        print MIN_VALUE(longuint);
        print MAX_VALUE(longuint);

        var min_int_val := MIN_VALUE(int);
        print min_int_val;
        var range_int_val := RANGE_SIZE(int);
        print range_int_val;
        var min_uint_val := MIN_VALUE(uint);
        print min_uint_val;
        var range_uint_val := RANGE_SIZE(uint);
        print range_uint_val;

        out("\n-----------------------\n");

        // all of these work with define as<
        print MIN_VALUE(addr_t);
        print MAX_VALUE(addr_t);
        print RANGE_SIZE(addr_t);
        
        
        var first_color : color_t = MIN_VALUE(color_t);
        print first_color;

        print MIN_VALUE(color_t);
        print MAX_VALUE(color_t);
        print RANGE_SIZE(color_t);
      
        out("\n-----------------------\n");
    };
};
'>
