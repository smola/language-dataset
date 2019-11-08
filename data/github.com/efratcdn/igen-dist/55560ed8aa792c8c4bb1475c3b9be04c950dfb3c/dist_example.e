 Demonstrating some of possible distributions policies, using soft-select.
  
  Defining multiple kinds of struct, very similar to each other. The only
  difference -  their distribution
  
  The second parameter of the normal() method is the standard deviation. 
  Because the range is big - choosing a big value for this parameter.

<'
struct pkt {
    address : uint;
};


type dist_kind : [NO_CONSTRAINT, MAINLY_MIN_MAX, NORMAL, 
                  REVERSE_NORMAL, NORMAL_AND_EDGES];

unit env {
    dist_kind : dist_kind;
    !pkt: pkt;

    // Distribution:
    when NO_CONSTRAINT env {
        // no constraints..
    };
    
    when MAINLY_MIN_MAX env {
        // half of the items - to min, 25% to max
        keep soft pkt.address == select {
            50 : min;
            25 : max;
            25 : others;
        };
    };
    
    when NORMAL env {
        // normal distribution around half the range
        keep soft pkt.address == select {
            100 : normal(MAX_UINT/2, MAX_UINT/10);
        };
    };
    when NORMAL_AND_EDGES env {
        // normal distribution around half the range, the rest - to the edges
        keep soft pkt.address == select {
            50 : normal(MAX_UINT/2, MAX_UINT/10);
            50 : edges;
        };
    };
    
    when REVERSE_NORMAL {
        // normal distribution around the edge values 
        keep soft pkt.address == select {
            50 : normal(2,  10);
            50 : normal(MAX_UINT-2, 10);
        };
    };
    
    // Coverage:
    event ended;
    cover ended is {
        item address : uint = pkt.address using ranges = {
            range([0], "Lowest", UNDEF, 10);
            range([1..10], "Low end", UNDEF, 10);
            range([11..MAX_UINT-9], "", MAX_UINT/20, 10);
            range([MAX_UINT-10..MAX_UINT-1], "High end address", UNDEF, 10);
            range([MAX_UINT], "Highest address", UNDEF, 10);
        };
    };
    
    create_pkts() @sys.any is {
        raise_objection(TEST_DONE);
        for i from 0 to 100 {
            wait cycle;
            gen pkt; 
            emit ended;
        };
        drop_objection(TEST_DONE);
    };
    run() is also {
        start create_pkts();
    };
};
extend sys {
    NO_CONSTRAINT_env    : NO_CONSTRAINT env is instance;
    MAINLY_MIN_MAX_env   : MAINLY_MIN_MAX env is instance;
    NORMAL_env           : NORMAL env is instance;
    NORMAL_AND_EDGES_env : NORMAL_AND_EDGES env is instance;
    REVERSE_NORMAL_env   : REVERSE_NORMAL env is instance;
};
'>
