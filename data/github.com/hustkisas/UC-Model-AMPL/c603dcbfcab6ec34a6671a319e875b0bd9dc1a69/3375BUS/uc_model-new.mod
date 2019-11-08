#--------------------------------------------------------------
#Unit Commitment Model (New)
#--------------------------------------------------------------


param NUNITS;                          # Number of units
param NSEGMENTS;                       # Number of segments of energy blocks
param NHOURS;                          # Number of hours
param NBUS;														 # Number of buses
param NBRANCH;                         # Number of branches
#param NPS;														 # Number of phase shifter

set G := 1..NUNITS;
set K := 1..NSEGMENTS;
set T := 1..NHOURS;
set N; #:= 1..NBUS;                    # Set of buses
set L; #:= 1..NBRANCH;                 # Set of transmission lines
#set PS; #:= 1..NPS;

param D {T};                           # total Demand of each hour

param PMAX {G};                        # Maximum power output 
param PMIN {G};                        # Minimum power output               
param MC {G,K};                        # Marginal cost of a block          
param PR {G,K};                        # Size of an energy block
param BUS_UNIT {G};                    # bus index of each generator unit

param UP_RESERV:= 0.10;
param DOWN_RESERV:= 0.05;

param RD {G};           # Ramp-down limit 
param RU {G};           # Ramp-up limit 
param PO {G};           # Initial power output 
param MSR {G};          # ramp rate per minute
param NLC {G};          # online cost

# UC variable and parameters---start
param SUCC {G};         # Start up cold costs
param SUHC {G};         # Start up hot costs 
param SDC {G};          # Shut-down cost 
param nCOLD {G};        # Number of hours of a cold start 
param nSTATE {G};       # Number of hours on (+) or off (-) before hour 1           
param MDT {G};          # Minimum down time 
param MUT {G};          # Minimum up time 
param QSC {G};          # MW
param INOFF {G};        # Number of hours that the unit has been off before hour 1
param MBOFF {G};        # Number of hours that the unit must be off from hour 1
param MBON {G};         # Number of hours that the unit must be on from hour 1
param UO {G};           # Initial state 
#var u {G,T} binary;     # Unit commitment 
var u_new {G,T} binary;     # Unit commitment
var y {G,T} >=0,<=1 binary;    # Start up 
var z {G,T} >=0,<=1 binary;    # Shut down
#var p {g in G,T}>=0, <=PMAX[g];         # Generation output
var p_new {g in G,T}>=0, <=PMAX[g]-PMIN[g];         # Generation output
var p_ur{g in G,T} >=0, <=PMAX[g];      # Committed generation capacity with up reserve
var p_dr{g in G,T} >=0, <=PMAX[g];      # Committed generation capacity with down reserve
var csu {G,T} >= 0;     # Start up cost
#constcsu2 {i in I, t in T}: csu[i,t] >= SUCC[i]*(y[i,t] - sum{n in 2..(MDT[i]+TCOLD[i]): n <= t-1} u[i, t-n] - max(0, 1-t-INOFF[i] + MDT[i]+TCOLD[i])); # Cold start cost
#var csd {g in G, t in T} = SDC[g]*z[g,t]; # Shut down cost
var theta   {N,T};          # bus voltage angles

var ff {L,T}>=0;          # forward power flow  from --> to
var fb {L,T}>=0;          # backward power flow  to --> from

# Dispatch variables
var delta {g in G,k in K,t in T} >= 0,<=PR[g,k]; #generation p seg

# Power generation cost*UC variable and parameters
var cpg{g in G, t in T} = NLC[g]*(u_new[g,t] + y[g,t]) + sum{k in K} MC[g,k]*delta[g,k,t];

#UC variable and parameters---end


# bus parameters
param bus_type {N};                  # type SL = 3, PV = 2, PQ = 0
param bus_p_load {N};
param p_bus_load {N,T};              # power load at each bus
param p_bus_load_por {N};            # power load distribution (%) at each bus

# branch parameters
param branch_from   {L};          # "from" bus
param branch_to     {L};          # "to" bus
param branch_type   {L};          # type 0=transmission line 1=fixed ratio transformer 2=OLTC 4=phase shifter 

param branch_p_maxflow {L};       # maximum power flow on each branch
param branch_p_maxflow2 {L};       # maximum power flow on each branch
param branch_p_maxflow3 {L};       # maximum power flow on each branch
param branch_r      {L};          # branch resistance
param branch_x      {L};          # branch reactance
param branch_c      {L};          # branch reactance
param branch_B			{L};
param UBO {L};          # initial decision variable


# other parameters
param deg_2_rad    := 3.14159/180;     # conversion from degrees to radians
param bmva         := 100;             # base MVA rating



# OBJECTIVE FUNCTION*UC variable and parameters
minimize totalcost: sum {g in G, t in T} cpg[g,t] + sum { g in G, t in T}csu[g,t];# +sum { g in G, t in T} csd[g,t];
startup_cost {g in G, t in T}: -csu[g,t] <= -SUHC[g]*y[g,t];  # Hot start cost

# SUBJECT TO THE FOLLOWING CONSTRAINTS

# Energy block*UC

gen_lmt1 {g in G, t in T}: p_new[g,t] = sum{k in K} delta[g,k,t];
gen_lmt2 {g in G, t in T}: (p_new[g,t] + PMIN[g]*(u_new[g,t] + y[g,t])) <= p_ur[g,t];
gen_lmt3 {g in G, t in T}: -(p_new[g,t] + PMIN[g]*(u_new[g,t] + y[g,t])) <= -p_dr[g,t];
gen_lmt4 {g in G, t in T}: p_ur[g,t] <= PMAX[g]*(u_new[g,t] + y[g,t]);
gen_lmt5 {g in G, t in T}: -p_dr[g,t] <= -PMIN[g]*(u_new[g,t] + y[g,t]);

# supply=demand
sup_demand {t in T}: sum{g in G}(p_new[g,t] + PMIN[g]*(u_new[g,t] + y[g,t])) = sum{n in N} p_bus_load[n,t];

# reserve comstraints
reserve_up{t in T}: sum{g in G}-p_ur[g,t] <= -sum{n in N} p_bus_load[n,t]*(1+UP_RESERV) ;
reserve_down{t in T}: sum{g in G}p_dr[g,t] <= sum{n in N} p_bus_load[n,t]*(1-DOWN_RESERV) ;

###DC network constraints (one here, and remaining later)
#bus power flow balance
flow_bal {n in N, t in T}:
   sum {g in G: BUS_UNIT[g]=n} (p_new[g,t] + PMIN[g]*(u_new[g,t] + y[g,t])) - p_bus_load[n,t] - (sum{l in L: branch_from[l]=n} ff[l,t] +sum{l in L: branch_to[l]=n} fb[l,t]) + (sum{l in L: branch_to[l]=n} ff[l,t]+sum{l in L: branch_from[l]=n} fb[l,t])= 0;

##ramping rate*UC
#p[g,1] can be fixed to initial power output if provided
ramp_up {g in G, t in T: t>=2}: p_ur[g,t] - (p_new[g,t-1] + PMIN[g]*(u_new[g,t-1] + y[g,t-1])) <= RU[g]*(u_new[g,t-1] + y[g,t-1]) + RU[g]*y[g,t]; # use ramping up rate as start up rate temporarily
ramp_down {g in G, t in T: t>=2}: (p_new[g,t-1] + PMIN[g]*(u_new[g,t-1] + y[g,t-1])) - p_dr[g,t] <= RD[g]*(u_new[g,t] + y[g,t])+ RD[g]*z[g,t]; # use ramping down rate as shut down rate temporarily

# Min up and startup*UC
min_up1 {g in G, t in 1..MBON[g]}: u_new[g,t] + y[g,t] = 1; # initially must be on for MBON hours
min_up2 {g in G, tp in T, t in tp..tp+MUT[g]-1: tp >= MBON[g] && t <= card(T)}: y[g,tp]<=u_new[g,t] + y[g,t]; # this can be replaced by aggregated constraints
#min_up2 {g in G, tp in T: tp >= MBON[g]+1 && tp <= card(T)-MUT[g]+1}: sum{t in tp..tp+MUT[g]-1} -u_new[g,t] - y[g,t] <= -MUT[g]*y[g,tp];
#min_up3 {g in G, tp in T: tp >= (card(T)-MUT[g]+2) && tp <= card(T)}: sum{t in tp..card(T)} (-u_new[g,t] - y[g,t] + y[g,tp]) <= 0;

# Min down and shutdown*UC
min_down1 {g in G, t in 1..MBOFF[g]}: u_new[g,t] + y[g,t] = 0; # initially must be off for MBOFF hours
min_down2 {g in G, tp in T, t in tp..(tp+MDT[g]-1): tp >= MBON[g] && t <= card(T)}: z[g,tp] <= 1 - u_new[g,t] - y[g,t];
#min_down2 {g in G, tp in T: tp >= MBOFF[g]+1 && tp <= card(T)-MDT[g]+1}: sum{t in tp..tp+MDT[g]-1} (-1 + u_new[g,t] + y[g,t]) <= -MDT[g]*z[g,tp];
#min_down3 {g in G, tp in T: tp >= (card(T)-MDT[g]+2) && tp <= card(T)}: sum{t in tp..card(T)} (-1 + u_new[g,t] + y[g,t] + z[g,tp]) <= 0;

###DC network constraints AGAIN
#Kirchoff's law
kirchoff_law {t in T, l in L}:
    (theta[branch_from[l],t]-theta[branch_to[l],t])*branch_B[l]*deg_2_rad = (ff[l,t]-fb[l,t])/bmva;

#transmission line thermal limit (security constraints)    
line_lmt1 {l in L, t in T}: ff[l,t]-fb[l,t] <= branch_p_maxflow[l];
line_lmt2 {l in L, t in T}: -ff[l,t]+fb[l,t] <= branch_p_maxflow[l];

#reference bus angle
ref_bus {t in T, n in N : bus_type[n] = 3}: theta[n,t]=0*deg_2_rad;

# Both
urelation {g in G, t in T: t>=2}: u_new[g,t] - u_new[g,t-1] = y[g,t-1]- z[g,t]; 
urelation1 {g in G}: u_new[g,1] - UO[g] = - z[g,1]; 
