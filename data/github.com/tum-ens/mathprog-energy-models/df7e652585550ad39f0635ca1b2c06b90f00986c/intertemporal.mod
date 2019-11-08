# INTERTEMPORAL: minimal intertemporal capacity expansion problem
# Last updated: 18 Nov 2016
# Author: johannes.dorfner@tum.de
# License: CC0 <http://creativecommons.org/publicdomain/zero/1.0/>
#
# USAGE
# glpsol -m intertemporal.mod
#
# OVERVIEW
# This LP model finds the minimum cost investment plan for for a set of two
# power plant technologies over multiple decades, allowing investment decisions
# every five years. Old investments phase out of the power plant fleet after 
# their lifetime is over. A discount factor incentivises later investment, while 
# demand must be satisfied throughout the years.
#


# SETS
set year;  # all years
set yearM within year;  # modelled years (investment decisions)
set year0 within year;  # initial years (no investments allowed)
set season;  # time steps within year
set process;  # power plant technologies


# PARAMETERS
param interest_rate;  # price change of invest/fuel over the years

param demand{s in season};  # seasonal demand
param weight{s in season};  # seasonal weight

param discount{yM in yearM}  # investment cost reduction for later invest
    default interest_rate**((yM - 2010)/5);  # exponential decay (ir < 1) or
                                             # exponential growth (ir > 1)

param invcost{p in process};    # plant investment costs (EUR/MW)
param fuelcost{p in process};   # plant fuel costs (EUR/MWh)
param efficiency{p in process}; # plant efficiency (MWh_out/MWh_in)
param lifetime{p in process};   # plant lifetime (years)


# helper data structure: contains all tuples (process, year1, year2) for each
# technology p in process, for which a new-built capacity from year1 is still 
# available in year2 (as in: plant lifetime not exceeded yet) 
set is_operational within process cross year cross year := setof{
    p in process, y1 in year, y2 in year:
        y1 <= y2 and y1 + lifetime[p] >= y2 
} (p, y1, y2);


# VARIABLES
var costs;
var new_capacity{year, process} >= 0;
var available_capacity{yearM, process} >= 0;
var production{yearM, season, process} >= 0;

# OBJECTIVE
minimize obj: costs;

# CONSTRAINTS
s.t. def_costs:
    costs = sum{yM in yearM, p in process} 
                new_capacity[yM, p] *
                invcost[p] *
                discount[yM]
          + sum{yM in yearM, s in season, p in process}
                production[yM, s, p] *
                fuelcost[p] /
                efficiency[p] *
                weight[s] *
                discount[yM];

s.t. res_no_investment_in_initial_years{y0 in year0, p in process}:
    new_capacity[y0, p] = 0;
    
s.t. def_available_capacity{this_year in yearM, p in process}:
    available_capacity[this_year, p] = sum{
            (p, earlier_year, this_year) in is_operational
        } new_capacity[earlier_year, p];

s.t. res_demand{yM in yearM, s in season}:
    demand[s] <= sum{p in process} production[yM, s, p];

s.t. res_production_available_capacity{yM in yearM, s in season, p in process}:
    production[yM, s, p] <= available_capacity[yM, p];

# SOLVE
solve;

# OUTPUT
printf "\nRESULT\n";
printf "Total cost: %g kEUR\n", costs/1e3;
printf "Interest rate: %g\n", interest_rate;

printf "\nCAPACITIES\n";
printf "%-9s %4s: %6s\n", "Process", "Year", "New Cap";
printf "-----------------------\n";
printf{yM in yearM, p in process: new_capacity[yM,p] > 0}:
    "%-6s in %4s: %4.0f MW\n", p, yM, new_capacity[yM,p];

printf "\nPRODUCTION\n";
printf "%14s  %-6s\n", " ", "Season";
printf "%-9s %3s:", "Process", "Year";
printf{s in season}: " %2s", s;
printf "\n----------------------------------\n";
for {yM in yearM, p in process}: {
    printf "%-6s in %2s:", p, yM; 
    printf{s in season}:
        (if production[yM, s, p] > 0 then " %2.0f" else " %2s"), 
        (if production[yM, s, p] > 0 then production[yM, s, p] else " ");
    printf "\n";
}
printf "\n";

# DATA
data;

set year  := 2000 2005 2010 2015 2020 2025 2030 2035 2040;
set year0 := 2000 2005 2010;
set yearM :=                2015 2020 2025 2030 2035 2040;

param interest_rate := 0.8;

param: season: demand weight:=
       S1          10   7300 
       S2          20   7300
       S3          40   7300
       S4          70   7300
       S5          50   7300
       S6          40   7300;

param: process: invcost fuelcost efficiency lifetime :=
       stcoal      1000      .01        .45       15
       gtgas        450      .02        .40       15;
