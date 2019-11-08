/********** kmeans.lsp **********/
use io;

/* Reads instance data */
function input(){
    usage = "\nUsage: localsolver kmeans.lsp "
        + "inFileName=inputFile [solFileName=outputFile] [lsTimeLimit=timeLimit] [k=value]\n";

    if (inFileName == nil) throw usage;
    local f = io.openRead(inFileName);
    nbObservations = f.readInt();
    nbVariables = f.readInt();
    
    if (k == nil){
        k = 2;
    }
    
    for[o in 1..nbObservations]{
        for[v in 1..nbVariables]{
	        M[o][v] = f.readDouble();
	    }
        clusters[o] = f.readString();
    }
    
    // mini[v] and maxi[v] are respectively the minimum and maximum of the vth variable
    for[v in 1..nbVariables]{
        mini[v] = min[o in 1..nbObservations](M[o][v]);
        maxi[v] = max[o in 1..nbObservations](M[o][v]);
    }
}

/* Declares the optimization model. */
function model(){
    // x[c][v] is the coordinate for the center of gravity of the cluster c along the variable v
    x[1..k][v in 1..nbVariables] <- float(mini[v], maxi[v]);

    // d[o] is the distance between observation o and the nearest cluster (its center of gravity)
    d[o in 1..nbObservations] <- min[c in 1..k](sum[v in 1..nbVariables](pow(x[c][v] - M[o][v], 2)));

    // Minimize the total distance
    obj <- sum[o in 1..nbObservations](d[o]);
    minimize obj;
}

/* Parameterizes the solver. */
function param(){
    if(lsTimeLimit == nil) lsTimeLimit=5;
}

/* Writes the solution in a file in the following format:
 *  - objective value
 *  - k
 *  - for each cluster, the coordinates along each variable
 */
function output(){
    if(solFileName == nil) return;
	local solFile = io.openWrite(solFileName);
    solFile.println(obj.value);
    solFile.println(k);
    for[c in 1..k] {
        for[v in 1..nbVariables] {
            solFile.print(x[c][v].value + " ");
        }
        solFile.println();
    }
}