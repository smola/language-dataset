script "volcano_mining.ash";
notify CrankyOne;
since r15029;

import <zlib.ash>;

string version = "1.0";

// These global initializations now just set defaults.  To change actual values,
// type "zlib <varName> = <value>" or modify data/vars_yourname.txt

//Max caves to check against, mainly to solve for infinite loops just in case
setvar("vmine_maxCaves", 2000);
int MAX_CAVES = to_int(vars["vmine_maxCaves"]);

//Name of the outfit to switch to for mining
setvar("vmine_outfit", "Volcano Mining");
string MINING_OUTFIT = vars["vmine_outfit"];

//Re-apply mood while mining
setvar("vmine_moodexec", false);
boolean MINING_MOODEXEC = to_boolean(vars["vmine_moodexec"]);

//Only mine direct sparkles in the first two rows if true, mine until gold if false
setvar("vmine_lazyFarm", false);
boolean LAZY_FARM = to_boolean(vars["vmine_lazyFarm"]);

//How many turns you want to have left when finished mining
setvar("vmine_numTurnsToLeave", 5);
int NUM_TURNS_TO_LEAVE = to_int(vars["vmine_numTurnsToLeave"]);

//In seconds, This will allow you to add delays after each mining attempt, used
//to spread out server load especially if you are starting script and letting
//it run while you go do other things and don't care when it finishes
setvar("vmine_delayBetweenMines", 0);
int DELAY_BETWEEN_MINES = to_int(vars["vmine_delayBetweenMines"]);

//Automatically use potions of detection before attempting to parse a mine if true
setvar("vmine_autoUpDetection", true);
boolean AUTO_UP_DETECTION = to_boolean(vars["vmine_autoUpDetection"]);

//Whether to use kol's healing script, if set to false will attempt custom logic to heal a point
setvar("vmine_useMafiaRestore", true);
boolean USE_MAFIA_RESTORE = to_boolean(vars["vmine_useMafiaRestore"]);

//Determines if you will use a healing skill or just go to docs, a value of $skill[none] means goto doc, otherwise attempt to use skill provided, if that doesn't work then it will still go to docs
//Note only used if useMafiaRestore = false;
setvar("vmine_healingSkill", "none");
skill HEALING_SKILL = vars["vmine_healingSkill"].to_skill();
//Some example skills to set vmine_healingSkill to
//"lasagna bandages", "tongue of the walrus", "cannelloni cocoon", "shake it off"

//Variable to let the script abort mining without calling abort()
boolean doneMining = false;

record Spot {
	int row;
	int col;
	int counter;
	boolean isInteresting;
	boolean isEmpty;
	boolean checkedForChain;
	int numDirects;
	int costToGetTo;
	boolean isNotVelvet;
	string shortestRoute;
	boolean mined;
};

record Chain {
	int numSpotsInChain;
	Spot[int] spotsInChain;
	Spot entryPoint;
};

record Mine {
	Chain currentLongestChain;
	Spot[int] interestingSpots;
	Spot[int] nearInterestingSpots;
	Spot[int] emptySpots;
	Spot[int][int] spots;
	int goldFound;
	int velvetFound;
	int crystalsFound;
	Spot[int] tailingSpotsChecked;
};

record MiningOperation {
	int startingNumGold;
	int startingNumVelvet;
	int startingNumCrystals;
	int numCavesSkipped;
	int numCavesFullyExplored;
	int numCaveIns;
	int numOilyLegsAtStart;
	int startTime;
	int startAdvs;
};

Spot newSpot(int counter, int col, int row) {
	Spot result = new Spot();
	result.col = col;
	result.row = row;
	result.counter = counter;
	result.isInteresting = false;
	result.numDirects = 0;
	result.costToGetTo = 10000;
	result.isNotVelvet = false;
	result.checkedForChain = false;
	result.shortestRoute = "";
	result.mined = false;
	return result;	
}

Chain newChain() {
	Chain newChain = new Chain();
	newChain.numSpotsInChain = 0;
	clear(newChain.spotsInChain);
	return newChain;
}


Mine newMine() {
	Mine result = new Mine();
	result.currentLongestChain = newChain();
	clear(result.interestingSpots);
	clear(result.spots);
	result.goldFound = 0;
	result.velvetFound = 0;
	result.crystalsFound = 0;
	return result;
}

Mine currentMine;
MiningOperation currentOperation; 

string printSpot(Spot currentSpot) {
	buffer result;
	result.append("(");
	result.append(currentSpot.col);
	result.append(",");
	result.append(currentSpot.row);
	result.append(")");
	return result;
}

void restoreMinHP() {
	if (USE_MAFIA_RESTORE) {
		//Trigger Mafia's MP and HP restoration
		restore_mp(0);
		restore_hp(1);
	} else if (HEALING_SKILL != $skill[none]) {
		//Attempt to use skill
		if (my_mp() > mp_cost(HEALING_SKILL))
			use_skill(HEALING_SKILL);
	}
	
	if (my_hp() == 0) {
		//if (my_meat() < 10)
		//	abort("Don't have enought meat to heal");
			
		//"galaktik" isn't a valid CLI command now.  Commenting this out.
		//cli_execute("galaktik hp 1"); //Thanks to Ezandora for pointing this out.
		abort("No valid healing options chosen");
	}
}

void restore_outfit(){
	outfit("Backup");
}

void endMiningOperation() {
	restore_outfit();
	int numGold = item_amount($item[1,970 carat gold]) - currentOperation.startingNumGold;
	int numVelvet = item_amount($item[unsmoothed velvet]) - currentOperation.startingNumVelvet;
	int numCrystals = item_amount($item[New Age healing crystal]) - currentOperation.startingNumCrystals;
	int numTurns = currentOperation.startAdvs - my_adventures();
	int amtProfit = numGold * 19700;

	int numMSTaken = gametime_to_int() - currentOperation.startTime;
	float numSecondsTaken = numMSTaken / 1000;
	float turnsPerGold = 0;
	if (numGold > 0)
		turnsPerGold = to_float(numTurns)/to_float(numGold);
	
	print("Number of Fully Explored Caves: " + currentOperation.numCavesFullyExplored, "blue");
	print("Number of Caves skipped because of turn expense: " + currentOperation.numCavesSkipped, "blue");
	print("Number of non-item spaces mined: " + currentOperation.numCaveIns, "blue");
	print("Number of " + $item[unsmoothed velvet] + " found: " + numVelvet, "blue");
	print("Number of " + $item[1,970 carat gold] + " found: " + numGold, "blue");
	print("Number of " + $item[New Age healing crystal] + " found: " + numCrystals, "blue");
	print("Number of seconds it took: " + numSecondsTaken, "blue");
	print("Number of turns used: " + numTurns, "blue");
	print("Average turns per gold: " + turnsPerGold, "blue");
	print("Total autosale price of gold: " + amtProfit, "blue");
}

void abortMining(string reason) {
	endMiningOperation();
	abort(reason);
}

void processSpotForChain(Chain currentChain, Spot currentSpot) {
	//Add to chain, marked off as being checked
	int numInChain = count(currentChain.spotsInChain);
	currentChain.spotsInChain[numInChain] = currentSpot;
	currentSpot.checkedForChain = true;
	
	int row = currentSpot.row;
	int col = currentSpot.col;
	
	//Check Left
	if (col != 1) {
		Spot checkSpot = currentMine.spots[col - 1][row];
		if (checkSpot.isInteresting) {
			currentSpot.numDirects += 1;
			if (!checkSpot.checkedForChain)
				processSpotForChain(currentChain, checkSpot);
		}
	}

	//Check Right
	if (col != 6) {
		Spot checkSpot = currentMine.spots[col + 1][row];
		if (checkSpot.isInteresting) {
			currentSpot.numDirects += 1;
			if (!checkSpot.checkedForChain)
				processSpotForChain(currentChain, checkSpot);
		}
	}

	//Check Above
	if (row != 1) {
		Spot checkSpot = currentMine.spots[col][row - 1];
		if (checkSpot.isInteresting) {
			currentSpot.numDirects += 1;
			if (!checkSpot.checkedForChain)
				processSpotForChain(currentChain, checkSpot);
		}
	}

	//Check Below
	if (row != 6) {
		Spot checkSpot = currentMine.spots[col][row + 1];
		if (checkSpot.isInteresting) {
			currentSpot.numDirects += 1;
			if (!checkSpot.checkedForChain)
				processSpotForChain(currentChain, checkSpot);
		}
	}

	
}

void calculateLongestChain() {
	Chain currentChain = newChain();
	Spot currentSpot;
	foreach spotNdx in currentMine.interestingSpots {
		currentSpot = currentMine.interestingSpots[spotNdx];
		if (currentSpot.isInteresting && !currentSpot.checkedForChain) {
			processSpotForChain(currentChain, currentSpot);
		}
		
		if (count(currentChain.spotsInChain) >= 6) // Found the chain we need, only chains 6 and longer here are the ones we are looking for
			break;
		else {
			foreach tmpSpotNdx in currentChain.spotsInChain {
				Spot tmpSpot = currentChain.spotsInChain[tmpSpotNdx];
				tmpSpot.isNotVelvet = true;
			}
		}
	}
	
	currentMine.currentLongestChain = currentChain;
	
	foreach spotNdx in currentMine.interestingSpots {
		currentSpot = currentMine.interestingSpots[spotNdx];
		if (currentSpot.isInteresting && !currentSpot.checkedForChain)
			currentSpot.isNotVelvet = true;
	}
}

void calcPathsFromSpot(Spot currentSpot, string pathSoFar, int lengthSoFar) {
	int increment = 1;
	
	int tmpLength = lengthSoFar + increment;
	if (tmpLength < currentSpot.costToGetTo) {
		currentSpot.costToGetTo = tmpLength;
		pathSoFar = currentSpot.counter +  ";" + pathSoFar;
		currentSpot.shortestRoute = pathSoFar;
		if (currentSpot.col > 1)
			calcPathsFromSpot(currentMine.spots[currentSpot.col - 1][currentSpot.row], pathSoFar, currentSpot.costToGetTo);
		if (currentSpot.row > 1)
			calcPathsFromSpot(currentMine.spots[currentSpot.col][currentSpot.row - 1], pathSoFar, currentSpot.costToGetTo);
		if (currentSpot.col < 6)
			calcPathsFromSpot(currentMine.spots[currentSpot.col + 1][currentSpot.row], pathSoFar, currentSpot.costToGetTo);
		if (currentSpot.row < 6)
			calcPathsFromSpot(currentMine.spots[currentSpot.col][currentSpot.row + 1], pathSoFar, currentSpot.costToGetTo);
	}
}

void updateHeatmapFromSpot(Spot startSpot) {
    if (startSpot.costToGetTo > 0) {
    	print("updateHeatmapFromSpot called on non-empty spot!", "red");
    	return;
    }

    // Since we've verified the spot is empty, the shortest route is to itself
    startSpot.shortestRoute = startSpot.counter + ";";

    if (startSpot.col > 1)
			calcPathsFromSpot(currentMine.spots[startSpot.col - 1][startSpot.row], "", 0);
	if (startSpot.row > 1)
			calcPathsFromSpot(currentMine.spots[startSpot.col][startSpot.row - 1], "", 0);
	if (startSpot.col < 6)
			calcPathsFromSpot(currentMine.spots[startSpot.col + 1][startSpot.row], "", 0);
	if (startSpot.row < 6)
			calcPathsFromSpot(currentMine.spots[startSpot.col][startSpot.row + 1], "", 0);
}

void parseMine() {
	//string page = visit_url("place.php?whichplace=desertbeach&action=db_crimbo14mine");
	if ((have_effect($effect[object detection]) == 0) && (AUTO_UP_DETECTION == true))
		cli_execute("use potion of detection");
	buffer page = visit_url("mining.php?mine=6");
	if (page.contains_text("way too beaten up")) {
		restoreMinHP();
		page = visit_url("mining.php?mine=6");
	}
	if (!page.contains_text("INSTRUCTIONS: Starting at the bottom of the mine"))
		abort("Unable to access the Velvet/Gold Mine.  Please manually verify that you can get to it.");
	if (page.contains_text("<table cellpadding=0 cellspacing=0 border=0 background=")) {
		page.substring(page.index_of("<table cellpadding=0 cellspacing=0 border=0 background="));
		for counter from 0 to 54 {
			int rowNdx = counter / 8;
			int colNdx = counter % 8;
			if (rowNdx > 0 && rowNdx < 7) {
				if (colNdx > 0 && colNdx < 7) {	
					Spot newSpot = newSpot(counter, colNdx, rowNdx);
					newSpot.costToGetTo = 7 - rowNdx;
					string tmpPathStr = "";
					for  i from rowNdx to 6 {
						tmpPathStr = ((i * 8) + colNdx) + ";" + tmpPathStr;
					}
					newSpot.shortestRoute = tmpPathStr;
					currentMine.spots[colNdx][rowNdx] = newSpot;
					
					if (page.contains_text("Promising Chunk of Wall (" + colNdx + "," + rowNdx + ")")) {
						currentMine.interestingSpots[count(currentMine.interestingSpots)] = newSpot;
						if (rowNdx > 4)
							currentMine.nearInterestingSpots[count(currentMine.nearInterestingSpots)] = newSpot;
						newSpot.isInteresting = true;
					}
					if (page.contains_text("Open Cavern (" + colNdx + "," + rowNdx + ")")) {
						currentMine.emptySpots[count(currentMine.emptySpots)] = newSpot;
						newSpot.costToGetTo = 0;
					}
				}
			}
		}
		foreach tmpSpotNdx in currentMine.emptySpots {
			updateHeatmapFromSpot(currentMine.emptySpots[tmpSpotNdx]);
		}
	}
}

void figureRoute() {
	int cheapestPath = 100000;
	Spot firstSpot;

	foreach spotNdx in currentMine.currentLongestChain.spotsInChain {
		Spot currentSpot = currentMine.currentLongestChain.spotsInChain[spotNdx];
		if ((currentSpot.costToGetTo < cheapestPath) && (currentSpot.costToGetTo > 0)) {
			cheapestPath = currentSpot.costToGetTo;
			firstSpot = currentSpot;
			currentMine.currentLongestChain.entryPoint = currentSpot;
		}
	}
	
	//print("Starting point will be: " + printSpot(firstSpot) + " path: " + firstSpot.shortestRoute);
}

void printMine() {
	for rowNdx from 1 to 6 {
		if (rowNdx == 1)
			print("=======================================");
		else 
			print("----------------------------------------");
		buffer row;
		row.append("| ");
		
		for colNdx from 1 to 6 {
			if (currentMine.spots[colNdx][rowNdx].isInteresting)
				row.append(" " + currentMine.spots[colNdx][rowNdx].numDirects + " ");
			else
				row.append(" _ ");
			row.append(" |");
		}
		print(row);
	}
}

void printPaths() {
	for rowNdx from 1 to 6 {
		if (rowNdx == 1)
			print("=======================================");
		else 
			print("----------------------------------------");
		buffer row;
		row.append("| ");
	
		for colNdx from 1 to 6 {
			row.append(" " + currentMine.spots[colNdx][rowNdx].costToGetTo + " ");
			row.append(" |");
		}
		print(row);

	}

}

void calcShortestPathsToAllSpots() {
	for colNdx from 1 to 6 by 1 {
		Spot startSpot = currentMine.spots[colNdx][6];
		calcPathsFromSpot(startSpot, "", 0);
	} 	
}


void gotoNextMine() {
	visit_url("mining.php?mine=6&reset=1&pwd");
	currentMine = newMine();
}

void mineSpot(Spot spotToMine) {
	if (my_hp() == 0) {
		restoreMinHP();
	}
	
	if (spotToMine.mined == true)
	    return; //Make attempts to mine empty squares into no-ops

	if (my_adventures() <= NUM_TURNS_TO_LEAVE)
		abortMining("At minimum number of turns left");
	
	string result = visit_url("mining.php?mine=6&which=" + spotToMine.counter + "&pwd");
	int[item] itemsFound = extract_items(result);
	if (spotToMine.isInteresting) {
		if (count(itemsFound) > 0) {
			foreach itemFound in itemsFound {
				if (itemFound == $item[1,970 carat gold]) 
					currentMine.goldFound += 1;
				if (itemFound == $item[unsmoothed velvet])
					currentMine.velvetFound += 1;
				if (itemFound == $item[New Age healing crystal])
					currentMine.crystalsFound += 1;
			}
		} else {
			currentOperation.numCaveIns += 1;
		}
	}
	
	spotToMine.mined = true;
	spotToMine.costToGetTo = 0;
	currentMine.emptySpots[count(currentMine.emptySpots)] = spotToMine;
	updateHeatmapFromSpot(spotToMine);

	if ( MINING_MOODEXEC )
		cli_execute("mood execute");
	
	if ( DELAY_BETWEEN_MINES > 0 )
		waitq(DELAY_BETWEEN_MINES);
}

void mineChainSpot(Spot spotToMine) {
	if (spotToMine.mined)
		return;
	if (!spotToMine.isInteresting)
		return;
	if (currentMine.velvetFound >= 6)
		return;//Already found 6 velvet here so rest of chain is cave ins
	
	if (spotToMine.isInteresting) {
		if (!spotToMine.mined) 
			mineSpot(spotToMine);
		if (spotToMine.row != 1)
			mineChainSpot(currentMine.spots[spotToMine.col][spotToMine.row - 1]);
		if (spotToMine.col != 1) 
			mineChainSpot(currentMine.spots[spotToMine.col - 1][spotToMine.row]);
		if (spotToMine.col != 6)
			mineChainSpot(currentMine.spots[spotToMine.col + 1][spotToMine.row]);
		if (spotToMine.row != 4)
			mineChainSpot(currentMine.spots[spotToMine.col][spotToMine.row + 1]);
	}
	
}

Spot findSpotByCounter(int counter) {
	int row = counter / 8;
	int col = counter % 8;
	
	return currentMine.spots[col][row];
}

void mineToSpot(Spot destination) {
	string routeToSpot = destination.shortestRoute;
	string[int] spots = split_string(destination.shortestRoute, ";");
	foreach ndx in spots {
		if (destination.counter != to_int(spots[ndx]))
			mineSpot(findSpotByCounter(to_int(spots[ndx])));	
	}
	mineSpot(destination);
}

void handleRoute() {
	Spot entryPoint = currentMine.currentLongestChain.entryPoint;
	string routeToEntry = entryPoint.shortestRoute;
	string[int] spots = split_string(routeToEntry, ";");
	foreach ndx in spots {
		if (entryPoint.counter != to_int(spots[ndx]))
			mineSpot(findSpotByCounter(to_int(spots[ndx])));
		
	}
	
	mineChainSpot(entryPoint);
}
/*
void useTestMine(int testMineNumber) {
	int ndxISpots = 0;
	buffer testMineBuffer;
	switch (testMineNumber) {
		case 1:
			testMineBuffer.append("WWWWWWWW");
			testMineBuffer.append("W00IIIIW");
			testMineBuffer.append("W00000IW");
			testMineBuffer.append("W00000IW");
			testMineBuffer.append("W000II0W");
			testMineBuffer.append("W000000W");
			testMineBuffer.append("W0000IIW");
			break;	
		default:
			testMineBuffer.append("WWWWWWWW");
			testMineBuffer.append("WIIII00W");
			testMineBuffer.append("W0II000W");
			testMineBuffer.append("W00I000W");
			testMineBuffer.append("W000000W");
			testMineBuffer.append("W00000IW");
			testMineBuffer.append("W000II0W");
			break;
	}
	for counter from 0 to 54 {
		int rowNdx = counter / 8;
		int colNdx = counter % 8;
		if (rowNdx > 0 && rowNdx < 7) {
			if (colNdx > 0 && colNdx < 7) {	
				Spot newSpot = newSpot(counter, colNdx, rowNdx);
				currentMine.spots[colNdx][rowNdx] = newSpot;
				if (testMineBuffer.char_at(counter) == 'I') {
					currentMine.interestingSpots[ndxISpots] = newSpot;
					newSpot.isInteresting = true;
					ndxISpots += 1;
					if (rowNdx >= 5)
						newSpot.isCaveIn = true;
				}
			}
		}
	}
}
*/

void mineSingleSpot() {
	Spot singleSpot = currentMine.spots[1][6];
	while (singleSpot.isNotVelvet) {
		singleSpot = currentMine.spots[singleSpot.col + 1][6];
	}
	mineSpot(singleSpot);
}

int findCheapestSpot(Spot[int] listOfSpots) {
	int cheapestFound = 10000;
	boolean foundViableSpot = false;
	Spot cheapestSpot = listOfSpots[0];
	foreach spotNdx in listOfSpots {
		if ((listOfSpots[spotNdx].costToGetTo < cheapestFound) && !listOfSpots[spotNdx].mined) {
			cheapestSpot = listOfSpots[spotNdx];
			cheapestFound = listOfSpots[spotNdx].costToGetTo;
			foundViableSpot = true;
		}
	}
	if (foundViableSpot)
		return cheapestSpot.counter;
	else
		return -1;
}

void handleCurrentMine() {
	if (have_effect($effect[object detection]) == 0 && !AUTO_UP_DETECTION) {
		abortMining("This script requires that you have the " + $effect[object detection] + " effect active while starting the mining process");
	}

	parseMine(); //First thing to do is parse the mine
	//useTestMine(0);
	calculateLongestChain(); //Then calculate longest chain

	printMine();
	//printPaths();

	// Look for sparkles in the first three rows
	int cheapestCounter = findCheapestSpot(currentMine.nearInterestingSpots);

	// No sparkles in first two rows
	if (cheapestCounter == -1) {
		currentOperation.numCavesSkipped += 1;
		mineSpot(currentMine.spots[1][6]);
		return;
	}
	
	if (LAZY_FARM == true) {
		while (cheapestCounter != -1) {
			Spot cheapestSpot = findSpotByCounter(cheapestCounter);
			if (cheapestSpot.costToGetTo > 1) {
				currentOperation.numCavesSkipped += 1;
				if (count(currentMine.emptySpots) == 0)
					mineSpot(currentMine.spots[1][6]);
				return;
			}
			if ((my_adventures() - NUM_TURNS_TO_LEAVE) < cheapestSpot.costToGetTo) {
				currentOperation.numCavesSkipped += 1;
				doneMining = true;
				return;
			}
			mineToSpot(cheapestSpot);
			if (currentMine.goldFound > 0)
				break;
			cheapestCounter = findCheapestSpot(currentMine.nearInterestingSpots);
		}
	} else {
		cheapestCounter = findCheapestSpot(currentMine.interestingSpots);
		while (cheapestCounter != -1) {
			Spot cheapestSpot = findSpotByCounter(cheapestCounter);
			if (cheapestSpot.costToGetTo > 2) {
				currentOperation.numCavesSkipped += 1;
				if (count(currentMine.emptySpots) == 0)
					mineSpot(currentMine.spots[1][6]);
				return;
			}
			if ((my_adventures() - NUM_TURNS_TO_LEAVE) < cheapestSpot.costToGetTo) {
				currentOperation.numCavesSkipped += 1;
				doneMining = true;
				return;
			}
			mineToSpot(cheapestSpot);
			if (currentMine.goldFound > 0)
				break;
			cheapestCounter = findCheapestSpot(currentMine.interestingSpots);
		}
	}

	currentOperation.numCavesFullyExplored += 1;
}

void initMiningOperations() {
	cli_execute("outfit save Backup");
	outfit(MINING_OUTFIT);
	
	currentOperation = new MiningOperation();
	currentOperation.startingNumGold = item_amount($item[1,970 carat gold]);
	currentOperation.startingNumVelvet = item_amount($item[unsmoothed velvet]);
	currentOperation.startingNumCrystals = item_amount($item[New Age healing crystal]);
	currentOperation.numCaveIns = 0;
	currentOperation.numCavesSkipped = 0;
	currentOperation.numCavesFullyExplored = 0;
	currentOperation.startTime = gametime_to_int();	
	currentOperation.startAdvs = my_adventures();
}

void mine_volcano() {
	initMiningOperations();

	int counter = 0;
	
	while(counter < MAX_CAVES && my_adventures() > 0 && !doneMining) {
		gotoNextMine();
		handleCurrentMine();
		counter += 1;
	}

	endMiningOperation();	
}

void mine_volcano(int turnsToMine, boolean lazyFarm, boolean autoDetection) {
	if (turnsToMine != 0)
		NUM_TURNS_TO_LEAVE = my_adventures() - turnsToMine;
	LAZY_FARM = lazyFarm;
	AUTO_UP_DETECTION = autoDetection;
	mine_volcano();
}

void mine_volcano(int turnsToMine, boolean lazyFarm, boolean autoDetection, string outfit) {
	if (turnsToMine != 0)
		NUM_TURNS_TO_LEAVE = my_adventures() - turnsToMine;
	LAZY_FARM = lazyFarm;
	AUTO_UP_DETECTION = autoDetection;
	MINING_OUTFIT = outfit;
	mine_volcano();
}

void main(int turnsToMine, boolean lazyFarm, boolean autoDetection) {
	if (turnsToMine != 0)
		NUM_TURNS_TO_LEAVE = my_adventures() - turnsToMine;
	LAZY_FARM = lazyFarm;
	AUTO_UP_DETECTION = autoDetection;
	mine_volcano();
}
