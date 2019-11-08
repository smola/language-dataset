// Sonora
// June 2004
// Nov. 2005 - JSB - Aztecs to Apache for Age3Xpack.  Maya villages left as-is.
// Nov 06 - YP update

// Main entry point for random map script
include "mercenaries.xs";
include "ypAsianInclude.xs";
include "ypKOTHInclude.xs";

void main(void)
{
   // Text
   // These status text lines are used to manually animate the map generation progress bar
   rmSetStatusText("",0.01);

// ***************** CHOOSE NATIVES ******************
   int subCiv0=-1;
   int subCiv1=-1;
   int subCiv2=-1;

   	// Which natives?  1 = Maya/Apache, 2 = Navajo
	int whichNatives=-1;
	whichNatives = rmRandInt(1,2);


   if (rmAllocateSubCivs(3) == true)
   {
	   if ( whichNatives == 1 )
	   {
			// Always Maya.
			subCiv0=rmGetCivID("maya");
			if (subCiv0 >= 0)
				rmSetSubCiv(0, "maya");
				
			// And for now, always apache.
			subCiv1=rmGetCivID("apache");
			if (subCiv1 >= 0)
				rmSetSubCiv(1, "apache");

			// And for now, always apache.
			subCiv2=rmGetCivID("apache");
			if (subCiv2 >= 0)
				rmSetSubCiv(2, "apache");
	   }
	   else
	   {
			subCiv0=rmGetCivID("navajo");
			if (subCiv0 >= 0)
				rmSetSubCiv(0, "navajo");
				
			subCiv1=rmGetCivID("navajo");
			if (subCiv1 >= 0)
				rmSetSubCiv(1, "navajo");

			subCiv2=rmGetCivID("navajo");
			if (subCiv2 >= 0)
				rmSetSubCiv(2, "navajo");
	   }
	}


// ********************* MAP PARAMETERS ************************
	// Picks the map size
	//  int playerTiles=10000; old setting
	int playerTiles = 18000;
	if (cNumberNonGaiaPlayers >4)
		playerTiles = 16000;
	if (cNumberNonGaiaPlayers >6)
		playerTiles = 12000;		
	/*
   if(cMapSize == 1)
   {
      playerTiles = 18000;
      rmEchoInfo("Large map");
   }
   */

	int size=2.0*sqrt(cNumberNonGaiaPlayers*playerTiles);
	rmEchoInfo("Map size="+size+"m x "+size+"m");
	rmSetMapSize(size, size);

	// rmSetMapElevationParameters(cElevTurbulence, 0.4, 6, 0.5, 3.0);  // DAL - original
	rmSetMapElevationParameters(cElevTurbulence, 0.02, 4, 0.7, 8.0);
	rmSetMapElevationHeightBlend(1);

	// Picks a default water height
	rmSetSeaLevel(4.0);

    // Picks default terrain and water
	rmSetSeaType("Amazon River");
	rmSetBaseTerrainMix("sonora_dirt");
	rmTerrainInitialize("sonora\ground2_son", 4.0);
	rmSetMapType("sonora");
	rmSetMapType("grass");
	rmSetMapType("land");
	rmSetLightingSet("Sonora");

	// Choose mercs.
	chooseMercs();

	// Corner constraint.
	rmSetWorldCircleConstraint(false);

	int classPlayer=rmDefineClass("player");
	rmDefineClass("classHill");
	rmDefineClass("classPatch");
	rmDefineClass("starting settlement");
	rmDefineClass("startingUnit");
	rmDefineClass("classForest");
	rmDefineClass("importantItem");
	rmDefineClass("natives");
	rmDefineClass("classCliff");
	rmDefineClass("secrets");
	rmDefineClass("classNugget");
	rmDefineClass("center");
	int canyon=rmDefineClass("canyon");

	int avoidVultures=rmCreateTypeDistanceConstraint("avoids Vultures", "PropVulturePerching", 40.0);
	int avoidCanyons=rmCreateClassDistanceConstraint("avoid canyons", rmClassID("canyon"), 35.0);
	int shortAvoidCanyons=rmCreateClassDistanceConstraint("short avoid canyons", rmClassID("canyon"), 15.0);
	int veryShortAvoidCanyons=rmCreateClassDistanceConstraint("very short avoid canyons", rmClassID("canyon"), 4.0);
	int avoidNatives=rmCreateClassDistanceConstraint("avoid natives", rmClassID("natives"), 15.0);
	int shortAvoidNatives=rmCreateClassDistanceConstraint("short avoid natives", rmClassID("natives"), 10.0);
	int avoidSilver=rmCreateTypeDistanceConstraint("gold avoid gold", "Mine", 55.0);
	int shortAvoidSilver=rmCreateTypeDistanceConstraint("short gold avoid gold", "Mine", 20.0);
	int avoidImpassableLand=rmCreateTerrainDistanceConstraint("avoid impassable land", "Land", false, 10.0);
	int shortAvoidImpassableLand=rmCreateTerrainDistanceConstraint("short avoid impassable land", "Land", false, 4.0);
	int centerConstraintFar=rmCreateClassDistanceConstraint("stay away from center far", rmClassID("center"), rmXFractionToMeters(0.23));
	int centerConstraint=rmCreateClassDistanceConstraint("stay away from center", rmClassID("center"), rmXFractionToMeters(0.10));
	int forestConstraint=rmCreateClassDistanceConstraint("forest vs. forest", rmClassID("classForest"), 50.0);
	int shortForestConstraint=rmCreateClassDistanceConstraint("short forest vs. forest", rmClassID("classForest"), 10.0);
  int avoidHill = rmCreateTypeDistanceConstraint("avoid hill", "ypKingsHill", 8.0);

	int avoidResource=rmCreateTypeDistanceConstraint("resource avoid resource", "resource", 10.0);
	int shortAvoidResource=rmCreateTypeDistanceConstraint("short resource avoid resource", "resource", 4.0);
	int avoidBuzzards=rmCreateTypeDistanceConstraint("buzzard avoid buzzard", "BuzzardFlock", 70.0);
	int avoidBison=rmCreateTypeDistanceConstraint("avoid Bison", "Bison", 40);
	int avoidPronghorn=rmCreateTypeDistanceConstraint("avoid Pronghorn", "Pronghorn", 40);
	int avoidTradeRoute=rmCreateTradeRouteDistanceConstraint("trade route", 5.0);
	int longPlayerConstraint=rmCreateClassDistanceConstraint("long stay away from players", classPlayer, 40.0);
	int playerConstraint=rmCreateClassDistanceConstraint("stay away from players", classPlayer, 30.0);
	int mediumPlayerConstraint=rmCreateClassDistanceConstraint("medium stay away from players", classPlayer, 20.0);
	int shortPlayerConstraint=rmCreateClassDistanceConstraint("stay away from players short", classPlayer, 8.0);
	int avoidNugget=rmCreateClassDistanceConstraint("nugget vs. nugget long", rmClassID("classNugget"), 60.0);
	int shortAvoidNugget=rmCreateClassDistanceConstraint("nugget vs. nugget short", rmClassID("classNugget"), 8.0);
	int avoidTradeRouteSockets=rmCreateTypeDistanceConstraint("avoid Trade Socket", "sockettraderoute", 10);
	int avoidStartingUnits=rmCreateClassDistanceConstraint("objects avoid starting units", rmClassID("startingUnit"), 8.0);
	int circleConstraint=rmCreatePieConstraint("circle Constraint", 0.5, 0.5, 0, rmZFractionToMeters(0.47), rmDegreesToRadians(0), rmDegreesToRadians(360));
	int patchConstraint=rmCreateClassDistanceConstraint("patch vs. patch", rmClassID("classPatch"), 5.0);
	int avoidAll=rmCreateTypeDistanceConstraint("avoid all", "all", 7.0);

		
		// starting resources

	int TCID = rmCreateObjectDef("player TC");
		if (rmGetNomadStart())
		{
			rmAddObjectDefItem(TCID, "CoveredWagon", 1, 0.0);
		}
		else
		{
            rmAddObjectDefItem(TCID, "townCenter", 1, 0);
		}
	rmSetObjectDefMinDistance(TCID, 0.0);
	rmSetObjectDefMaxDistance(TCID, 10.0);
	rmAddObjectDefConstraint(TCID, avoidTradeRoute);
	rmAddObjectDefToClass(TCID, rmClassID("player"));
	rmAddObjectDefToClass(TCID, rmClassID("startingUnit"));

	int startingUnits = rmCreateStartingUnitsObjectDef(5.0);
	rmSetObjectDefMinDistance(startingUnits, 8.0);
	rmSetObjectDefMaxDistance(startingUnits, 12.0);
	rmAddObjectDefConstraint(startingUnits, avoidAll);
	rmAddObjectDefToClass(startingUnits, rmClassID("startingUnit"));
	rmAddObjectDefConstraint(startingUnits, avoidStartingUnits);

	int playerSilverID = rmCreateObjectDef("player silver");
	rmAddObjectDefItem(playerSilverID, "mine", 1, 0);
	rmAddObjectDefConstraint(playerSilverID, avoidTradeRoute);
	rmSetObjectDefMinDistance(playerSilverID, 20.0);
	rmSetObjectDefMaxDistance(playerSilverID, 25.0);
	//rmAddObjectDefConstraint(playerSilverID, avoidAll);
	rmAddObjectDefToClass(playerSilverID, rmClassID("startingUnit"));
	rmAddObjectDefConstraint(playerSilverID, avoidImpassableLand);
	rmAddObjectDefConstraint(playerSilverID, avoidStartingUnits);

	int StartAreaTreeID=rmCreateObjectDef("starting trees");
	rmAddObjectDefItem(StartAreaTreeID, "TreeSonora", rmRandInt(7,10), 4.0);
	rmAddObjectDefItem(StartAreaTreeID, "UnderbrushDesert", rmRandInt(4,6), 4.0);
	rmSetObjectDefMinDistance(StartAreaTreeID, 8);
	rmSetObjectDefMaxDistance(StartAreaTreeID, 15);
	rmAddObjectDefToClass(StartAreaTreeID, rmClassID("startingUnit"));
	rmAddObjectDefConstraint(StartAreaTreeID, avoidImpassableLand);
	rmAddObjectDefConstraint(StartAreaTreeID, avoidTradeRoute);
	//rmAddObjectDefConstraint(StartAreaTreeID, shortAvoidSilver);
	rmAddObjectDefConstraint(StartAreaTreeID, avoidStartingUnits);
	//rmAddObjectDefConstraint(StartAreaTreeID, avoidResource);

	int StartTurkeyID=rmCreateObjectDef("starting berries");
	rmAddObjectDefItem(StartTurkeyID, "berrybush", 4, 4.0);
	//rmSetObjectDefCreateHerd(StartTurkeyID, true);
	rmSetObjectDefMinDistance(StartTurkeyID, 10);
	rmSetObjectDefMaxDistance(StartTurkeyID, 18);
	rmAddObjectDefToClass(StartTurkeyID, rmClassID("startingUnit"));
	rmAddObjectDefConstraint(StartTurkeyID, avoidImpassableLand);
	rmAddObjectDefConstraint(StartTurkeyID, avoidTradeRoute);
	rmAddObjectDefConstraint(StartTurkeyID, avoidStartingUnits);
	rmAddObjectDefConstraint(StartTurkeyID, avoidResource);


	
// Text
	rmSetStatusText("",0.2);
	int canyonConstraint=rmCreateClassDistanceConstraint("canyons start away from each other", canyon, 5.0);

	int failCount=0;
	int numTries=cNumberNonGaiaPlayers*15;



// Text
	rmSetStatusText("",0.6);

	// ************************* TRADE ROUTE **********************
   int socketID=rmCreateObjectDef("sockets to dock Trade Posts");
	rmAddObjectDefItem(socketID, "SocketTradeRoute", 1, 0.0);
	rmSetObjectDefAllowOverlap(socketID, true);
	rmSetObjectDefMinDistance(socketID, 0.0);
	rmSetObjectDefMaxDistance(socketID, 6.0);
	rmAddObjectDefConstraint(socketID, shortAvoidCanyons);

	int tradeRouteID = rmCreateTradeRoute();
	rmSetObjectDefTradeRouteID(socketID, tradeRouteID);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.7, 0.0);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.65, 0.21);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.60, 0.31);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.36, 0.35);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.30, 0.50); // center
	rmAddTradeRouteWaypoint(tradeRouteID, 0.36, 0.66);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.60, 0.70);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.61, 0.75);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.65, 0.90);
	rmAddTradeRouteWaypoint(tradeRouteID, 0.60, 1.0);
	bool placedTradeRouteA = rmBuildTradeRoute(tradeRouteID, "dirt");
	if(placedTradeRouteA == false)
		rmEchoError("Failed to place trade route");


	rmPlaceObjectDefAtLoc(socketID, 0, 0.63, 0.21);
//	rmPlaceObjectDefAtLoc(socketID, 0, 0.60, 0.31);
	rmPlaceObjectDefAtLoc(socketID, 0, 0.36, 0.35);
	rmPlaceObjectDefAtLoc(socketID, 0, 0.30, 0.50);
	rmPlaceObjectDefAtLoc(socketID, 0, 0.36, 0.66);
//	rmPlaceObjectDefAtLoc(socketID, 0, 0.61, 0.75);
	rmPlaceObjectDefAtLoc(socketID, 0, 0.62, 0.85);
	

	// Create a center area.
	int centerID=rmCreateArea("center");
	rmSetAreaSize(centerID, 0.05, 0.05);
	rmSetAreaLocation(centerID, 0.5, 0.5);
	rmSetAreaCoherence(centerID, 1.0);
	rmAddAreaToClass(centerID, rmClassID("center"));

		// ****************************** PLACE PLAYERS ******************************

	int teamZeroCount = rmGetNumberPlayersOnTeam(0);
	int teamOneCount = rmGetNumberPlayersOnTeam(1);
// 2 team and FFA support
	float OneVOnePlacement=rmRandFloat(0, 1);
	if (cNumberNonGaiaPlayers == 2)
	{
		if ( OneVOnePlacement < 0.5)
		{
			rmSetPlacementTeam(0);
			rmPlacePlayersLine(0.42, 0.15, 0.46, 0.15, 0, 0.05);

			rmSetPlacementTeam(1);
			rmPlacePlayersLine(0.40, 0.87, 0.44, 0.87, 0, 0.05);
		}
		else
		{
			rmSetPlacementTeam(0);
			rmPlacePlayersLine(0.40, 0.87, 0.44, 0.87, 0, 0.05);

			rmSetPlacementTeam(1);
			rmPlacePlayersLine(0.42, 0.15, 0.46, 0.15, 0, 0.05);
		}
	}
	else if ( cNumberTeams <= 2 && teamZeroCount <= 4 && teamOneCount <= 4)
	{
		rmSetPlacementTeam(0);
		rmSetPlacementSection(0.35, 0.6); // 0.5
		rmSetTeamSpacingModifier(0.25);
		rmPlacePlayersCircular(0.38, 0.40, 0);

		rmSetPlacementTeam(1);
		rmSetPlacementSection(0.85, 0.10); // 0.5
		rmSetTeamSpacingModifier(0.25);
		rmPlacePlayersCircular(0.38, 0.40, 0);
	}
	else
	{	
		rmSetTeamSpacingModifier(0.7);
		rmPlacePlayersCircular(0.42, 0.44, 0.0);
	}

/*
if ( cNumberTeams == 2 )
	{
		rmSetPlacementTeam(0);
		rmPlacePlayersLine(0.60, 0.10, 0.65, 0.1, 0, 0.002);
	
		rmSetPlacementTeam(1);
		rmPlacePlayersLine(0.50, 0.9, 0.55, 0.9, 0, 0.002);
	}
	else
	{
  		rmSetPlacementSection(0.0, 0.95);
		rmPlacePlayersCircular(0.35, 0.40, 0.0);
	}
*/
   // Set up player areas.
   float playerFraction=rmAreaTilesToFraction(100);
   for(i=1; <cNumberPlayers)
   {
      // Create the area.
      int id=rmCreateArea("Player"+i);
      // Assign to the player.
      rmSetPlayerArea(i, id);
      // Set the size.
      rmSetAreaSize(id, playerFraction, playerFraction);
      rmAddAreaToClass(id, classPlayer);
      rmSetAreaMinBlobs(id, 1);
      rmSetAreaMaxBlobs(id, 1);
//      rmAddAreaConstraint(id, playerConstraint); 
//      rmAddAreaConstraint(id, playerEdgeConstraint); 
      rmSetAreaLocPlayer(id, i);
	  rmSetAreaTerrainType(id, "sonora\ground7_son");
      rmSetAreaWarnFailure(id, false);
   }
	// Build the areas.
   rmBuildAllAreas();

	
	// Text
	rmSetStatusText("",0.8);


	// ******************************* UBER MINE AT CENTER **************************************
	float centerType = rmRandFloat(0.0, 1.0);
  // check for KOTH game mode
  if(rmGetIsKOTH()) {
    
    int randLoc = rmRandInt(1,3);
    float xLoc = 0.5;
    float yLoc = 0.5;
    float walk = 0.05;
    
    ypKingsHillPlacer(xLoc, yLoc, walk, shortAvoidCanyons);
    rmEchoInfo("XLOC = "+xLoc);
    rmEchoInfo("XLOC = "+yLoc);
  }
   
  else if (centerType <= 0.5)
	{
	int uberMineType = rmRandInt(1,3);
	int uberMineID = -1;
	uberMineID = rmCreateGrouping("ubermine","sonoramegamine_0"+uberMineType);
	rmSetGroupingMinDistance(uberMineID, 0.0);
	rmSetGroupingMaxDistance(uberMineID, 0.0);
	rmAddGroupingToClass(uberMineID, rmClassID("canyon"));
	rmPlaceGroupingAtLoc(uberMineID, 0, 0.55, 0.50);
	}
	else if (centerType <= 0.8)
	{
		if (subCiv2 == rmGetCivID("Apache")) // RANDOM CENTER NATIVES
		{   
			int apacheVillage2ID = -1;
			int apacheVillage2Type = rmRandInt(1,3); //rmRandInt(1,9);
			apacheVillage2ID = rmCreateGrouping("apache village2", "native apache village "+apacheVillage2Type);
			rmSetGroupingMinDistance(apacheVillage2ID, 0.0);
			rmSetGroupingMaxDistance(apacheVillage2ID, 0.0);
			rmAddGroupingToClass(apacheVillage2ID, rmClassID("natives"));
			rmAddGroupingToClass(apacheVillage2ID, rmClassID("canyon"));
			rmAddGroupingToClass(apacheVillage2ID, rmClassID("importantItem"));
			rmPlaceGroupingAtLoc(apacheVillage2ID, 0, 0.55, 0.55);
		}
		else
		{
				int navajoVillage2ID = -1;
				int navajoVillage2Type = rmRandInt(1,3); //rmRandInt(1,9);
				navajoVillage2ID = rmCreateGrouping("navajo village 2", "native navajo village "+navajoVillage2Type);
				rmSetGroupingMinDistance(navajoVillage2ID, 0.0);
				rmSetGroupingMaxDistance(navajoVillage2ID, 0.0);
				rmAddGroupingToClass(navajoVillage2ID, rmClassID("natives"));
				rmAddGroupingToClass(navajoVillage2ID, rmClassID("canyon"));
				rmAddGroupingToClass(navajoVillage2ID, rmClassID("importantItem"));
				rmPlaceGroupingAtLoc(navajoVillage2ID, 0, 0.55, 0.55);
		}
	}
	//    *********************************** PLACE NATIVES ***********************************

	int randomPlacement = -1;
	if (subCiv0 == rmGetCivID("maya"))
	{  
		int mayaVillageID = -1;
		int mayaVillageType = rmRandInt(1,5);
		randomPlacement = rmRandInt(1,10);
		mayaVillageID = rmCreateGrouping("maya village", "native maya village "+mayaVillageType);
		rmSetGroupingMinDistance(mayaVillageID, 0.0);
		rmSetGroupingMaxDistance(mayaVillageID, 20.0);
		rmAddGroupingConstraint(mayaVillageID, avoidImpassableLand);
		rmAddGroupingToClass(mayaVillageID, rmClassID("natives"));
		rmAddGroupingToClass(mayaVillageID, rmClassID("importantItem"));
			if (randomPlacement <= 5)
			rmPlaceGroupingAtLoc(mayaVillageID, 0, 0.82, 0.5);
			else
			rmPlaceGroupingAtLoc(mayaVillageID, 0, 0.82, 0.5);
	}
	else
	{
		int navajoVillageID = -1;
		int navajoVillageType = rmRandInt(1,5);
		randomPlacement = rmRandInt(1,10);
		navajoVillageID = rmCreateGrouping("navajo village", "native navajo village "+navajoVillageType);
		rmSetGroupingMinDistance(navajoVillageID, 0.0);
		rmSetGroupingMaxDistance(navajoVillageID, 20.0);
		rmAddGroupingConstraint(navajoVillageID, avoidImpassableLand);
		rmAddGroupingToClass(navajoVillageID, rmClassID("natives"));
		rmAddGroupingToClass(navajoVillageID, rmClassID("importantItem"));
			if (randomPlacement <= 5)
			rmPlaceGroupingAtLoc(navajoVillageID, 0, 0.82, 0.5);
			else
			rmPlaceGroupingAtLoc(navajoVillageID, 0, 0.82, 0.5);
	}

	if (subCiv1 == rmGetCivID("apache"))
	{
		int apacheVillageID = -1;
		int apacheVillageType = rmRandInt(1,5);
		apacheVillageID = rmCreateGrouping("apache village", "native apache village "+apacheVillageType);
		rmSetGroupingMinDistance(apacheVillageID, 0.0);
		rmSetGroupingMaxDistance(apacheVillageID, 20.0);
		rmAddGroupingConstraint(apacheVillageID, avoidImpassableLand);
		rmAddGroupingToClass(apacheVillageID, rmClassID("natives"));
		rmPlaceGroupingAtLoc(apacheVillageID, 0, 0.18, 0.5);
	}
	else
	{
		int navajoVillage3ID = -1;
		int navajoVillage3Type = rmRandInt(1,5);
		navajoVillage3ID = rmCreateGrouping("navajo village 3", "native navajo village "+navajoVillageType);
		rmSetGroupingMinDistance(navajoVillage3ID, 0.0);
		rmSetGroupingMaxDistance(navajoVillage3ID, 20.0);
		rmAddGroupingConstraint(navajoVillage3ID, avoidImpassableLand);
		rmAddGroupingToClass(navajoVillage3ID, rmClassID("natives"));
		rmPlaceGroupingAtLoc(navajoVillage3ID, 0, 0.18, 0.5);
	}

//**************************************** cliff embellishments *********************************************
	for(i=0; <numTries)
	{
		int cliffHeight=rmRandInt(0,10);
		int mesaID=rmCreateArea("mesa"+i);
		rmSetAreaSize(mesaID, rmAreaTilesToFraction(2), rmAreaTilesToFraction(200));  // used to be 300
		rmSetAreaWarnFailure(mesaID, false);
		rmSetAreaCliffType(mesaID, "Sonora");
		rmAddAreaToClass(mesaID, rmClassID("canyon"));	// Attempt to keep cliffs away from each other.
		rmSetAreaCliffEdge(mesaID, 1, 1.0, 0.1, 1.0, 0);
		if (cliffHeight <= 5)
		rmSetAreaCliffHeight(mesaID, rmRandInt(4,10), 1.0, 1.0);
		else
		rmSetAreaCliffHeight(mesaID, -15, 1.0, 1.0);
		rmAddAreaConstraint(mesaID, avoidCanyons);
		rmAddAreaConstraint(mesaID, avoidNatives);
		rmSetAreaMinBlobs(mesaID, 3);
		rmSetAreaMaxBlobs(mesaID, 5);
		rmSetAreaMinBlobDistance(mesaID, 3.0);
		rmSetAreaMaxBlobDistance(mesaID, 5.0);
		rmSetAreaCoherence(mesaID, 0.5);
		rmAddAreaConstraint(mesaID, playerConstraint); 
		rmAddAreaConstraint(mesaID, avoidTradeRouteSockets);
		rmAddAreaConstraint(mesaID, avoidTradeRoute);
		rmAddAreaConstraint(mesaID, shortAvoidSilver);
    rmAddAreaConstraint(mesaID, avoidHill);
		if(rmBuildArea(mesaID)==false)
		{
			// Stop trying once we fail 3 times in a row.
			failCount++;
			if(failCount==2)
				break;
		}
		else
			failCount=0;
	}
// small cliffs
for(i=0; <numTries)
	{
		int smallCliffHeight=rmRandInt(0,10);
		int smallMesaID=rmCreateArea("small mesa"+i);
		rmSetAreaSize(smallMesaID, rmAreaTilesToFraction(4), rmAreaTilesToFraction(8));  // used to be 300
		rmSetAreaWarnFailure(smallMesaID, false);
		rmSetAreaCliffType(smallMesaID, "Sonora");
		rmAddAreaToClass(smallMesaID, rmClassID("canyon"));	// Attempt to keep cliffs away from each other.
		rmSetAreaCliffEdge(smallMesaID, 1, 1.0, 0.1, 1.0, 0);
		rmSetAreaCliffHeight(smallMesaID, rmRandInt(6,8), 1.0, 1.0);
		rmAddAreaConstraint(smallMesaID, shortAvoidCanyons);
		rmSetAreaMinBlobs(smallMesaID, 3);
		rmSetAreaMaxBlobs(smallMesaID, 5);
		rmSetAreaMinBlobDistance(smallMesaID, 3.0);
		rmSetAreaMaxBlobDistance(smallMesaID, 5.0);
		rmSetAreaCoherence(smallMesaID, 0.3);
		rmAddAreaConstraint(smallMesaID, mediumPlayerConstraint); 
		rmAddAreaConstraint(smallMesaID, avoidNatives); 
		rmAddAreaConstraint(smallMesaID, avoidTradeRouteSockets);
		rmAddAreaConstraint(smallMesaID, avoidTradeRoute);
		rmAddAreaConstraint(smallMesaID, shortAvoidSilver);
    rmAddAreaConstraint(smallMesaID, avoidHill);
		if(rmBuildArea(smallMesaID)==false)
		{
			// Stop trying once we fail 3 times in a row.
			failCount++;
			if(failCount==20)
				break;
		}
		else
			failCount=0;
	}

	for(i=1; <cNumberPlayers)
	{
		
		// Test of Marcin's Starting Units stuff...
		rmPlaceObjectDefAtLoc(TCID, i, rmPlayerLocXFraction(i), rmPlayerLocZFraction(i));
    
		rmPlaceObjectDefAtLoc(startingUnits, i, rmPlayerLocXFraction(i), rmPlayerLocZFraction(i));
		rmPlaceObjectDefAtLoc(playerSilverID, 0, rmPlayerLocXFraction(i), rmPlayerLocZFraction(i));
		rmPlaceObjectDefAtLoc(StartTurkeyID, 0, rmPlayerLocXFraction(i), rmPlayerLocZFraction(i));
		rmPlaceObjectDefAtLoc(StartAreaTreeID, 0, rmPlayerLocXFraction(i), rmPlayerLocZFraction(i));
		
    vector TCLocation=rmGetUnitPosition(rmGetUnitPlacedOfPlayer(TCID, i));
    
    if(ypIsAsian(i) && rmGetNomadStart() == false)
      rmPlaceObjectDefAtLoc(ypMonasteryBuilder(i), i, rmXMetersToFraction(xsVectorGetX(TCLocation)), rmZMetersToFraction(xsVectorGetZ(TCLocation)));
    
		//vector closestPoint=rmGetUnitPosition(rmGetUnitPlacedOfPlayer(startingUnits, i));
		//rmSetHomeCityGatherPoint(i, closestPoint);
	}


// ************************************ FORESTS **************************************


	for(i=0; <cNumberNonGaiaPlayers*15)
	{
		int edgeForestID=rmCreateArea("edgeForest"+i);
		rmSetAreaWarnFailure(edgeForestID, false);
		rmSetAreaSize(edgeForestID, rmAreaTilesToFraction(200), rmAreaTilesToFraction(300));
		rmSetAreaForestType(edgeForestID, "sonora forest");
		rmSetAreaForestDensity(edgeForestID, 1.0);
		rmSetAreaForestClumpiness(edgeForestID, 1.0);		
		rmSetAreaForestUnderbrush(edgeForestID, 0.0);
		rmAddAreaToClass(edgeForestID, rmClassID("classForest"));
		rmSetAreaMinBlobs(edgeForestID, 1);
		rmSetAreaMaxBlobs(edgeForestID, 3);					
		rmSetAreaMinBlobDistance(edgeForestID, 16.0);
		rmSetAreaMaxBlobDistance(edgeForestID, 30.0);
		rmSetAreaCoherence(edgeForestID, 0.6);
		rmSetAreaSmoothDistance(edgeForestID, 10);
		rmAddAreaConstraint(edgeForestID, shortPlayerConstraint);
		rmAddAreaConstraint(edgeForestID, forestConstraint); 
		rmAddAreaConstraint(edgeForestID, shortAvoidSilver);  
		rmAddAreaConstraint(edgeForestID, canyonConstraint); 
		rmAddAreaConstraint(edgeForestID, avoidTradeRoute);
		rmAddAreaConstraint(edgeForestID, avoidTradeRouteSockets);
		rmAddAreaConstraint(edgeForestID, avoidStartingUnits);
		rmAddAreaConstraint(edgeForestID, avoidNatives);
    rmAddAreaConstraint(edgeForestID, avoidHill);
			if(rmBuildArea(edgeForestID)==false)
			{
				// Stop trying once we fail 5 times in a row.  
				failCount++;
				if(failCount==5)
					break;
			}
			else
				failCount=0; 
	}

	// COIN RESOURCES

	int silverType = rmRandInt(1,10);
	int silverID = -1;
	int silverCount = cNumberNonGaiaPlayers*3;	
	rmEchoInfo("silver count = "+silverCount);

	for(i=0; < silverCount)
	{
		silverID = rmCreateObjectDef("silver"+i);
		rmAddObjectDefItem(silverID, "mine", 1, 0);
		rmSetObjectDefMinDistance(silverID, 0.0);
		rmSetObjectDefMaxDistance(silverID, rmXFractionToMeters(0.45));
		rmAddObjectDefConstraint(silverID, avoidImpassableLand);
		rmAddObjectDefConstraint(silverID, canyonConstraint);
		rmAddObjectDefConstraint(silverID, avoidSilver);
		rmAddObjectDefConstraint(silverID, mediumPlayerConstraint);
		rmAddObjectDefConstraint(silverID, shortForestConstraint);
		rmAddObjectDefConstraint(silverID, avoidNatives);
		rmAddObjectDefConstraint(silverID, avoidTradeRouteSockets);
		rmPlaceObjectDefAtLoc(silverID, 0, 0.5, 0.5);

	}

	
	for(i=0; < 10*cNumberNonGaiaPlayers)
	{
		int randomTreeID=rmCreateObjectDef("random tree "+i);
		rmAddObjectDefItem(randomTreeID, "TreeSonora", rmRandInt(1,4), 3.0);
		rmSetObjectDefMinDistance(randomTreeID, 0.0);
		rmSetObjectDefMaxDistance(randomTreeID, rmXFractionToMeters(0.3));
		rmAddObjectDefConstraint(randomTreeID, avoidResource);
		rmAddObjectDefConstraint(randomTreeID, shortAvoidImpassableLand);
		rmAddObjectDefConstraint(randomTreeID, avoidTradeRouteSockets);
		rmAddObjectDefConstraint(randomTreeID, shortPlayerConstraint);
		rmAddObjectDefConstraint(randomTreeID, avoidStartingUnits);
    rmAddObjectDefConstraint(randomTreeID, avoidHill);
		rmPlaceObjectDefAtLoc(randomTreeID, 0, 0.5, 0.5);
   }


	// ************************************ TREASURES ***********************************

		int nugget1= rmCreateObjectDef("nugget easy"); 
		rmAddObjectDefItem(nugget1, "Nugget", 1, 0.0);
		rmSetNuggetDifficulty(1, 1);
		rmAddObjectDefToClass(nugget1, rmClassID("classNugget"));
		rmAddObjectDefConstraint(nugget1, avoidResource);
		rmAddObjectDefConstraint(nugget1, shortAvoidImpassableLand);
		rmAddObjectDefConstraint(nugget1, avoidTradeRouteSockets);
		rmAddObjectDefConstraint(nugget1, avoidTradeRoute);
		rmAddObjectDefConstraint(nugget1, mediumPlayerConstraint);
		rmAddObjectDefConstraint(nugget1, avoidStartingUnits);
		rmAddObjectDefConstraint(nugget1, shortAvoidCanyons);
		rmAddObjectDefConstraint(nugget1, avoidNugget);
		rmAddObjectDefConstraint(nugget1, avoidNatives);
		rmAddObjectDefConstraint(nugget1, circleConstraint);
		rmSetObjectDefMinDistance(nugget1, 40.0);
		rmSetObjectDefMaxDistance(nugget1, 60.0);
		rmPlaceObjectDefPerPlayer(nugget1, false, 2);

		int nugget2= rmCreateObjectDef("nugget medium"); 
		rmAddObjectDefItem(nugget2, "Nugget", 1, 0.0);
		rmSetNuggetDifficulty(2, 2);
		rmAddObjectDefToClass(nugget2, rmClassID("classNugget"));
		rmSetObjectDefMinDistance(nugget2, 0.0);
		rmSetObjectDefMaxDistance(nugget2, rmXFractionToMeters(0.5));
		rmAddObjectDefConstraint(nugget2, avoidResource);
		rmAddObjectDefConstraint(nugget2, shortAvoidImpassableLand);
		rmAddObjectDefConstraint(nugget2, avoidTradeRouteSockets);
		rmAddObjectDefConstraint(nugget2, avoidTradeRoute);
		rmAddObjectDefConstraint(nugget2, mediumPlayerConstraint);
		rmAddObjectDefConstraint(nugget2, shortAvoidCanyons);
		rmAddObjectDefConstraint(nugget2, avoidStartingUnits);
		rmAddObjectDefConstraint(nugget2, avoidNugget);
		rmAddObjectDefConstraint(nugget2, avoidNatives);
		rmAddObjectDefConstraint(nugget2, circleConstraint);
		rmSetObjectDefMinDistance(nugget2, 80.0);
		rmSetObjectDefMaxDistance(nugget2, 120.0);
		rmPlaceObjectDefPerPlayer(nugget2, false, 1);

		int nugget3= rmCreateObjectDef("nugget hard"); 
		rmAddObjectDefItem(nugget3, "Nugget", 1, 0.0);
		rmSetNuggetDifficulty(3, 3);
		rmAddObjectDefToClass(nugget3, rmClassID("classNugget"));
		rmSetObjectDefMinDistance(nugget3, 0.0);
		rmSetObjectDefMaxDistance(nugget3, rmXFractionToMeters(0.4));
		rmAddObjectDefConstraint(nugget3, avoidResource);
		rmAddObjectDefConstraint(nugget3, shortAvoidImpassableLand);
		rmAddObjectDefConstraint(nugget3, avoidTradeRouteSockets);
		rmAddObjectDefConstraint(nugget3, avoidTradeRoute);
		rmAddObjectDefConstraint(nugget3, mediumPlayerConstraint);
		rmAddObjectDefConstraint(nugget3, shortAvoidCanyons);
		rmAddObjectDefConstraint(nugget3, avoidStartingUnits);
		rmAddObjectDefConstraint(nugget3, avoidNugget);
		rmAddObjectDefConstraint(nugget3, avoidNatives);
		rmAddObjectDefConstraint(nugget3, circleConstraint);
		rmPlaceObjectDefAtLoc(nugget3, 0, 0.5, 0.5, cNumberNonGaiaPlayers);

		int nugget4= rmCreateObjectDef("nugget nuts"); 
		rmAddObjectDefItem(nugget4, "Nugget", 1, 0.0);
		rmSetNuggetDifficulty(4, 4);
		rmAddObjectDefToClass(nugget4, rmClassID("classNugget"));
		rmSetObjectDefMinDistance(nugget4, 0.0);
		rmSetObjectDefMaxDistance(nugget4, rmXFractionToMeters(0.4));
		rmAddObjectDefConstraint(nugget4, avoidResource);
		rmAddObjectDefConstraint(nugget4, shortAvoidImpassableLand);
		rmAddObjectDefConstraint(nugget4, avoidTradeRouteSockets);
		rmAddObjectDefConstraint(nugget4, avoidTradeRoute);
		rmAddObjectDefConstraint(nugget4, mediumPlayerConstraint);
		rmAddObjectDefConstraint(nugget4, shortAvoidCanyons);
		rmAddObjectDefConstraint(nugget4, avoidStartingUnits);
		rmAddObjectDefConstraint(nugget4, avoidNugget);
		rmAddObjectDefConstraint(nugget4, avoidNatives);
		rmAddObjectDefConstraint(nugget4, circleConstraint);
		rmPlaceObjectDefAtLoc(nugget4, 0, 0.5, 0.5, rmRandInt(0,3));

/*
	int nuggetID=rmCreateObjectDef("nugget"); 
	rmAddObjectDefItem(nuggetID, "Nugget", 1, 2.0);
	rmSetObjectDefMinDistance(nuggetID, 0.0);
	rmSetObjectDefMaxDistance(nuggetID, rmXFractionToMeters(0.49));
	rmAddObjectDefConstraint(nuggetID, avoidImpassableLand);
	rmAddObjectDefToClass(nuggetID, rmClassID("classNugget"));
	rmAddObjectDefConstraint(nuggetID, avoidNugget);
	rmAddObjectDefConstraint(nuggetID, shortAvoidSilver);
//	rmAddObjectDefConstraint(nuggetID, avoidResource);
	rmAddObjectDefConstraint(nuggetID, canyonConstraint);
	rmAddObjectDefConstraint(nuggetID, avoidTradeRoute);
	rmAddObjectDefConstraint(nuggetID, shortForestConstraint);
	rmAddObjectDefConstraint(nuggetID, circleConstraint);
	rmAddObjectDefConstraint(nuggetID, playerConstraint);
	rmAddObjectDefConstraint(nuggetID, avoidNatives);
	rmPlaceObjectDefAtLoc(nuggetID, 0, 0.5, 0.5, 6*cNumberNonGaiaPlayers);
*/
	int buzzardFlockID=rmCreateObjectDef("buzzards");
	rmAddObjectDefItem(buzzardFlockID, "BuzzardFlock", 1, 3.0);
	rmSetObjectDefMinDistance(buzzardFlockID, 0.0);
	rmSetObjectDefMaxDistance(buzzardFlockID, rmXFractionToMeters(0.3));
	rmAddObjectDefConstraint(buzzardFlockID, avoidBuzzards);
	rmAddObjectDefConstraint(buzzardFlockID, avoidTradeRouteSockets);
	rmAddObjectDefConstraint(buzzardFlockID, avoidImpassableLand);
	rmAddObjectDefConstraint(buzzardFlockID, playerConstraint);
	rmPlaceObjectDefAtLoc(buzzardFlockID, 0, 0.5, 0.5, 2*cNumberNonGaiaPlayers);

	int randomVultureTreeID=rmCreateObjectDef("random vulture tree");
	rmAddObjectDefItem(randomVultureTreeID, "PropVulturePerching", 1, 3.0);
	rmSetObjectDefMinDistance(randomVultureTreeID, 0.0);
	rmSetObjectDefMaxDistance(randomVultureTreeID, rmXFractionToMeters(0.40));
    rmAddObjectDefConstraint(randomVultureTreeID, avoidImpassableLand);
	rmAddObjectDefConstraint(randomVultureTreeID, playerConstraint);
	rmAddObjectDefConstraint(randomVultureTreeID, avoidVultures);
	rmAddObjectDefConstraint(randomVultureTreeID, avoidTradeRoute);
	rmAddObjectDefConstraint(randomVultureTreeID, avoidTradeRouteSockets);
	rmPlaceObjectDefAtLoc(randomVultureTreeID, 0, 0.5, 0.5, 1.5*cNumberNonGaiaPlayers);

	if (cNumberNonGaiaPlayers<5)
		int bisonCount =4*cNumberNonGaiaPlayers;
	else
		bisonCount =2*cNumberNonGaiaPlayers;

	int bisonID=rmCreateObjectDef("Bison herd");
	rmAddObjectDefItem(bisonID, "bison", rmRandInt(6,8), 6.0);
	rmSetObjectDefCreateHerd(bisonID, true);
	rmSetObjectDefMinDistance(bisonID, 0.0);
	rmSetObjectDefMaxDistance(bisonID, rmXFractionToMeters(0.40));
	rmAddObjectDefConstraint(bisonID, shortAvoidResource);
	rmAddObjectDefConstraint(bisonID, centerConstraint);
	rmAddObjectDefConstraint(bisonID, avoidImpassableLand);
	rmAddObjectDefConstraint(bisonID, avoidBison);
	rmAddObjectDefConstraint(bisonID, shortAvoidNatives);
	rmAddObjectDefConstraint(bisonID, veryShortAvoidCanyons);
	rmAddObjectDefConstraint(bisonID, avoidPronghorn);
	rmAddObjectDefConstraint(bisonID, shortAvoidNugget);
	rmAddObjectDefConstraint(bisonID, mediumPlayerConstraint);
	rmAddObjectDefConstraint(bisonID, avoidTradeRouteSockets);
	rmPlaceObjectDefAtLoc(bisonID, 0, 0.5, 0.5, bisonCount);

	if (cNumberNonGaiaPlayers<5)
		int pronghornCount =4*cNumberNonGaiaPlayers;
	else
		pronghornCount =2.25*cNumberNonGaiaPlayers;

	int pronghornID=rmCreateObjectDef("pronghorn herd");
	rmAddObjectDefItem(pronghornID, "Pronghorn", rmRandInt(8, 10), 6.0);
	rmSetObjectDefCreateHerd(pronghornID, true);
	rmSetObjectDefMinDistance(pronghornID, 0.0);
	rmSetObjectDefMaxDistance(pronghornID, rmXFractionToMeters(0.40));
	rmAddObjectDefConstraint(pronghornID, shortAvoidResource);
	rmAddObjectDefConstraint(pronghornID, centerConstraint);
	rmAddObjectDefConstraint(pronghornID, avoidImpassableLand);
	rmAddObjectDefConstraint(pronghornID, shortAvoidNatives);
	rmAddObjectDefConstraint(pronghornID, veryShortAvoidCanyons);
	rmAddObjectDefConstraint(pronghornID, avoidPronghorn);
	rmAddObjectDefConstraint(pronghornID, shortAvoidNugget);
	rmAddObjectDefConstraint(pronghornID, mediumPlayerConstraint);
	rmAddObjectDefConstraint(pronghornID, avoidBison);
	rmAddObjectDefConstraint(pronghornID, avoidTradeRouteSockets);
	rmPlaceObjectDefAtLoc(pronghornID, 0, 0.5, 0.5, pronghornCount);

	for (i=0; <10)
      {
		int dirtPatch=rmCreateArea("open dirt patch "+i);
		rmSetAreaWarnFailure(dirtPatch, false);
		rmSetAreaSize(dirtPatch, rmAreaTilesToFraction(200), rmAreaTilesToFraction(300));
		rmSetAreaTerrainType(dirtPatch, "sonora\ground7_son");
		// rmAddAreaTerrainLayer(dirtPatch, "great_plains\ground2_gp", 0, 1);
		rmAddAreaToClass(dirtPatch, rmClassID("classPatch"));
		//rmSetAreaBaseHeight(dirtPatch, 4.0);
		rmSetAreaMinBlobs(dirtPatch, 1);
		rmSetAreaMaxBlobs(dirtPatch, 5);
		rmSetAreaMinBlobDistance(dirtPatch, 16.0);
		rmSetAreaMaxBlobDistance(dirtPatch, 40.0);
		rmSetAreaCoherence(dirtPatch, 0.0);
		rmSetAreaSmoothDistance(dirtPatch, 10);
		rmAddAreaConstraint(dirtPatch, shortAvoidImpassableLand);
		rmAddAreaConstraint(dirtPatch, patchConstraint);
		rmBuildArea(dirtPatch); 
      }


		// Text
	rmSetStatusText("",1.0);
	}
}  
