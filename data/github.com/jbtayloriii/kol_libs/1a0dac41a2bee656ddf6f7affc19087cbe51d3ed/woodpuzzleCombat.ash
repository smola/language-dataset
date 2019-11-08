print("imported woodpuzzleCombat.ash");

int freeRuns = 0;

int NOVELTY_TROPICAL_SKELETON_ID = $monster[novelty tropical skeleton].id;

boolean [location] kgbBanishLocations;
boolean [location] snokebombBanishLocations;

//Constants
boolean [skill] oneTimeSkills = $skills[Entangling Noodles, Extract, Digitize, Compress];
boolean [item] oneTimeItems = $items[time-spinner];

boolean verbose = true;

boolean getSkillFlag(string name) {
	return to_boolean(to_int(get_property("cb_skill_" + name)));
}

void setSkillFlag(string name, boolean val) {
	set_property("cb_skill_" + name, to_int(val));
}

boolean getItemFlag(string name) {
	return to_boolean(to_int(get_property("cb_item_" + name)));
}

void setItemFlag(string name, boolean val) {
	set_property("cb_item_" + name, to_int(val));
}

/////////////////////////////////////////////

void cb_initCombat() {
	//initialize once per combat skills
	foreach skl in oneTimeSkills {
		setSkillFlag(skl, have_skill(skl));
	}
	
	//initialize once per combat items
	foreach it in oneTimeItems {
		setItemFlag(it, item_amount(it) > 0);		
	}
}

/////////////////////////////////////////////

boolean canCastOTSkill(skill skl) {
	return getSkillFlag(skl);
}

string castOTSkill(skill skl) {
	if(canCastOTSkill(skl)) {
		setSkillFlag(skl, false);
		return "skill " + skl;	
	}
	abort("error casting skill " + skl);
	return ""; //won't trigger
}

boolean canUseOTItem(item it) {
	return getItemFlag(it);
}

string useOTItem(item it) {
	if(canUseOTItem(it)) {
		setItemFlag(it, false);
		return "item " + it;
	}
	abort("error using item in combat: " + it);
	return ""; //won't trigger
}


///////////////////////////////////////

void determineFight(skill [int] skills) {

	foreach index in skills {
		print(skills[index]);
	}
}

int getNextHitDamage(monster opp){
	int monsterDiff = monster_attack(opp) - my_buffedstat($stat[moxie]);
	int monsterHit = monsterDiff + max(0, .25 * (monster_attack(opp) - damage_reduction()));
	if(verbose) {
		print("Next Hit: " + monsterHit);
	}
	return monsterHit;
}

boolean wpc_canTakeHit(monster opp) {
	int nextHit = getNextHitDamage(opp);
	if(nextHit < my_hp()) {
		return true;
	} else {
		return false;
	}
}

string combatNormal(int round, monster opp, string text) {
	print("normal combat script");
	if(canUseOTItem($item[Time-Spinner])) {
		return useOTItem($item[Time-Spinner]);
	}
	
	if(canCastOTSkill($skill[Entangling Noodles])) {
		return castOTSkill($skill[Entangling Noodles]);
	}
	
	if(wpc_canTakeHit(opp) && have_skill($skill[Extract])) {
		return "skill extract";
	} else {
		return "skill saucestorm";
	}
}

string combatSkeletonStore(int round, monster opp, string text) {
	if(opp.id == NOVELTY_TROPICAL_SKELETON_ID) {
		//fight normally
		return combatNormal(round, opp, text);
	} else {
		if(!(kgbBanishLocations contains $location[the skeleton store])) {
			kgbBanishLocations[$location[the skeleton store]] = true;
			return "skill KGB tranquilizer dart";
		} else if(!(snokebombBanishLocations contains $location[the skeleton store])) {
			snokebombBanishLocations[$location[the skeleton store]] = true;
			return "skill Snokebomb";
		} else if(freeRuns < (familiar_weight($familiar[pair of stomping boots]) % 5)) {
			//run away!
			freeRuns += 1;
			return "runaway";
		} else {
			abort("trouble with skeleton fight.");
		}
	}
	return ""; //can't
}

string combatHaikuDungeon(int round, monster opp, string text) {
	if(opp == $monster[amateur ninja]) {
		//fight normally
		return combatNormal(round, opp, text);
	} else {
		if(!(kgbBanishLocations contains $location[The Haiku Dungeon])) {
			kgbBanishLocations[$location[The Haiku Dungeon]] = true;
			return "skill KGB tranquilizer dart";
		} else if(!(snokebombBanishLocations contains $location[The Haiku Dungeon])) {
			snokebombBanishLocations[$location[The Haiku Dungeon]] = true;
			return "skill Snokebomb";
		} else if(freeRuns < (familiar_weight($familiar[pair of stomping boots]) % 5)) {
			//run away!
			freeRuns += 1;
			return "runaway";
		} else {
			abort("trouble with haiku dungeon fight.");
		}
	}
	return ""; //can't get here
}

String combatChateau(int round, monster opp, string text) {
	return "skill saucestorm";
}

string combatDigitize(int round, monster opp, string text) {
	print("digitize combat script");
	if(canCastOTSkill($skill[Digitize])) {
		return castOTSkill($skill[Digitize]);
	}
	
	if(canUseOTItem($item[Time-Spinner])) {
		return useOTItem($item[Time-Spinner]);
	}
	
	if(canCastOTSkill($skill[Entangling Noodles])) {
		return castOTSkill($skill[Entangling Noodles]);
	}
	
	if(wpc_canTakeHit(opp) && have_skill($skill[Extract])) {
		return "skill extract";
	} else {
		return "skill saucestorm";
	}
}

