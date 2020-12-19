//do guzzlr quests

import <scripts/ttpack/util/tt_util.ash>

//public prototype start
void guzzlr_settings_print();

//public prototype end

item GUZZLRBUCK = $item[Guzzlrbuck];
item GUZZLR_TABLET = $item[Guzzlr tablet];
item GUZZLR_COCKTAIL_SET = $item[Guzzlr cocktail set];
item GUZZLR_SHOES = $item[Guzzlr shoes];
item GUZZLR_PANTS = $item[Guzzlr pants];
item GUZZLR_HAT = $item[Guzzlr hat];
boolean [item] GUZZLR_COCKTAILS = $items[Steamboat, Ghiaccio Colada, Nog-on-the-Cob, Sourfinger, Buttery Boy];

void guzzlr_settings_defaults()
{
	boolean new_setting_added = false;
	
	//set defaults
	if(get_property("guzzlr_deliverBronze") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_deliverBronze", true);
	}
	if(get_property("guzzlr_maxMeatCostBronze") == "" || get_property("guzzlr_maxMeatCostBronze").to_int() < 1)
	{
		new_setting_added = true;
		set_property("guzzlr_maxMeatCostBronze", 5000);
	}
	if(get_property("guzzlr_deliverGold") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_deliverGold", true);
	}
	if(get_property("guzzlr_maxMeatCostGold") == "" || get_property("guzzlr_maxMeatCostGold").to_int() < 1)
	{
		new_setting_added = true;
		set_property("guzzlr_maxMeatCostGold", 10000);
	}
	if(get_property("guzzlr_deliverPlatinum") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_deliverPlatinum", true);
	}
	if(get_property("guzzlr_maxMeatCostPlatinum") == "" || get_property("guzzlr_maxMeatCostPlatinum").to_int() < 1)
	{
		new_setting_added = true;
		set_property("guzzlr_maxMeatCostPlatinum", 15000);
	}
	if(get_property("guzzlr_abandonTooExpensive") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_abandonTooExpensive", false);
	}
	if(get_property("guzzlr_deliverInrun") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_deliverInrun", false);
	}
	if(get_property("guzzlr_treatCasualAsAftercore") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_treatCasualAsAftercore", false);
	}
	if(get_property("guzzlr_treatPostroninAsAftercore") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_treatPostroninAsAftercore", true);
	}
	if(get_property("guzzlr_abandonPlatinumForcedAftercore") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_abandonPlatinumForcedAftercore", false);
	}
	if(get_property("guzzlr_abandonPlatinumForcedInrun") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_abandonPlatinumForcedInrun", false);
	}
	if(get_property("guzzlr_autoFamiliar") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_autoFamiliar", true);
	}
	if(get_property("guzzlr_manualFamiliar") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_manualFamiliar", false);
	}
	if(get_property("guzzlr_allowPirateQuest") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_allowPirateQuest", false);
	}
	if(get_property("guzzlr_useMafiaThumbRing") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_useMafiaThumbRing", true);
	}
	if(get_property("guzzlr_mood") == "")
	{
		new_setting_added = true;
		set_property("guzzlr_mood", "guzzlr");
	}
	
	if(new_setting_added)
	{
		guzzlr_settings_print();
		abort("Settings have been configured to default. Please verify they are correct before running me again");
	}
}

void guzzlr_settings_print()
{
	//print current settings status
	print();
	print("Current settings for guzzlr:", "blue");
	tt_printSetting("guzzlr_deliverBronze");
	tt_printSetting("guzzlr_maxMeatCostBronze");
	tt_printSetting("guzzlr_deliverGold");
	tt_printSetting("guzzlr_maxMeatCostGold");
	tt_printSetting("guzzlr_deliverPlatinum", "Platinum will not be taken if you already used your 1 per day abandon");
	tt_printSetting("guzzlr_maxMeatCostPlatinum", "The maximum allowed price for drink crafting ingredient and if needed a dayticket or access items");
	tt_printSetting("guzzlr_abandonTooExpensive", "When true will automatically abandon deliveries that are too expensive. When false will abort instead");
	tt_printSetting("guzzlr_deliverInrun", "Set to false to disable doing deliveries during a run");
	tt_printSetting("guzzlr_treatCasualAsAftercore");
	tt_printSetting("guzzlr_treatPostroninAsAftercore");
	tt_printSetting("guzzlr_abandonPlatinumForcedAftercore", "Override all other settings for the purpose of starting the day by taking a platinum delivery and immediately aborting it");
	tt_printSetting("guzzlr_abandonPlatinumForcedInrun", "Override all other settings for the purpose of starting the day by taking a platinum delivery and immediately aborting it");
	tt_printSetting("guzzlr_autoFamiliar", "Automatically switch familiar using autoscend code to IOTM familiars that still have items to drop today and when out of that to +item drop familiars");
	tt_printSetting("guzzlr_manualFamiliar", "Automatically switch to a single manually specified familiar");
	tt_printSetting("guzzlr_manualFamiliarChoice", "The name of the familiar you want to manually switch to");
	tt_printSetting("guzzlr_allowPirateQuest", "Do the pirate quest if needed to reach a gold delivery target");
	tt_printSetting("guzzlr_useMafiaThumbRing", "Use mafia thumb ring to generate extra adventures");
	tt_printSetting("guzzlr_mood", "The name of the mood you want to use with guzzlr. use [apathetic] to disable moods");
	
	print();
	print("You can make changes to these settings by typing:", "blue");
	print("set [setting_name] = [target]", "blue");
	print();
}

int [string] parseGuzzlrTablet()
{
    string desc = visit_url(`desc_item.php?whichitem={GUZZLR_TABLET.descid}`);

    matcher boozeDrop = "\\+(\\d+)% Booze Drops from Monsters".create_matcher( desc );
    matcher mpRegen = "Regenerate (\\d+)-(\\d+) MP per Adventure".create_matcher( desc );
    matcher hpRegen = "Regenerate (\\d+)-(\\d+) HP per Adventure".create_matcher( desc );

    int [string] results;

    if ( boozeDrop.find() )
    {
        results["booze_drop"] = boozeDrop.group(1).to_int();
    }
    
    if ( mpRegen.find() )
    {
        results["mp_regen_min"] = mpRegen.group(1).to_int();
        results["mp_regen_max"] = mpRegen.group(2).to_int();
    }

    if ( hpRegen.find() )
    {
        results["hp_regen_min"] = hpRegen.group(1).to_int();
        results["hp_regen_max"] = hpRegen.group(2).to_int();
    }

    return results;
}

int guzzlr_QuestTier()
{
	if(quest_unstarted("questGuzzlr"))
	{
		return 0;
	}
	
	string mafia_tier = get_property("guzzlrQuestTier");
	int my_tier = 0;
	
	if(mafia_tier == "bronze") my_tier = 1;
	if(mafia_tier == "gold") my_tier = 2;
	if(mafia_tier == "platinum") my_tier = 3;
	
	return my_tier;
}

boolean guzzlr_QuestStart(int tier)
{
	//returns true if successfully started the quest or if the quest is already started on the desired target
	if(tier > 3 || tier < 1)
	{
		abort("Error: " + tier + "is not a valid tier for boolean guzzlr_QuestStart(int tier)");
	}
	if(!quest_unstarted("questGuzzlr"))		//already started. just compare it to desired target
	{
		return tier == guzzlr_QuestTier();
	}
	
	if(tier == 3)		//platinum delivery
	{
		if(get_property("_guzzlrPlatinumDeliveries").to_int() > 0) return false;	//already started platinum quest today
		if(get_property("_guzzlrQuestAbandoned").to_boolean()) return false;		//already abandoned quest today
		if(get_property("guzzlrGoldDeliveries").to_int() < 5) return false;			//not enough gold deliveries to unlock platinum
	}
	if(tier == 2)
	{
		if(get_property("_guzzlrGoldDeliveries").to_int() > 2) return false;		//already did max gold deliveries today
		if(get_property("guzzlrBronzeDeliveries").to_int() < 5) return false;		//not enough bronze deliveries to unlock gold
	}
	
	if(tier == 1) print("Taking a Bronze delivery", "blue");
	if(tier == 2) print("Taking a Gold delivery", "blue");
	if(tier == 3) print("Taking a Platinum delivery", "blue");
	
	//choices: 2 = bronze 3 = gold 4 = platinum 5 = change your mind and not take a quest
	visit_url("inventory.php?tap=guzzlr", false);
	run_choice(tier+1);
	
	if(tier == guzzlr_QuestTier())
	{
		return true;
	}
	return false;
}

item accessItem()
{
	location goal = get_property("guzzlrQuestLocation").to_location();
	
	//The Worm Wood. you get there via a potion called [tiny bottle of absinthe] which gives 10 turns of access.
	if($locations[The Mouldering Mansion, The Rogue Windmill, The Stately Pleasure Dome] contains goal)
	{
		return $item[tiny bottle of absinthe];
	}
	
	//A Transporter Booth. you get there via an item called [transporter transponder] which gives 30 turns of access.
	if($locations[Domed City of Ronaldus, Domed City of Grimacia, Hamburglaris Shield Generator] contains goal)
	{
		return $item[transporter transponder];
	}
	
	//The Rabbit Hole. you get there via an item called [&quot;DRINK ME&quot; potion] which gives 20 turns of access.
	if($locations[The Red Queen\'s Garden] contains goal)
	{
		return $item[&quot;DRINK ME&quot; potion];
	}
	
	//Spring Break Beach. you get there via a dayticket
	if($locations[The Fun-Guy Mansion, Sloppy Seconds Diner, The Sunken Party Yacht] contains goal)
	{
		return $item[one-day ticket to Spring Break Beach];
	}
	
	//Dinseylandfill. you get there via a dayticket
	if($locations[Barf Mountain, Pirates of the Garbage Barges, The Toxic Teacups, Uncle Gator\'s Country Fun-Time Liquid Waste Sluice] contains goal)
	{
		return $item[one-day ticket to Dinseylandfill];
	}
	
	//That 70s Volcano. you get there via a dayticket
	if($locations[The SMOOCH Army HQ, The Velvet \/ Gold Mine, LavaCo&trade; Lamp Factory, The Bubblin\' Caldera] contains goal)
	{
		return $item[one-day ticket to That 70s Volcano];
	}
	
	//Conspiracy Island. you get there via a dayticket
	if($locations[The Deep Dark Jungle, The Mansion of Dr. Weirdeaux, The Secret Government Laboratory] contains goal)
	{
		return $item[one-day ticket to Conspiracy Island];
	}
	
	//The Glaciest. you get there via a dayticket
	if($locations[The Ice Hotel, VYKEA, The Ice Hole] contains goal)
	{
		return $item[One-day ticket to The Glaciest];
	}
	
	abort("Could not determine which access item matches the location [" + goal + "]. Please report this so it can be added");
	return $item[none];
}

boolean platinumZoneAvailable()
{
	//Checks if you already have access to a t3 zone
	if(guzzlr_QuestTier() != 3)
	{
		return false;
	}

	item access_item = accessItem();
	
	if(access_item == $item[tiny bottle of absinthe])
	{
		if(have_effect($effect[Absinthe-Minded]) > 0)
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[transporter transponder])
	{
		if(have_effect($effect[Transpondent]) > 0)
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[&quot;DRINK ME&quot; potion])
	{
		if(have_effect($effect[Down the Rabbit Hole]) > 0)
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[one-day ticket to Spring Break Beach])
	{
		if(get_property("sleazeAirportAlways").to_boolean() || get_property("_sleazeAirportToday").to_boolean())
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[one-day ticket to Dinseylandfill])
	{
		if(get_property("stenchAirportAlways").to_boolean() || get_property("_stenchAirportToday").to_boolean())
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[one-day ticket to That 70s Volcano])
	{
		if(get_property("hotAirportAlways").to_boolean() || get_property("_hotAirportToday").to_boolean())
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[one-day ticket to Conspiracy Island])
	{
		if(get_property("spookyAirportAlways").to_boolean() || get_property("_spookyAirportToday").to_boolean())
		{
			return true;
		}
		return false;
	}
	if(access_item == $item[One-day ticket to The Glaciest])
	{
		if(get_property("coldAirportAlways").to_boolean() || get_property("_coldAirportToday").to_boolean())
		{
			return true;
		}
		return false;
	}
	
	abort("Could not determine whether the platinum zone that goes with  [" + access_item + "] is available or not. Please report this so it can be added");
	return false;
}

boolean accessZoneViaItem()
{
	//this function is used to unlock platinum delivery zones via an item. either a day ticket or a potion that gives an effect that gives temporary access
	if(guzzlr_QuestTier() != 3)
	{
		return false;		//not doing a platinum quest
	}
	if(platinumZoneAvailable())
	{
		return true;		//already have access
	}

	item access_item = accessItem();
	tt_acquire(access_item);
	use(1, access_item);
	if(platinumZoneAvailable()) return true;
	abort("Failed to unlock zone using [" + access_item + "]");
	return false;	//need a return false even after abort.
}

boolean guzzlrAdv(location goal)
{
	location place = my_location();
	set_property("auto_priorLocation", place);
	
	//familiar switching
	familiar familiar_target_100 = get_property("auto_100familiar").to_familiar();
	if(familiar_target_100 != $familiar[none])		//do not break 100 familiar runs. yes, even in aftercore.
	{
		handleFamiliar(familiar_target_100);
		use_familiar(familiar_target_100);
	}
	else if(get_property("guzzlr_autoFamiliar").to_boolean())			//want to use auto familiar chooice.
	{
		handleFamiliar("drop");		//autoscend familiar choosing. choose a familiar that drops items
		if(!get_property("_auto_thisLoopHandleFamiliar").to_boolean())	//if could not find a drop familiar
		{
			handleFamiliar("item");
		}
		if(!get_property("_auto_thisLoopHandleFamiliar").to_boolean())	//if could not find an item familiar
		{
			handleFamiliar("meat");
		}
		use_familiar(get_property("auto_familiarChoice").to_familiar());
	}
	else if(get_property("guzzlr_manualFamiliar").to_boolean())		//want to use a user specified familiar
	{
		familiar fam = to_familiar(get_property("guzzlr_manualFamiliarChoice"));
		if(fam == $familiar[none])
		{
			abort("Tried to use a manually chosen familiar but failed to convert it from a string to a familiar");
		}
		use_familiar(fam);
	}
	
	equipMaximizedGear();
	cli_execute("checkpoint clear");
	executeFlavour();
	
	return adv1(goal, -1, "");
}

boolean tt_accessPirates()
{
	//this function is used to unlock pirate delivery zones for gold or bronze zones if desired and needed.
	//since this zone has unusual handling it is done first.
	
	location goal = get_property("guzzlrQuestLocation").to_location();
	if($locations[Barrrney\'s Barrr, The F\'c\'le, The Poop Deck, Belowdecks] contains goal)
	{
		if(!get_property("guzzlr_allowPirateQuest").to_boolean())
		{
			abort("We would need to do the pirate quest to unlock location [" + goal + "]. but are not permitted to do so");
		}
	}
	else
	{
		return false; 	//we are not in a pirate zone
	}
	//we always want the fledges for pirates. because the disguise pants conflict with guzzlr pants
	if(possessEquipment($item[pirate fledges]) && can_adv(goal))
	{
		return false; 	//if we have fledges we are done with this function and should use the normal guzzlr adventuring function instead.
	}
	
	//use autoscend pre and post adv scripts while unlocking pirates
	backupSetting("afterAdventureScript", "scripts/autoscend/auto_post_adv.ash");
	backupSetting("betweenBattleScript", "scripts/autoscend/auto_pre_adv.ash");

	if(LX_islandAccess()) return true;
	if(LX_pirateOutfit()) return true;
	
	if (internalQuestStatus("questM12Pirate") < 5)
	{
		if($location[Barrrney\'s Barrr] == goal)
		{
			if(possessEquipment(GUZZLR_SHOES)) autoEquip($slot[acc1], GUZZLR_SHOES);
		}
		if(LX_joinPirateCrew()) return true;
	}
	if (internalQuestStatus("questM12Pirate") == 5)
	{
		if($location[The F\'c\'le] == goal)
		{
			if(possessEquipment(GUZZLR_SHOES)) autoEquip($slot[acc1], GUZZLR_SHOES);
		}
		if(LX_fledglingPirateIsYou()) return true;
	}
	if(LX_unlockBelowdecks()) return true;
	
	return false;
}

boolean tt_accessZoneViaAdv()
{
	//this function is used to unlock delivery zones for gold or bronze zones if desired and needed.
	//unlocking might have to spend adventures. If zone with a high adv req is added then a setting will be added.
	//return true if we want to restart the main loop. aka we spend a turn somewhere
	//return false if we want main loop to continue. aka have already unlocked the target zone.
	
	location goal = get_property("guzzlrQuestLocation").to_location();
	if(can_adv(goal)) return false;		//location already unlocked.
	
	//[cobb's knob menagerie key] unlocks 3 locations in menagerie. no need to for setting here since it is very few adv.
	if($locations[Cobb's Knob Menagerie\, Level 1, Cobb's Knob Menagerie\, Level 2, Cobb's Knob Menagerie\, Level 3] contains goal)
	{
		if(item_amount($item[Cobb\'s Knob Menagerie key]) > 0) return false;
		if(guzzlrAdv($location[cobb\'s knob laboratory])) return true;
		abort("Failed to adventure in [cobb\'s knob laboratory] to unlock Menagerie");
	}
	//locations that are unlocked as part of the nemesis quest.
	if($locations[The Fungal Nethers, The \"Fun\" House] contains goal)
	{
		backupSetting("afterAdventureScript", "scripts/autoscend/auto_post_adv.ash");
		backupSetting("betweenBattleScript", "scripts/autoscend/auto_pre_adv.ash");
		if(LX_NemesisQuest()) return true;		//currently only partially unlocks fungal neathers. you need to finish it manually.
	}
	
	//These zones are unlocked by starting a subquest. costs nothing.
	switch(goal)
	{
		case $location[The Old Landfill]:		startHippyBoatmanSubQuest();	break;
		case $location[Madness Bakery]:			startArmorySubQuest();			break;
		case $location[The Skeleton Store]:		startMeatsmithSubQuest();		break;
		case $location[The Overgrown Lot]:		startGalaktikSubQuest();		break;
	}
	
	return false;
}

void guzzlrEquip()
{
	//modify maximizer string with some specific equipment.
	//acc1 is locked for shoes
	//acc2 is for mafia thumb ring (at the moment)
	//acc3 is for zone specific required equipment
	
	//can't force equip accessories due to how mafia handles slots. namely -acc1 will prevent mafia from equipping any accessory.
	if(possessEquipment(GUZZLR_SHOES)) autoEquip($slot[acc1], GUZZLR_SHOES);
	if(possessEquipment(GUZZLR_PANTS)) autoForceEquip(GUZZLR_PANTS);
	
	//TODO only wear the hat if you are going to finish the delivery now.
	//if(possessEquipment(GUZZLR_HAT)) autoEquip(GUZZLR_HAT);
	
	//mafia thumb ring has 4% chance to generate extra adventures
	if(get_property("guzzlr_useMafiaThumbRing").to_boolean())
	{
		autoEquip($slot[acc2], $item[Mafia Thumb Ring]);
	}

	//some zones require you to be wearing a certain piece of equip to access them even after they were unlocked.
	location goal = get_property("guzzlrQuestLocation").to_location();
	
	if($locations[Barrrney\'s Barrr, The F\'c\'le, The Poop Deck, Belowdecks] contains goal)
	{
		autoEquip($slot[acc3], $item[pirate fledges]);
	}
	if($location[The Obligatory Pirate\'s Cove] == goal)
	{
		//must not wear fledges or outfit by accident. as they would replace adventuring in pirate cove with an entire zone
		addToMaximize("-equip pirate fledges");
		addToMaximize("-equip stuffed shoulder parrot");
	}
	
	if($location[The Secret Government Laboratory] == goal)
	{
		if(possessEquipment($item[Personal Ventilation Unit]))
		{
			autoEquip($slot[acc3], $item[Personal Ventilation Unit]);
		}
	}
	
	if($location[8-Bit Realm] == goal)
	{
		if(!possessEquipment($item[continuum transfunctioner]))
		{
			visit_url("place.php?whichplace=forestvillage&action=fv_mystic");
		}
		if(!possessEquipment($item[continuum transfunctioner]))
		{
			abort("please get the [continuum transfunctioner] to unlock [8-bit realm] then run me again");
		}
		autoEquip($slot[acc3], $item[continuum transfunctioner]);
	}

}

boolean abandonQuest()
{
	//returns true if quest is successfully abandoned.
	if(get_property("_guzzlrQuestAbandoned").to_boolean())
	{
		return false; //can only abandon one a day
	}
	if(quest_unstarted("questGuzzlr"))
	{
		return true;
	}
	
	visit_url("inventory.php?tap=guzzlr", false);
	run_choice(1);	//abandon quest
	run_choice(5);	//exit menu
	
	if(quest_unstarted("questGuzzlr"))
	{
		return true;
	}
	return false;
}

item platinumDrinkIngredient(item drink)
{
	if(drink == $item[Steamboat]) return $item[miniature boiler];
	if(drink == $item[Ghiaccio Colada]) return $item[cold wad];
	if(drink == $item[Nog-on-the-Cob]) return $item[robin's egg];
	if(drink == $item[Sourfinger]) return $item[mangled finger];
	if(drink == $item[Buttery Boy]) return $item[Dish of Clarified Butter];
	abort("item platinumDrinkIngredient(item drink) failed to recognize the drink [" + drink + "]");
	return $item[none];
}

item cheapestPlatinumDrink()
{
	item drink = $item[Ghiaccio Colada];
	int cheapest_price = auto_mall_price(platinumDrinkIngredient(drink));
	
	foreach it in $items[Steamboat, Nog-on-the-Cob, Sourfinger, Buttery Boy]
	{
		if(auto_mall_price(platinumDrinkIngredient(it)) < cheapest_price)
		{
			drink = it;
			cheapest_price = auto_mall_price(platinumDrinkIngredient(it));
		}
	}
	
	return drink;
}

void abandonPlatinum()
{
	if(get_property("_guzzlrQuestAbandoned").to_boolean())
	{
		return;		//can only abandon 1 delivery a day
	}
	//using internal tracking below because mafia is tracking deliveries finished not deliveries started
	if(get_property("_guzzlrPlatinumDeliveries").to_int() > 0)
	{
		return;		//can only start 1 delivery a day
	}
	if(get_property("guzzlrGoldDeliveries").to_int() < 5)
	{
		return;		//not enough gold deliveries finished ever to unlock platinum
	}
	
	if(guzzlr_QuestStart(3))
	{
		if(!abandonQuest())		//try to abandon quest
		{
			abort("Failed to abandon platinum guzzlr delivery");
		}
	}
}

boolean guzzlr_deliverLoop()
{
	//return true when changes are made to restart the loop.
	//return false to end the loop.
	
	resetState();
	auto_interruptCheck();
	
	//try to fix two platinum deliveries in a row not crafting drink for the second one. r20148
	if(quest_unstarted("questGuzzlr"))
	{
		cli_execute("refresh inventory");
	}
	
	//start best quest
	if(quest_unstarted("questGuzzlr") && get_property("guzzlr_deliverPlatinum").to_boolean())
	{
		guzzlr_QuestStart(3);		//platinum
	}
	if(quest_unstarted("questGuzzlr") && get_property("guzzlr_deliverGold").to_boolean())
	{
		guzzlr_QuestStart(2);		//gold
	}
	if(quest_unstarted("questGuzzlr"))
	{
		if(get_property("guzzlr_deliverBronze").to_boolean())
		{
			guzzlr_QuestStart(1);		//bronze
		}
		else	//we failed to start plat and gold and do not want to deliver bronze. we are done.
		{
			print("All desired deliveries done. Finishing up for today", "blue");
			return false;
		}
	}
	if(guzzlr_QuestTier() == 0)
	{
		abort("Failed to start guzzlr quest for some reason");
	}
	
	//find out costs and targets
	item drink;
	int drink_price;
	if(guzzlr_QuestTier() == 3)
	{
		drink = cheapestPlatinumDrink();
		drink_price = auto_mall_price(platinumDrinkIngredient(drink));
	}
	else
	{
		drink = get_property("guzzlrQuestBooze").to_item();
		drink_price = auto_mall_price(drink);
	}
	location goal = get_property("guzzlrQuestLocation").to_location();
	
	int max_cost_bronze = get_property("guzzlr_maxMeatCostBronze").to_int();
	int max_cost_gold = get_property("guzzlr_maxMeatCostGold").to_int();
	int max_cost_platinum = get_property("guzzlr_maxMeatCostPlatinum").to_int();
	boolean abandon_too_expensive = get_property("guzzlr_abandonTooExpensive").to_boolean();
	
	//abandon or abort if quest is too expensive	
	if((guzzlr_QuestTier() == 1 && drink_price > max_cost_bronze) || (guzzlr_QuestTier() == 2 && drink_price > max_cost_gold))
	{
		if(abandon_too_expensive)
		{
			print("The booze [" + drink + "] is too expensive. abandoning delivery", "blue");
			if(abandonQuest()) return true;
		}
		abort("The drink [" + drink + "] is too expensive");
	}
	if(guzzlr_QuestTier() == 3)
	{
		item access_item = accessItem();
		int price_access_item = auto_mall_price(access_item);
		int price_sum = drink_price;
		if(!platinumZoneAvailable())
		{
			price_sum += price_access_item;
			if(access_item == $item[tiny bottle of absinthe])
			{
				price_sum += 2*price_access_item;		//guesstimate needing 3 tiny bottle of absinthe per delivery
			}
		}
		
		if(max_cost_platinum < price_sum)
		{
			if(abandon_too_expensive)
			{
				print("The access item [" + access_item + "] is too expensive. abandoning delivery", "blue");
				if(abandonQuest()) return true;
			}
			abort("The access item [" + access_item + "] is too expensive");
		}
		if(!is_unrestricted(access_item))
		{
			print("The access item [" + access_item + "] is restricted in your path. abandoning delivery", "blue");
			if(abandonQuest()) return true;
			abort("The access item [" + access_item + "] is restricted in your path and we failed to abandon delivery");
		}
	}
	
	//acquire drink
	if(item_amount(drink) == 0)
	{
		if(guzzlr_QuestTier() == 3)
		{
			tt_acquire(platinumDrinkIngredient(drink));
			create(drink);
		}
		else
		{
			tt_acquire(drink);
		}
	}
	if(item_amount(drink) == 0)
	{
		abort("Failed to acquire the booze [" + drink + "]");
	}
	
	//buy and use iotm access item for platinum delivery if needed
	accessZoneViaItem();
	
	//adventure to unlock zones for gold or bronze deliveries if needed and desired.
	//check it twice in case something transient causes an errant false to be returned.
	if(tt_accessPirates()) return true;
	if(tt_accessPirates()) return true;
	if(tt_accessZoneViaAdv()) return true;
	if(tt_accessZoneViaAdv()) return true;
	if(!can_adv(goal)) abort("failed to unlock the zone [" + goal + "]");
	
	//now that we are done with autoscend stuff. make sure we are back to not using pre and post adventure scripts
	restoreSetting("afterAdventureScript");
	restoreSetting("betweenBattleScript");
	
	//wear some equipment.
	guzzlrEquip();
	
	//get some +combat
	shrugAT($effect[Carlweather\'s Cantata Of Confrontation]);		//remove an AT buff so we have room for Cantata Of Confrontation
	foreach eff in $effects[Musk of the Moose, Carlweather\'s Cantata of Confrontation]
	{
		buffMaintain(eff, 0, 1, 1);
	}
	getHorse("crazy");	//black horse gives -5 combat chance
	asdonBuff($effect[Driving Obnoxiously]);	//+10 combat chance
	
	//finally adventure
	if(guzzlrAdv(goal)) return true;
	abort("Failed to adventure in [" + goal + "]");
	return false;
}

void guzzlr_deliver(int adv_to_use)
{
	backupSetting("printStackOnAbort", true);
	backupSetting("promptAboutCrafting", 0);
	backupSetting("breakableHandling", 4);
	backupSetting("dontStopForCounters", true);
	backupSetting("afterAdventureScript", "");
	backupSetting("betweenBattleScript", "");
	backupSetting("choiceAdventureScript", "scripts/ttpack/util/guzzlr_choice_adv.ash");
	backupSetting("currentMood", get_property("guzzlr_mood"));
	backupSetting("battleAction", "custom combat script");
	backupSetting("maximizerCombinationLimit", "10000");
	
	int adv_initial = my_session_adv();
	
	//primary loop
	int adv_spent = 0;
	try
	{
		while(adv_to_use > adv_spent && my_adventures() > 0 && guzzlr_deliverLoop())
		{
			adv_spent = my_session_adv() - adv_initial;
		}
	}
	finally
	{	
		restoreAllSettings();
	}
}

void guzzlr_aftercore(int adv_to_use)
{
	if(get_property("guzzlr_abandonPlatinumForcedAftercore").to_boolean())
	{
		abandonPlatinum();
	}
	guzzlr_deliver(adv_to_use);
}

void guzzlr_inrun(int adv_to_use)
{
	if(get_property("guzzlr_abandonPlatinumForcedInrun").to_boolean())
	{
		abandonPlatinum();
	}
	if(!get_property("guzzlr_deliverInrun").to_boolean())
	{
		return;		//delivering in run is disabled
	}
	guzzlr_deliver(adv_to_use);
}

void main(int adv_to_use)
{
	if(!possessEquipment($item[Guzzlr tablet]))
	{
		abort("I can't find a guzzlr tablet");
	}
	if(my_adventures() == 0)
	{
		abort("You don't have any adventures");
	}
	
	guzzlr_settings_defaults();
	
	boolean inrun = true;
	if(inPostRonin() && get_property("guzzlr_treatPostroninAsAftercore").to_boolean())
	{
		inrun = false;
	}
	else if(inCasual() && get_property("guzzlr_treatCasualAsAftercore").to_boolean())
	{
		inrun = false;
	}
	else if(inAftercore())
	{
		inrun = false;
	}
	
	try
	{
		if(inrun)
		{
			guzzlr_inrun(adv_to_use);
		}
		else
		{
			guzzlr_aftercore(adv_to_use);
		}
	}
	finally
	{
		guzzlr_settings_print();
	}
}