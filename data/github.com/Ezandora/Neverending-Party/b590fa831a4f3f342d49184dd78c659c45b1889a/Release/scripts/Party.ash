string __party_version = "1.0.9";

boolean [int][int] parseSavedPartyChoices()
{
	boolean [int][int] party_choices_taken;
	string [int] choices_level_1 = get_property("_neverendingPartyChoicesTakenToday").split_string("\\|");
	foreach key, choice_unparsed in choices_level_1
	{
		string [int] choices_level_2 = choice_unparsed.split_string(",");
		if (choices_level_2.count() != 2) continue;
		party_choices_taken[choices_level_2[0].to_int()][choices_level_2[1].to_int()] = true;
	}
	return party_choices_taken;
}

void savePartyChoices(boolean [int][int] party_choices_taken)
{
	buffer out;
	foreach choice_id in party_choices_taken
	{
		foreach option in party_choices_taken[choice_id]
		{
			if (out.length() != 0)
				out.append("|");
			out.append(choice_id);
			out.append(",");
			out.append(option);
		}
	}
	set_property("_neverendingPartyChoicesTakenToday", out);
}

void main(string arguments)
{
	arguments = arguments.to_lower_case();
	
	if (arguments == "help")
	{
		print_html("Party v" + __party_version);
		print_html("<b>free</b>: only complete free fights");
		print_html("<b>quest</b>: complete quest (on by default)");
		print_html("<b>noquest</b>: only complete free fights, do not start quest (best for in-run)");
		print_html("<b>hard</b>: hard mode, if available");
		print_html("<b>mall</b>: open favors and sell results in mall");
		print_html("");
		print_html("Example usage:");
		print_html("<b>party quest</b>: complete quest");
		print_html("<b>party hard</b>: complete hard mode quest");
		print_html("<b>party noquest</b>: use when in-run - won't complete quest.");
		print_html("<b>party free</b>: only use free fights, but will complete the quest if it can.");
		return;
	}
	if (property_exists("_questPartyFair") && (get_property("_questPartyFair") == "finished" || get_property("_questPartyFair") == ""))
	{
		print_html("Quest done for the day.");
		return;
	}
	
	boolean start_quest = true;
	boolean only_do_free_fights = false;
	boolean complete_quest = true;
	boolean hard_mode = false;
	boolean sell_gains = false;
	boolean use_claras_bell = false;
	boolean block_guests_quest = false;
	boolean simple_mode_enabled = false;
	
	int [item] starting_favor_count;
	foreach it in $items[Neverending Party favor,deluxe Neverending Party favor]
	{
		starting_favor_count[it] = it.item_amount();
	}
	string [int] arguments_words = arguments.split_string(" ");
	foreach key, word in arguments_words
	{
		if (word == "quest")
		{
			only_do_free_fights = false;
			complete_quest = true;
		}
		if (word == "free")
		{
			only_do_free_fights = true;
		}
		if (word == "hard")
			hard_mode = true;
		if (word == "noquest")
		{
			start_quest = false;
			complete_quest = false;
			only_do_free_fights = true;
		}
		if (word == "mall")
		{
			//FIXME write this
			sell_gains = true;
			//FIXME do hard mode if the rewards are better on average
		}
		if (word == "clara" || word == "clara's" || word == "bell")
		{
			use_claras_bell = true;
		}
		if (word == "noguests" || word == "noguest")
		{
			block_guests_quest = true;
		}
		if (word == "simple") //don't change outfits, don't submit a combat script, don't do hard mode
		{
			simple_mode_enabled = true;
		}
		//FIXME buffs/statgain:
	}
	
	if ($item[PARTY HARD T-shirt].available_amount() == 0)
	{
		print("You don't have a PARTY HARD t-shirt, defaulting to easy mode...", "red");
		hard_mode = false;
	}
	
	boolean [int][int] party_choices_taken = parseSavedPartyChoices();
	
	int active_quest = -1;
	int quest_substate = -1;
	
	string last_maximisation = "";
	
	item item_wanted = $item[none];
	int item_wanted_amount = 0;
	item item_that_gives_wanted_item = $item[none];
	
	int QUEST_TYPE_DJ = 1;
	int QUEST_TYPE_CLEAR_OUT_GUESTS = 2;
	int QUEST_TYPE_GERALD = 3;
	int QUEST_TYPE_GERALDINE = 4;
	int QUEST_TYPE_HYPE = 5;
	int QUEST_TYPE_TRASH = 6;
	int QUEST_TYPE_NEARLY_COMPLETED = 7;
	
	int breakout = 100;
	while (breakout > 0)
	{
		breakout -= 1;
		
		if (only_do_free_fights)
		{
			//Check if we have free fights left:
			buffer town_wrong_page_text = visit_url("place.php?whichplace=town_wrong");
			if (!town_wrong_page_text.contains_text("The Wrong Side of the Tracks"))
			{
				run_combat();
			}
			town_wrong_page_text = visit_url("place.php?whichplace=town_wrong");
			if (!town_wrong_page_text.contains_text("The Wrong Side of the Tracks"))
			{
				print("Cannot see kingdom? Are you in a choice adventure or fight?", "red");
				return;
			}
			if (!town_wrong_page_text.contains_text("town/nparty_free.gif"))
			{
				print_html("Done with free fights.");
				break;
			}
		}
		if (property_exists("_questPartyFair"))
		{
			string quest_state_string = get_property("_questPartyFair");
			if (quest_state_string == "finished" || quest_state_string == "")
			{
				print_html("Done with quest.");
				break;
			}
		}
		if (start_quest)
		{
			set_property("choiceAdventure1322", 1);
		}
		else
		{
			set_property("choiceAdventure1322", 2);
		}
		
		if (complete_quest && active_quest == -1)
		{
			buffer quest_log_text = visit_url("questlog.php?which=7");
			if (!quest_log_text.contains_text("Your Quest Log"))
			{
				print("Error: cannot load quest log.", "red");
				return;
			}
			string partial_match = quest_log_text.group_string("<b>Party Fair</b>(.*?)<p>")[0][1];
			
			if (partial_match == "")
			{
				//not started yet
				if (property_exists("_questPartyFair") && get_property("_questPartyFair") != "unstarted")
				{
					print("Error: cannot parse quest log.", "red");
					return;
				}
			}
			else if (partial_match.contains_text("Return to the") && partial_match.contains_text("for your reward"))
			{
				active_quest = QUEST_TYPE_NEARLY_COMPLETED;
			}
			else if (partial_match.contains_text("Collect Meat for the DJ"))
			{
				active_quest = QUEST_TYPE_DJ;
			}
			else if (partial_match.contains_text("Clear all of the guests out of the"))
			{
				active_quest = QUEST_TYPE_CLEAR_OUT_GUESTS;
				if (block_guests_quest)
					only_do_free_fights = true;
			}
			else if (partial_match.contains_text("Clean up the trash at the"))
			{
				active_quest = QUEST_TYPE_TRASH;
			}
			else if (partial_match.contains_text("see what kind of booze Gerald wants"))
			{
				active_quest = QUEST_TYPE_GERALD;
				quest_substate = 0;
			}
			else if (partial_match.contains_text("to see what kind of snacks Geraldine wants"))
			{
				active_quest = QUEST_TYPE_GERALDINE;
				quest_substate = 0;
			}
			else if (partial_match.contains_text("for Gerald at the") || partial_match.contains_text("for Geraldine at the") || partial_match.contains_text("to Geraldine in the kitchen"))
			{
				if (partial_match.contains_text("Geraldine"))
					active_quest = QUEST_TYPE_GERALDINE;
				else
					active_quest = QUEST_TYPE_GERALD;
				quest_substate = 1;
				if (active_quest == QUEST_TYPE_GERALD)
				{
					item_that_gives_wanted_item = $item[unremarkable duffel bag];
				}
				else
				{
					item_that_gives_wanted_item = $item[van key];
				}
				
				string [int] progress_property_text_split = get_property("_questPartyFairProgress").split_string(" ");
				if (progress_property_text_split.count() == 2 && progress_property_text_split[0].to_int() == 10)
				{
					item_wanted_amount = progress_property_text_split[0].to_int();
					item_wanted = progress_property_text_split[1].to_item();
					if (item_wanted != $item[none])
						print("Want " + item_wanted_amount + " of " + item_wanted + " (from mafia tracking)");
				}
				if (item_wanted == $item[none])
				{
					//Parse the one we want:
					string [int][int] matches;
					if (active_quest == QUEST_TYPE_GERALD)
					{
						matches = partial_match.group_string("Get ([0-9]+) (.*?) for Gerald at the");
					}
					else
					{
						matches = partial_match.group_string("Get ([0-9]+) (.*?) for Geraldine at the");
					}
					if (matches.count() == 0)
						matches = partial_match.group_string("Take the ([0-9]+) (.*?) to Geraldine in the kitchen");
					int amount_wanted = matches[0][1].to_int();
					string plural_wanted = matches[0][2];
					item_wanted_amount = amount_wanted;
					//Convert plural back:
					foreach it in $items[]
					{
						if (it.plural == plural_wanted)
						{
							item_wanted = it;
							break;
						}
					}
					print("Want " + item_wanted_amount + " of " + item_wanted + " (assumed match against plural \"" + plural_wanted + "\")");
				}
				if (item_wanted == $item[none])
				{
					print("Error: cannot recognise item the quest wants.", "red");
					return;
				}
			}
			else if (partial_match.contains_text("started!") && partial_match.contains_text("Hype level:"))
			{
				active_quest = QUEST_TYPE_HYPE;
			}
			else
			{
				print("Quest log parsing error: unknown partial match = \"" + partial_match.entity_encode() + "\"", "red");
				return;
			}
		}
		
		string maximisation_command;
		if (active_quest == QUEST_TYPE_DJ || !can_interact())
			maximisation_command = "meat";
		else
			maximisation_command = "item";
		maximisation_command += " -tie";
		if (active_quest == QUEST_TYPE_CLEAR_OUT_GUESTS)
		{
			if ($item[intimidating chainsaw].available_amount() > 0)
				maximisation_command += " +equip intimidating chainsaw";
			
		}
		if (active_quest == QUEST_TYPE_HYPE)
		{
			if ($item[cosmetic football].available_amount() > 0)
				maximisation_command += " +equip cosmetic football";
		}
		if (inebriety_limit() - my_inebriety() < 0)
		{
			if ($item[drunkula's wineglass].available_amount() > 0 && $item[drunkula's wineglass].can_equip())
			{
				maximisation_command += " +equip drunkula's wineglass";
			}
			else
			{
				print_html("Overdrunk");
				break;
			}
		}
		if (hard_mode)
			maximisation_command += " +equip PARTY HARD T-shirt";
		maximisation_command += " -equip broken champagne bottle";
		if (last_maximisation != maximisation_command && !simple_mode_enabled)
		{
			maximize(maximisation_command, false);
			last_maximisation = maximisation_command;
		}
		if (hard_mode && $item[PARTY HARD T-shirt].equipped_amount() == 0 && $item[PARTY HARD T-shirt].available_amount() > 0 && !simple_mode_enabled)
		{
			//absolutely equip the t-shirt, because sometimes maximize() fails...? or something?
			equip($item[PARTY HARD T-shirt]);
		}
		//Items:
		if (item_wanted != $item[none] && can_interact() && item_wanted_amount <= 20)
		{
			//Compare cost of item_wanted versus using item_that_gives_wanted_item:
			//item_that_gives_wanted_item gives 2-4 of each.
			
			
			if (item_that_gives_wanted_item.mall_price() > 0 && item_that_gives_wanted_item.mall_price().to_float() / 3.0 < item_wanted.mall_price())
			{
				//Cheaper on average to use the van key/etc. So, use them:
				print_html("Cheaper to use " + item_that_gives_wanted_item);
				for i from 1 to 5
				{
					if (item_wanted.item_amount() >= item_wanted_amount)
						break;
					int amount_before = item_wanted.item_amount();
					use(1, item_that_gives_wanted_item);
					int amount_after = item_wanted.item_amount();
					if (amount_before == amount_after) break; //not working
				}
			}
			retrieve_item(item_wanted_amount, item_wanted);
		}
		//Choice adventure:
		int [int] choices;
		choices[1324] = 5; //like all things in life; if we run out of ideas, fight.
		if (active_quest == QUEST_TYPE_CLEAR_OUT_GUESTS)
		{
			if ($item[intimidating chainsaw].available_amount() == 0)
			{
				choices[1324] = 4;
				choices[1328] = 3;
			}
			else if (!party_choices_taken[1325][3] && $item[jam band bootleg].item_amount() > 0)
			{
				choices[1324] = 1;
				choices[1325] = 3;
			}
			else if (!party_choices_taken[1327][5] && $item[purple beast energy drink].item_amount() > 0)
			{
				//should we even...?
				choices[1324] = 3;
				choices[1327] = 5;
			}
		}
		if (active_quest == QUEST_TYPE_GERALD)
		{
			choices[1324] = 3;
			if (quest_substate == 0)
				choices[1327] = 3;
			else
				choices[1327] = 4;
		}
		if (active_quest == QUEST_TYPE_GERALDINE)
		{
			choices[1324] = 2;
			if (quest_substate == 0)
				choices[1326] = 3;
			else
				choices[1326] = 4;
			
		}
		if (active_quest == QUEST_TYPE_TRASH)
		{
			if (!party_choices_taken[1326][5])
			{
				//Is this even worth it? Last time I tried it, it did 81 pieces of trash. Fights did more than that, I think.
				choices[1324] = 2; //kitchen
				choices[1326] = 5; //burn some trash
			}
			else
				choices[1324] = 5; //fight!
		}
		if (active_quest == QUEST_TYPE_DJ)
		{
			if (my_buffedstat($stat[moxie]) >= 300 && !party_choices_taken[1325][4])
			{
				choices[1324] = 1;
				choices[1325] = 4;
			}
		}
		if (active_quest == QUEST_TYPE_HYPE)
		{
			if (!party_choices_taken[1328][4] && $item[electronics kit].item_amount() > 0)
			{
				choices[1324] = 4;
				choices[1328] = 4;
			}
			else if (!party_choices_taken[1325][5] && $item[very small red dress].item_amount() > 0)
			{
				choices[1324] = 1;
				choices[1325] = 5;
			}
		}
		
		foreach choice_id, option in choices
			set_property("choiceAdventure" + choice_id, option);
			
		string combat_script = "";
		//scaling monsters are tough, let's go shopping:
		if ($skill[saucestorm].have_skill())
		{
			combat_script = "cast saucestorm; repeat";
			if (my_level() < 13)
				restore_mp(32);
			else
				restore_mp(64);
		}
		if ($item[drunkula's wineglass].equipped_amount() > 0)
			combat_script = "attack; repeat;";
		if (get_property("customCombatScript") == "helix fossil")
			combat_script = "";
		if (use_claras_bell && $item[clara's bell].available_amount() > 0 && !get_property("_claraBellUsed").to_boolean() && active_quest > 0) //'
		{
			cli_execute("use clara's bell");
		}
		if (simple_mode_enabled)
			combat_script = "";
		boolean was_beaten_up = $effect[beaten up].have_effect() == 0;
		boolean successish = adv1($location[the neverending party], 0, combat_script);
		if ($effect[beaten up].have_effect() > 0 && !was_beaten_up)
		{
			print("You were beaten up! Stopping. Maybe perm saucestorm?", "red");
			return;
		}
		
		
		string last_encounter = get_property("lastEncounter");
		if (last_encounter == "All Done!" || last_encounter == "Party's Over" || !successish)
		{
			print_html("Done with quest.");
			break;
		}
		if (last_encounter == "Forward to the Back" || last_encounter == "Gone Kitchin'")
		{
			//reparse quest state, for Gerald/Geraldine:
			active_quest = -1;
			quest_substate = -1;
		}
		//Store last_choice() and last_decision(), for quests:
		
		party_choices_taken[last_choice()][last_decision()] = true;
		savePartyChoices(party_choices_taken);
	}
	
	if (sell_gains)
	{
		//Open party favors we gained, sell:
		int [item] favor_rewards_count;
		foreach it in $items[party beer bomb,sweet party mix,party balloon,Neverending Party guest pass,TRIO cup of beer,party platter for one,Party-in-a-Can&trade;]
		{
			favor_rewards_count[it] = it.item_amount();
		}
		foreach it in starting_favor_count
		{
			int new_count = it.item_amount();
			if (new_count > starting_favor_count[it])
			{
				use(1, it);
			}
		}
		foreach it, starting_amount in favor_rewards_count
		{
			int delta = it.item_amount() - starting_amount;
			if (delta <= 0) continue;
			if (delta > 3) continue; //what
			
			int price = it.mall_price();
			if (price <= 0) continue;
			put_shop(price, 0, delta, it); 
		}
	}
}