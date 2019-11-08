/*
  Script: CakeLevel

  Provides functions and infrastructure for an automatic-leveling script.
  This is intended to replace LevelUp.ash by dj_d but it only can handle
  increasing the three unbuffed attributes, not meat.
*/

// Imports of other scripts
import <CakeLib.ash>
import <CakeData.ash>
import <CakeProvision.ash>
import <canadv.ash>

/*
  Variable: CAKE_LEVEL_VERSION

  Contains the version of this library.
*/
int CAKE_LEVEL_VERSION = 0;

/*
  Variable: cake_level_verbosity

  Contains the verbosity of this library. It defaults to zero, but manually
  setting this will increase the information printed to the user.
*/
int cake_level_verbosity = 0;

// This script assumes that Wossname.ash is going to handle the island war.
setvar("cake_level_consider_island_war", false);

// Clan basements can require coordination with the other clan members and
// aren't suitable for level grinding. Though, they do produce relatively a
// large amount of experience.
setvar("cake_level_consider_clan_basement", false);

// can_adv() cannot handle holiday detection yes, so this script ignores them.
setvar("cake_level_consider_holiday", false);

// At the moment, item-based locations are not handled.
setvar("cake_level_consider_item_based", false);

// These areas requires an outfit change, which is not handled yet.
setvar("cake_level_consider_pirate", false);
setvar("cake_level_consider_dwarf", false);

// This determines how many locations we attempt to optimize for. This should
// be a relatively small number since each optimization can hit the server
// fairly hard.
int cake_level_optimize_size = 3;

/*
  Structure: cake_level_location
*/
record cake_level_location
{
	location loc;
	cake_location cake_loc;
	float adventure_stats;
	float adventure_meat;
	float combat_stats;
	float combat_meat;
	float noncombat_stats;
	float noncombat_meat;
	float score;
	string cli_script;
};

/*
  Section: Logging
*/

/*
  Function: cake_level_error
*/
void cake_level_error(int detail, string message)
{
	if (detail <= cake_level_verbosity)
		cake_error(message);
}

/*
  Function: cake_level_warning
*/
void cake_level_warning(int detail, string message)
{
	if (detail <= cake_level_verbosity)
		cake_warning(message);
}

/*
  Function: cake_level_info
*/
void cake_level_info(int detail, string message)
{
	if (detail <= cake_level_verbosity)
		cake_info(message);
}

/*
  Variable: cake_level_locations

  Contains a list of locations and their calculated values.
*/
cake_level_location[location] cake_level_locations;

/*
  Section: Scoring

  This section details with functions that score the individual provision
  against a desired primary goal. This is intended to be used with the
  "call" interface and won't be directly used within the function.

  All of these functions return a float score.
*/

float cake_score_muscle_gain(cake_provision provision)
{
	return 0;
}

float cake_score_moxie_gain(cake_provision provision)
{
	return 0;
}

float cake_score_mysticality_gain(cake_provision provision)
{
	return 0;
}

/*
  Section: Leveling Functions
*/

/*
  Function: cake_calculate_combat_stats
*/
float cake_calculate_combat_stats(stat desired_stat, float stats)
{
	// Add any stat increases for familiars, items, and potential effects.

	// The base formula for stats is breaking the stats into a 2:1:1 ratio
	// with the 2 being the user's prime stat. There are some items that
	// change change this.
	float amount = stats / 4;

	if (desired_stat == my_primestat()) amount = amount * 2;

	// Return the resulting stat points.
	return amount;
}

// -----------------------------------------------------------------------------
// cake_level_calculate_locations(stat desired_stat)
//	desired_stat: The stat the script wants to increase.
//
// This function integrates the cake_location map with values of the player at
// the current time. This will actually calculate out the worth of every
// location and also determine if they are adventurable or not.
// -----------------------------------------------------------------------------
void cake_level_calculate_locations(stat desired_stat)
{
	// Make a bit of noise.
	cake_info("Calculating locations for current player.");
	cake_indent();

	// Load the cake location's from cakelib. Then loop through the
	// locations in that array as our primary loop routine.
	cake_load_locations_cache();
	clear(cake_level_locations);

	foreach location_name in cake_locations_cache
	{
		// Grab the location associated with this. Ignore invalid
		// locations, which includes the header.
		cake_location cake_loc = cake_locations_cache[location_name];
		location loc = to_location(location_name);
		if (loc == $location[none]) continue;

		// See if this is a location we would ignore due to either
		// settings, it being obsolete or plot related. Special also
		// means can_adv() can't handle it yet.
		if (cake_loc.location_type == "obsolete" ||
		    cake_loc.location_type == "special" ||
		    (cake_loc.location_type != "none" &&
				!to_boolean(vars["cake_level_consider_" + cake_loc.location_type])))
		{
			// Ignore it, we don't want to use it.
			cake_print("Ignore " + location_name
				+ " because of type: "
				+ cake_loc.location_type,
				"gray");
			continue;
		}

		// We have a location that isn't specifically filtered out by our
		// controls, so now validate if we can adventure there using
		// can_adv().
		if (!can_adv(loc, false))
		{
			cake_print("Cannot adventure in " + location_name, "gray");
			continue;
		}

		// Figure out the expected combat yields of this location.
		float combat_stats = cake_calculate_combat_stats(
			desired_stat,
			cake_loc.combat_stats);
		float combat_meat = cake_loc.combat_meat;

		// Figure out the expected non-combat yields.
		float noncombat_stats = 0;

		if (desired_stat == $stat[Muscle])
		{
			noncombat_stats = eval_rpn_character(cake_loc.noncombat_mus);
		}
		else if (desired_stat == $stat[Moxie])
		{
			noncombat_stats = eval_rpn_character(cake_loc.noncombat_mox);
		}
		else
		{
			noncombat_stats = eval_rpn_character(cake_loc.noncombat_mys);
		}

		float noncombat_meat = eval_rpn_character(cake_loc.noncombat_meat);

		// Calculate the per-adventure values.
		float combat_rate = cake_loc.combat_rate;
		float adventure_stats =
			((combat_rate * combat_stats)
				+ ((100 - combat_rate) * noncombat_stats)) / 100;
		float adventure_meat =
			((combat_rate * combat_meat)
				+ ((100 - combat_rate) * noncombat_meat)) / 100;

		// Create a new record since we now have somewhere we can
		// adventure and hopefully do things.
		cake_level_location level_location;
		level_location.loc = loc;
		level_location.cake_loc = cake_loc;
		level_location.combat_meat = combat_meat;
		level_location.combat_stats = combat_stats;
		level_location.adventure_meat = adventure_meat;
		level_location.adventure_stats = adventure_stats;
		cake_level_locations[loc] = level_location;

		// Write out a bit of information about the location.
		cake_info("Found a valid location: " + location_name);
		cake_indent();
		cake_info("Combat yields: meat=" + combat_meat
			+ ", stats=" + combat_stats);
		cake_info("Noncombat yields: meat=" + noncombat_meat
			+ ", stats=" + noncombat_stats);
		cake_info("Adventure yields: meat=" + adventure_meat
			+ ", stats=" + adventure_stats);

		cake_info("Flags:");
		cake_indent();
		float character_attack = my_basestat($stat[Muscle]);
		cake_info("Location: max attack=" + cake_loc.max_attack);
		cake_outdent();

		cake_outdent();

		// Debugging, stop after X items.
		//if (count(cake_level_locations) > 10) return;
	}

	// Outdent and finish up.
	cake_outdent();
}

// -----------------------------------------------------------------------------
// cake_level_insert_sorted(...)
//
// These two functions create a sorted list of the level_location with the
// highest scores. The cake_level_optimize_size variable controls how large
// the array and also determines how many locations will attempt to be optmized.
// -----------------------------------------------------------------------------
boolean cake_level_insert_sorted(
	cake_level_location[int] options,
	cake_level_location level_location,
	int index)
{
	// If there isn't anything in that location, just insert it.
	if (!(options contains index))
	{
		options[index] = level_location;
		return true;
	}

	// Compare the given location to the one in the array. If the one there
	// has a higher score, then we don't do anything.
	cake_level_location option_location = options[index];

	if (option_location.score >= level_location.score) return false;

	// We need to move everything down before insert into this location.
	for move_index from index upto (cake_level_optimize_size - 2)
	{
		options[move_index + 1] = options[move_index];
	}

	// Now insert this into the proper location.
	options[index] = level_location;
	return true;
}

void cake_level_insert_sorted(
	cake_level_location[int] options,
	cake_level_location level_location)
{
	for index from 0 upto (cake_level_optimize_size - 1)
	{
		// Try to insert this into the given index. If it has a higher
		// score, it will move everything down and return true.
		if (cake_level_insert_sorted(options, level_location, index))
		{
			// Everything is inserted correctly, so finish up.
			return;
		}
	}
}

// -----------------------------------------------------------------------------
// cake_location_pick_stat_location()
//
// Goes through the level_location list and finds the best location for getting
// stat gains.
// -----------------------------------------------------------------------------
cake_level_location cake_location_pick_stat_location()
{
	// Make a bit of noise.
	cake_info("Choosing the best location for stat gain.");
	cake_indent();

	// Go through all the locations we have determine we can visit.
	cake_level_location[int] options;

	foreach loc in cake_level_locations
	{
		// Grab the location and compare it to the list.
		cake_level_location level_location = cake_level_locations[loc];
		level_location.score = level_location.adventure_stats;
		cake_level_insert_sorted(options, level_location);
	}

	// Tell which ones we are considering.
	cake_info("Considering the following locations:");
	cake_indent();

	foreach option_index in options
	{
		cake_level_location option = options[option_index];
		cake_info(option.loc + ": score=" + option.score);
	}

	cake_outdent();

	// TODO Optimize the character for each location and find the actual
	// "best" score. Once the scores are built, use insert_sorted() to
	// resort the list.

	// Finish up and return it.
	cake_level_location best_location = options[0];
	cake_info("Best location found is: " + best_location.loc);
	cake_info("Adventure yields: meat=" + best_location.adventure_meat
		+ ", stats=" + best_location.adventure_stats);
	cake_outdent();
	return options[0];
}

// -----------------------------------------------------------------------------
// cake_achieve(stat desired_stat, int desired_level, int turn_budget)
//	desired_stat: The stat that needs to be improved.
//	desired_level: The level when this is considered done.
//	turn_budget: The number of turns to perform these actions.
//
//
// Calling this function will attempt to achieve a specific stat level
// in a given number of turns.
// -------------------------------------------------------------------
boolean cake_achieve(stat desired_stat, int desired_level, int turn_budget)
{
	// If we already got it, just finish up.
	string stat_goal = desired_stat + " " + desired_level;
	if (my_basestat(desired_stat) >= desired_level)
	{
		cake_warning("Already achieved " + stat_goal + ".");
		return true;
	}

	// Make a bit of noise so the user knows what is going on.
	cake_print("Attempting to achieve " + stat_goal + " in "
		+ turn_budget + " turns.");
	cake_indent();

	// Calculate all the internal values for our locations.
	cake_level_calculate_locations(desired_stat);

	if (count(cake_level_locations) == 0)
	{
		cake_error("Cannot find a location to adventure!");
		cake_outdent();
		return false;
	}

	// Find the best level to adventure.
	cake_level_location best_location = cake_location_pick_stat_location();

	// If we aren't simulating, then actually adventure there.
	if (turn_budget > 0)
	{
		cake_print(0, "Now attempting to adventure at "
			+ best_location.loc
			+ " for " + turn_budget + " turns.");
		wait(5);

		// Set the condition and adventure at the given location.
		adventure(1, best_location.loc, desired_stat + " " + desired_level);
	}
	else
	{
		cake_print(0, "Cannot adventure here without turns budgeted.");
	}

	// Finish up with a bit of reporting.
	boolean successful = my_basestat(desired_stat) >= desired_level;

	if (successful)
	{
		cake_print("Successful achieved " + stat_goal + ".", "green");
	}
	else
	{
		cake_error("Did not successful achieve " + stat_goal + ".");
	}

	cake_outdent();
	return successful;
}

/*
  Function: cake_achieve_level

  Parameters:

  desired_level - The desired character level.
  adventures - The number of adventures to attempt to gain the level.
*/
boolean cake_achieve_level(int desired_level, int adventures)
{
	// Check to see if we are already there.
	if (my_level() >= desired_level) return true;

	// Figure out what level of the prime stat we need to fulfill gain
	// the desired level. The formula for the stat needed is:
	//	stat points = desired_level ** 2 + 4.
	int last_level = desired_level - 1;
	int need_stat_level = (last_level * last_level) + 4;

	// Get the primary stat and level needed for that point.
	cake_achieve(my_primestat(), need_stat_level, adventures);

	// Return the results.
	return my_level() >= desired_level;
}

// -----------------------------------------------------------------------------
// Main entry point into the script. This is called directly from the CLI and
// shows the version of the script and some simulated runs.
// -----------------------------------------------------------------------------
void main()
{
	// Make a bit of noise about the project.
	cake_info("CakeLevel, a component of CakeLib (Version "
		+ CAKE_LEVEL_VERSION + ")");
	cake_indent();
	cake_info("Written by cakyrespa");
	cake_info("Provides automatic leveling of stats (and meat some days)");
	cake_outdent();

	// Turn on debugging and turn off actions.
	cake_verbose = 5;
	vars["cake_map_downloads"] = false;

	print("Adventure Hole: " + can_adv($location[Hole in the Sky], false));

	//print("gt " + can_adv($location[Greater-Than Sign], false));
	//print("gt " + can_adv($location[Dungeons of Doom], false));

	// Show the strategy for gaining a level using all our adventures.
	//cake_achieve(my_primestat(), my_basestat(my_primestat()), 0);
	//cake_achieve_level(my_level() + 1, 0);

	cake_achieve($stat[Moxie], my_basestat($stat[Moxie]) + 1, 0);

	/*
	// Show the strategy for gaining +1 muscle.
	int desired_muscle = my_basestat($stat[Muscle]) + 1;
	print("Location and strategy for base Muscle " + desired_muscle + ".");
	achieve($stat[Muscle], desired_muscle, my_adventures());

	// Show the strategy for gaining +1 moxie.
	int desired_moxie = my_basestat($stat[Moxie]) + 1;
	print("Location and strategy for base Moxie " + desired_moxie + ".");
	achieve($stat[Moxie], desired_moxie, my_adventures());

	/*
	// Show the strategy for gaining +1 mystically.
	int desired_mysticality = my_basestat($stat[Mysticality]) + 1;
	print("Location and strategy for base Mysticality " + desired_mysticality + ".");
	achieve($stat[Mysticality], desired_mysticality, my_adventures());
	*/
}
