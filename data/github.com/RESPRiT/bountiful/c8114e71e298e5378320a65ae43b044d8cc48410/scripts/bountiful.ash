/******************************************************************************
* Bountiful by RESPRiT
* Version 0.3
* https://github.com/RESPRiT
*
* Adapted from AutoBHH:
* -Originally by izchak
* -Major revisions by Raorn and Zarqon
******************************************************************************/
script "Bountiful";
notify tamedtheturtle;
since r18000;
import <canadv.ash>;

/********************************
Custom Properties and Default Values (* indicates unused):
 - bountiful.useBanisher : false
 - bountiful.useCopier : false
 - bountiful.useFax : false
 - bountiful.useRunaway* : false
 - bountiful.useFreeKill : false
 - bountiful.maxBanishCost : autoBuyPriceLimit
 - bountiful.maxSpecialCost : autoBuyPriceLimit
 - bountiful.maxRunawayCost : autoBuyPriceLimit
 - bountiful.automaticallyGiveup : false
********************************/

/*
TODO:
 - handle elemental airport properties bug
 - reorder functions/general organization
 - script information for third-party comprehension
 - runaway logic
 - add olfaction
 - romantic arrow support
*/

//----------------------------------------
// Global Variables
item LAST_BANISH; // for debugging/preventing a mafia bug from breaking things
location LAST_LOCATION;

bounty current;

//----------------------------------------
// Constant Variables

// Properties
boolean useBan = get_property("bountiful.useBanisher").to_boolean();
boolean useCopier = get_property("bountiful.useCopier").to_boolean();
boolean useFax = get_property("bountiful.useFax").to_boolean();
boolean useRun = get_property("bountiful.useRunaway").to_boolean();
boolean useKill = get_property("bountiful.useFreeKill").to_boolean();
int maxBanish = get_property("bountiful.maxBanishCost").to_int();
if(maxBanish == 0) maxBanish = get_property("autoBuyPriceLimit").to_int();
int maxSpecial = get_property("bountiful.maxSpecialCost").to_int();
if(maxSpecial == 0) maxSpecial = get_property("autoBuyPriceLimit").to_int();
boolean giveup = get_property("bountiful.automaticallyGiveup").to_boolean();

// Types
string EASY = "easy";
string HARD = "hard";
string SPECIAL = "special";

// Banishers
int[item] BAN_ITEMS = {
  $item[Louder Than Bomb] : 20,
  $item[crystal skull] : 20,
  $item[tennis ball] : 20,
  $item[divine champagne popper] : 5
};

// TODO: add other skills (mostly IOTMs I don't have)
boolean[skill] BAN_SKILLS = {
  $skill[Snokebomb] : true,           // Snojo
  $skill[Talk About Politics] : true  // Pantsgiving
};

// Unlockers
item[location] CONTENT_ITEMS = {
  $location[Anger Man's Level] : $item[jar of psychoses (The Crackpot Mystic)],
  $location[Pirates of the Garbage Barges] : $item[one-day ticket to Dinseylandfill],
  $location[the Ice Hotel] : $item[one-day ticket to The Glaciest],
  $location[The Stately Pleasure Dome] : $item[tiny bottle of absinthe],
  $location[Domed City of Grimacia] : $item[transporter transponder],
  $location[LavaCo&trade; Lamp Factory] : $item[one-day ticket to That 70s Volcano],
  $location[The Clumsiness Grove] : $item[devilish folio],
  $location[The Nightmare Meatrealm] : $item[jar of psychoses (The Meatsmith)],
  $location[Sloppy Seconds Diner] : $item[one-day ticket to Spring Break Beach],
  $location[An Incredibly Strange Place (Bad Trip)] : $item[astral mushroom],
  $location[the Red Queen's Garden] : $item[&quot;DRINK ME&quot; potion],
  $location[Mt. Molehill] : $item[llama lama gong],
  $location[The Jungles of Ancient Loathing] : $item[empty agua de vida bottle],
  $location[Chinatown Shops] : $item[jar of psychoses (The Suspicious-Looking Guy)],
  $location[the Secret Government Laboratory] : $item[one-day ticket to Conspiracy Island]
};

// "No Banish" Locations
// TODO: incomplete? a is_banishable() function/property would be great..
boolean[location] NO_BANISH_LOCATIONS = {
  $location[Pirates of the Garbage Barges] : true,
  $location[the Secret Government Laboratory] : true,
  $location[Sloppy Seconds Diner] : true
};

//----------------------------------------
// Private Bounty Functions

/**
* Returns the current bounty item of the given type
* @param {string} type - the bounty type
* @returns {bounty} - the current bounty item of the given type, either taken
                      or untaken
*/
bounty _bounty(string type) {
  bounty ret;
  switch(type) {
    case EASY:
      ret = to_bounty(get_property("currentEasyBountyItem"));
      if(ret == $bounty[none]) {
        return to_bounty(get_property("_untakenEasyBountyItem"));
      } else {
        return ret;
      }
    case HARD:
      ret = to_bounty(get_property("currentHardBountyItem"));
      if(ret == $bounty[none]) {
        return to_bounty(get_property("_untakenHardBountyItem"));
      } else {
        return ret;
      }
    case SPECIAL:
      ret = to_bounty(get_property("currentSpecialBountyItem"));
      if(ret == $bounty[none]) {
        return to_bounty(get_property("_untakenSpecialBountyItem"));
      } else {
        return ret;
      }
    default:
      abort("_bounty: Invalid bounty type");
  }
  return $bounty[none];
}

/**
* Returns if the bounty of the given type is currently taken
* @param {string} type - the bounty type
* @returns {int} - if the bounty of the given type is currently taken
*/
boolean _accepted(string type) {
  switch(type) {
    case EASY:
      return get_property("currentEasyBountyItem") != "";
    case HARD:
      return get_property("currentHardBountyItem") != "";
    case SPECIAL:
      return get_property("currentSpecialBountyItem") != "";
    default:
      abort("_accepted: Invalid bounty type - " + type);
  }
  return false;
}

// @Overload
boolean _accepted(bounty b) {
  return _accepted(b.type);
}

/**
* Returns the number of bounty items acquired of the given type
* @param {string} type - the bounty type
* @returns {int} - the bounty item count if the type is accepted,
                   -1 if the given type is not currently accepted
*/
int _count(string type) {
  string[int] prop;
  if(_accepted(type)) {
    switch(type) {
      case EASY:
        prop = get_property("currentEasyBountyItem").split_string(":");
        return prop[1].to_int();
      case HARD:
        prop = get_property("currentHardBountyItem").split_string(":");
        return prop[1].to_int();
      case SPECIAL:
        prop = get_property("currentSpecialBountyItem").split_string(":");
        return prop[1].to_int();
      default:
        abort("_count: Invalid bounty type");
      return -666;
    }
  } else {
    return -1;
  }
}

/**
* Returns the number of bounty items remaining of the given type
* @param {string} type - the bounty type
* @returns {int} - the bounty items remaining if the type is accepted,
                   -1 if the given type is not currently accepted
*/
int _remaining(string type) {
  int count = _count(type);
  int total = _bounty(type).number;

  if(count >= 0) {
    return total - count;
  } else {
    return count;
  }
}

// @Overload
int _remaining(bounty b) {
  return _remaining(b.type);
}

//----------------------------------------
// Helper Functions

/**
* Returns the number of copies available daily
* @returns {int} - the number of copies available daily, does not subtract
                   copies used that day
*/
int copies_available() {
  int copies = 0;

  if(available_amount($item[Rain-Doh black box]) > 0) {
    copies += 1;
  }

  if(available_amount($item[Spooky Putty sheet]) > 0) {
    copies += 1;
  }

  if(copies > 0) {
    copies += 4;
  }

  return copies;
}

/**
* Purchases banishes that are within budget
* @returns {boolean} - if at least one banisher was purchased
*/
boolean buy_banishers() {
  int count = 0;

  foreach banisher in BAN_ITEMS {
    if(item_amount(banisher) < 1) {
      count += buy(1, banisher, maxBanish);
    }
  }

  return count > 0;
}

/**
* Returns if the given location allows banishing
* @param {location} l - the location to check
* @returns {boolean} - true if the location allows banishing
*/
boolean can_banish(location l) {
  return !(NO_BANISH_LOCATIONS contains l);
}

/**
* Uses an item with the given combat filter, like use() but with combat filters
* @param {item} it - the item to use
* @param {string} filter - the combat filter to use
* @returns {boolean} - true if the item was used successfully
*/
boolean use_combat(item it, string filter) {
  string page = visit_url("inv_use.php?&pwd&which=3&checked=1&whichitem=" + it.to_int());

  if(page.contains_text("You don't have the item you're trying to use")) {
    return false;
  }

  if(page.contains_text("You're fighting")) {
    run_combat(filter);
  }

  return true;
}

//----------------------------------------
// BHH Functions

/**
* Visits the BHH and possibly performs an action based on the query
* @param {string} query - the query to do BHH actions
* @returns {string} - the page text
*/
string visit_bhh(string query) {
  string page = visit_url("bounty.php?pwd"+query);
  return page;
}

// @Overload
string visit_bhh() {
  return visit_bhh("");
}

/**
* Accepts the bounty of the given type
* @param {string} type - the bounty type
* @returns {boolean} - false if the given bounty is already accepted
*/
boolean accept_bounty(string type) {
  if(_accepted(type)) {
    return false;
  }

  visit_bhh("&action=take"+_bounty(type).kol_internal_type);
  visit_bhh();
  return true;
}

// @Overload
boolean accept_bounty(bounty b) {
  return accept_bounty(b.type);
}

/**
* Cancels the bounty of the given type
* @param {string} type - the bounty type
* @returns {boolean} - false if the given bounty is not already accepted
*/
boolean cancel_bounty(string type) {
  if(!_accepted(type)) {
    return false;
  }

  visit_bhh("&action=giveup_"+substring(_bounty(type).kol_internal_type, 0, 3));
  visit_bhh();
  return true;
}

/**
* Checks if the given monster is currently being hunted
* @param {monster} opp - the string representation of the monster to check
* @returns {boolean} - if the given monster is being hunted
*/
boolean is_hunted(monster opp) {
  // Checks if opp equals any of the current bounty monsters
  return opp == to_bounty(get_property("currentEasyBountyItem")).monster ||
         opp == to_bounty(get_property("currentHardBountyItem")).monster ||
         opp == to_bounty(get_property("currentSpecialBountyItem")).monster;
}

// @Overload
boolean is_hunted(string opp) {
  return is_hunted(to_monster(opp));
}

/**
* Returns the bounty with the smallest item count
* @returns {bounty} - the bounty with the smallest item count,
                      $bounty[none] if no bounties are available/active
*/
bounty optimal_bounty() {
  bounty[int] bounty_counts;
  bounty_counts[_bounty(EASY).number] = _bounty(EASY);
  bounty_counts[_bounty(HARD).number] = _bounty(HARD);
  bounty_counts[_bounty(SPECIAL).number] = _bounty(SPECIAL);

  int min = 696969;
  foreach count in bounty_counts {
    if(count < min && count != 0) {
      min = count;
    }
  }

  return bounty_counts[min];
}

/**
* Attempts to hunt the given bounty based on current settings and state:
*  - Will fax if bountiful.useFax is true and a fax is available
*  - Will use a copy if bountiful.useCopier is true and a copy is available
*  - Will adventure at a special content-unlockable location if the unlock
*    item costs less than or equal to bountiful.maxSpecialCost or the zone is
*    available
*  - Will adventure at a normal location if it is available
*  - Will give up a bounty if inaccessible and bountiful.automaticallyGiveup is true
* @param {bounty} b - the bounty to hunt
* @returns {boolean} - false if the bounty could not be hunted
*/
boolean hunt_bounty(bounty b) {
  accept_bounty(b.type); // doesn't do anything if already accepted
  print("There are " + _remaining(optimal_bounty().type).to_string() + " " +
        b.plural + " remaining!", "green");
  current = b;

  // TODO: Fax logic for doable inaccessible bounties
  if(useFax && !to_boolean(get_property("_photocopyUsed"))) {
    if(can_banish(_bounty(SPECIAL).location) ||
       (b.type == SPECIAL && !can_banish(_bounty(SPECIAL).location))) {
      faxbot(_bounty(b.type).monster);
      use_combat($item[photocopied monster], "combat");
    }
  // use copy if that's what we're doing and a copy is avilable
  } else if(useCopier && item_amount($item[Rain-Doh box full of monster]) > 0 &&
            to_monster(get_property("rainDohMonster")) == b.monster) {
    use_combat($item[Rain-Doh box full of monster], "combat");
  } else if(useCopier && item_amount($item[Spooky Putty monster]) > 0 &&
            to_monster(get_property("spookyPuttyMonster")) == b.monster) {
    use_combat($item[Spooky Putty monster], "combat");
  // if location is available or affordable, adventure there
  } else if(can_adv(b.location, false) ||
            (b.type == SPECIAL &&
            mall_price(CONTENT_ITEMS[b.location]) <= maxSpecial)) {
    if(useBan)
      buy_banishers();

    // unlock special zone if currently not available
    if(!can_adv(b.location, false))
      buy(1, CONTENT_ITEMS[b.location], maxSpecial);

    // prepare zone and check accessibility
    if(!can_adv(b.location, true))
      abort("Couldn't prepare the zone for some reason");

    adventure(1, b.location, "combat");
  } else {
    // turns out we're doing nothing
    print("Can't access the location of the bounty! Give up?", "orange");
    if(giveup) cancel_bounty(b.type); // automatically give up if unaccessible
    return false;
  }

  // refresh BHH information
  visit_bhh();
  return true;
}

// @Overload
boolean hunt_bounty(string b) {
  return hunt_bounty(_bounty(b));
}

//----------------------------------------
// Combat Functions

// TODO: Consolidate using a record for banishers
monster[item] get_used_item_banishers(location loc) {
  // Banished monster data is stored in the format by mafia:
  // monster1:item1:turn_used1:monster2:item2:turn_used2:etc...
  // TODO/BUG: Sometimes this property isn't updated?
  string[int] banish_data = get_property("banishedMonsters").split_string(":");

  monster[item] list;
  for(int i = 1; i < banish_data.count(); i += 3) {
    monster m = to_monster(banish_data[i - 1]);
    int[monster] invert;
    foreach id, em in get_monsters(loc) {
      invert[em] = id;
    }
    item it = to_item(banish_data[i]);
    if(invert contains m && it.combat) list[it] = m;
  }

  return list;
}

item get_unused_item_banisher(location loc) {
  monster[item] used = get_used_item_banishers(loc);

  foreach banisher in BAN_ITEMS {
    if(!(used contains banisher)) {
      return banisher;
    }
  }

  return $item[none];
}

monster[skill] get_used_skill_banishers(location loc) {
  // Banished monster data is stored in the format by mafia:
  // monster1:item1:turn_used1:monster2:item2:turn_used2:etc...
  string[int] banish_data = get_property("banishedMonsters").split_string(":");

  monster[skill] list;
  for(int i = 1; i < banish_data.count(); i += 3) {
    monster m = to_monster(banish_data[i - 1]);
    int[monster] invert;
    foreach id, em in get_monsters(loc) {
      invert[em] = id;
    }

    skill sk;
    // Special case handling
    if(banish_data[i] == "pantsgiving") {
      sk = $skill[Talk About Politics];
    } else {
      sk = to_skill(banish_data[i]);
    }
    if(invert contains m && sk.combat) list[sk] = m;
  }

  return list;
}

skill get_unused_skill_banisher(location loc) {
  monster[skill] used = get_used_skill_banishers(loc);

  foreach banisher in BAN_SKILLS {
    if(!(used contains banisher)) {
      if(banisher != $skill[Snokebomb] ||
         (banisher == $skill[Snokebomb] &&
         get_property("_snokebombUsed").to_int() < 3)) {
           return banisher;
         }
    }
  }

  return $skill[none];
}

/**
* Custom action filter, see here: http://wiki.kolmafia.us/index.php?title=Adventure
* Behavior includes:
*  - Copying bounty targets if bountiful.useCopier is true
*  - Banishing non-bounty targets if bountiful.useBanisher is true and the monster
*    is banishable
*  - (Unimplemented) free runaway from non-bounty targets if bountiful.useRunaway
*    is true
*  - Does CCS action otherwise
* @param {int} round   - the current combat round
* @param {monster} opp - the current enemy monster
* @param {string} text - the current page text
* @returns {boolean} - if the given monster is being hunted
*/
string combat(int round, monster opp, string text) {
  // Check if the current monster is hunted
  if(is_hunted(opp)) {
    print("Hey it's the bounty monster!", "blue");
    // Copy at the beginning of the fight if possible
    // TODO: Fix my_location() not working with copies (putty, etc)
    if(useCopier && (round == 0) && (_remaining(current) > 1)) {
      int doh_copies = get_property("_raindohCopiesMade").to_int();
      int putty_copies = get_property("spookyPuttyCopiesMade").to_int();

      if((doh_copies + putty_copies) < copies_available()) {
        if((item_amount($item[Rain-Doh black box]) > 0) &&
           (doh_copies < 5)) {
          return "item Rain-Doh black box";
        } else if(item_amount($item[Spooky Putty sheet]) > 0 &&
                 (putty_copies < 5)) {
          return "item Spooky Putty sheet";
        }
      }
    // Free kill if we're doing that
    } else if(useKill && item_amount($item[Power pill]) > 1 &&
              get_property("_powerPillUses").to_int() < 20) {
      return "item power pill";
    }
    // Ban logic
  } else if(useBan && can_banish(my_location())) {
    // Prefer skill banishes over items (they're free)
    skill skill_banisher = get_unused_skill_banisher(my_location());
    if(have_skill(skill_banisher)) {
      print("I have skill " + skill_banisher.to_string());
      return "skill " + to_string(skill_banisher);
    }

    item item_banisher = get_unused_item_banisher(my_location());

    // This should never happen but Mafia seems to occasionally not keep track
    // of banishes for some reason - TODO: Figure this out
    // BUG: This debugging code introduces a bug...
    /*
    if(LAST_BANISH == item_banisher && LAST_LOCATION == my_location()) {
      abort("Script picked the same banisher (" + LAST_BANISH.to_string() +
            ") twice in a row for the same location (" + LAST_LOCATION.to_string() + ")");
    }
    */

    if(item_amount(item_banisher) > 0) {
      // For debugging: see above comment
      LAST_BANISH = item_banisher;
      LAST_LOCATION = my_location();

      return "item " + to_string(item_banisher);
    }
  } else if(useRun) {
    // Use familiar run away
    if(my_familiar() == $familiar[Pair of Stomping Boots] ||
       (my_familiar() == $familiar[Frumious Bandersnatch] &&
        have_effect($effect[Ode to Booze]) > 0)) {

      // Yucky nested if statement
      if(get_property("_banderRunaways").to_int() < round(numeric_modifier("Familiar Weight")) / 5) {
        return "run away";
      }
    }
  }

  // Default to CCS if custom actions can't/don't need to happen
  return get_ccs_action(round);
}

//----------------------------------------
// Main Function

void main(string params) {
  string[int] args = split_string(params, " ");
  string doWhat = args[0];
  int arglen = count(args);

  // Command handling
  switch(doWhat) {
    case 'hunt':
      if(arglen > 1) {
        visit_bhh(); // refresh BHH status
        switch(args[1]) {
          // This will accept *ALL* easy/hard/special bounties
          // ie. if you have an easy from a previous day, it will do that one,
          // as well as the easy for the current day
          case 'easy':
            print("Hunting easy bounty!", "blue");
            while(_bounty(EASY) != $bounty[none]) {
              if(!hunt_bounty(_bounty(EASY))) break;
            }
            break;
          case 'hard':
            print("Hunting hard bounty!", "blue");
            while(_bounty(HARD) != $bounty[none]) {
              if(!hunt_bounty(_bounty(HARD))) break;
            }
            break;
          case 'special':
            print("Hunting special bounty!", "blue");
            while(_bounty(SPECIAL) != $bounty[none]) {
              if(!hunt_bounty(_bounty(SPECIAL))) break;
            }
            break;
          case 'optimal':
          case 'fastest':
          case 'best':
            print("Hunting optimal bounty!", "blue");
            bounty b = optimal_bounty();
            while(_bounty(b.type) != $bounty[none]) {
              if(!hunt_bounty(b)) break;
            }
            break;
          case 'all':
            print("Hunting all bounties!", "blue");
            while(optimal_bounty() != $bounty[none]) {
              if(!hunt_bounty(optimal_bounty())) break;
            }
            break;
          default:
            print("Invalid bounty type!", "red");
        }
      } else {
        print("No bounty type given!", "red");
      }
      break;
    default:
      print("Invalid command!", "red");
  }
}
