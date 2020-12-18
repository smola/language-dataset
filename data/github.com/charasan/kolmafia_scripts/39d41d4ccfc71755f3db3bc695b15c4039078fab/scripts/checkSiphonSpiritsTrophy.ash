/*
 *	Checks your consumption list vs the Siphoned Drinks list, tells you what you still need for the trophy
 */
script "checkSiphonSpiritsTrophy.ash"
notify charasan;
since r18480;

boolean loaded = false;
boolean [item] siphonedDrinks;

// Beasts
siphonedDrinks[$item[Zoodriver]] = false;
siphonedDrinks[$item[Sloe Comfortable Zoo]] = false;
siphonedDrinks[$item[Sloe Comfortable Zoo on Fire]] = false;
// Bugs
siphonedDrinks[$item[Grasshopper]] = false;
siphonedDrinks[$item[Locust]] = false;
siphonedDrinks[$item[Plague of Locusts]] = false;
// Constellations
siphonedDrinks[$item[Dark & Starry]] = false;
siphonedDrinks[$item[Black Hole]] = false;
siphonedDrinks[$item[Event Horizon]] = false;
// Constructs
siphonedDrinks[$item[Cement Mixer]] = false;
siphonedDrinks[$item[Jackhammer]] = false;
siphonedDrinks[$item[Dump Truck]] = false;
// Crimbo Elves
siphonedDrinks[$item[Lollipop Drop]] = false;
siphonedDrinks[$item[Candy Alexander]] = false;
siphonedDrinks[$item[Candicaine]] = false;
// Demons
siphonedDrinks[$item[Suffering Sinner]] = false;
siphonedDrinks[$item[Suppurating Sinner]] = false;
siphonedDrinks[$item[Sizzling Sinner]] = false;
// Dudes
siphonedDrinks[$item[Humanitini]] = false;
siphonedDrinks[$item[More Humanitini than Humanitini]] = false;
siphonedDrinks[$item[Oh, the Humanitini]] = false;
// Elements
siphonedDrinks[$item[Firewater]] = false;
siphonedDrinks[$item[Earth and Firewater]] = false;
siphonedDrinks[$item[Earth, Wind and Firewater]] = false;
// Fish
siphonedDrinks[$item[Caipiranha]] = false;
siphonedDrinks[$item[Flying Caipiranha]] = false;
siphonedDrinks[$item[Flaming Caipiranha]] = false;
// Goblins
siphonedDrinks[$item[Buttery Knob]] = false;
siphonedDrinks[$item[Slippery Knob]] = false;
siphonedDrinks[$item[Flaming Knob]] = false;
// Hippies
siphonedDrinks[$item[Fauna Libre]] = false;
siphonedDrinks[$item[Chakra Libre]] = false;
siphonedDrinks[$item[Aura Libre]] = false;
// Hobos
siphonedDrinks[$item[Mohobo]] = false;
siphonedDrinks[$item[Moonshine Mohobo]] = false;
siphonedDrinks[$item[Flaming Mohobo]] = false;
// Horrors
siphonedDrinks[$item[Great Older Fashioned]] = false;
siphonedDrinks[$item[Fuzzy Tentacle]] = false;
siphonedDrinks[$item[Crazymaker]] = false;
// Humanoids
siphonedDrinks[$item[Red Dwarf]] = false;
siphonedDrinks[$item[Golden Mean]] = false;
siphonedDrinks[$item[Green Giant]] = false;
// Mer-kins
siphonedDrinks[$item[Punchplanter]] = false;
siphonedDrinks[$item[Doublepunchplanter]] = false;
siphonedDrinks[$item[Haymaker]] = false;
// Orcs
siphonedDrinks[$item[Sazerorc]] = false;
siphonedDrinks[$item[Sazuruk-hai]] = false;
siphonedDrinks[$item[Flaming Sazerorc]] = false;
// Penguins
siphonedDrinks[$item[Herring Daiquiri]] = false;
siphonedDrinks[$item[Herring Wallbanger]] = false;
siphonedDrinks[$item[Herringtini]] = false;
// Pirates
siphonedDrinks[$item[Aye Aye]] = false;
siphonedDrinks[$item[Aye Aye, Captain]] = false;
siphonedDrinks[$item[Aye Aye, Tooth Tooth]] = false;
// Plants
siphonedDrinks[$item[Green Velvet]] = false;
siphonedDrinks[$item[Green Muslin]] = false;
siphonedDrinks[$item[Green Burlap]] = false;
// Slimes
siphonedDrinks[$item[Slimosa]] = false;
siphonedDrinks[$item[Extra-slimy Slimosa]] = false;
siphonedDrinks[$item[Slimebite]] = false;
// Undead
siphonedDrinks[$item[Drac & Tan]] = false;
siphonedDrinks[$item[Transylvania Sling]] = false;
siphonedDrinks[$item[Shot of the Living Dead]] = false;
// Weird
siphonedDrinks[$item[Drunken Philosopher]] = false;
siphonedDrinks[$item[Drunken Neurologist]] = false;
siphonedDrinks[$item[Drunken Astrophysicist]] = false;

void main()
{
	print("Checking consumption history...", "blue");
	string page = visit_url("showconsumption.php");
	page = replace_string(page, "<i>", "");
	page = replace_string(page, "</i>", "");

  int countFound = 0;
  
	matcher entry_matcher = create_matcher( "You have consumed the following food items:</b><p><table cellpadding=3><tr><td><table cellpadding=3>(.*)You have consumed the following booze items:</b><p><table><tr><td><table cellpadding=3>(.*)", page );

	if (entry_matcher.find())
	{
		loaded = true;

		string boozeCons = entry_matcher.group(2);

		matcher boozeMatcher = create_matcher("<a [^>]*>([^<]*)</a>", boozeCons);
		while(boozeMatcher.find())
		{
			string b = boozeMatcher.group(1);
      
      foreach sd in siphonedDrinks {
        if(sd.name == b) {
          countFound++;
          siphonedDrinks[sd] = true;
        }
      }
		}

    if(countFound < count(siphonedDrinks)) {
      print("Still need to consume the following:", "red");
      foreach sd in siphonedDrinks {
        if(!siphonedDrinks[sd]) {
          print("  " + sd.name, "maroon");
        }
      }
    } else {
      print("All siphoned drinks consumed! Enjoy that trophy!");
    }
	}
	else
	{
		print("Could not load consumption history", "red");
	}
}
