script "snapshot.ash";
notify chameco;
since r19083;

string EUDORA_CACHE = "";
string CAMPGROUND_CACHE = "";

boolean page_contains(string url, string name) {
	return contains_text(visit_url(url), name);
}

boolean has_item(string name) {
	item i = to_item(name);
	if(i == $item[none]) return false;

	if (item_amount(i) > 0 || closet_amount(i) > 0 || equipped_amount(i) > 0
		|| storage_amount(i) > 0 || display_amount(i) > 0 || shop_amount(i) > 0) {
		return true;
	}

	foreach fam in $familiars[] {
		if(have_familiar(fam) && fam != my_familiar()) {
			if(i == familiar_equipped_equipment(fam)) {
				return true;
			}
		}
	}

	if(get_campground() contains i) return true;
	return false;
}

boolean has_skill(string name) { return have_skill(to_skill(name)); }

boolean has_familiar(string name) { return have_familiar(to_familiar(name)); }

boolean has_property(string name) { return get_property(name).to_boolean(); }

boolean has_eudora(string name) {
	if (EUDORA_CACHE == "") EUDORA_CACHE = visit_url("account.php?tab=correspondence");
	return contains_text(EUDORA_CACHE, name);
}

boolean has_campground(string name) {
	if (CAMPGROUND_CACHE == "") CAMPGROUND_CACHE = visit_url("campground.php");
	return contains_text(CAMPGROUND_CACHE, name);
}

boolean has_iotm(int y, int m) {
	switch (y) {
	case 2004:
		switch (m) {
		case 10: return has_item("Dark Jill-O-Lantern") || has_familiar("Jill-O-Lantern");
		case 11: return has_item("hand turkey outline") || has_familiar("Hand Turkey");
		case 12: return has_item("crimbo elfling") || has_familiar("Crimbo Elf");
		default: return false;
		}
	case 2005:
		switch (m) {
		case  1: return has_item("orphan baby yeti") || has_familiar("Baby Yeti");
		case  2: return has_item("silk garter snake") || has_familiar("Feather Boa Constrictor");
		case  3: return has_item("lucky Tam O'Shanter") || has_item("lucky Tam O'Shatner");
		case  4: return has_item("personal raindrop") || has_familiar("Personal Raincloud");
		case  5: return has_item("miniature gravy-covered maypole");
		case  6: return has_item("deflated inflatable dodecapede") || has_familiar("Inflatable Dodecapede");
		case  7: return has_item("wax lips");
		case  8: return has_item("pygmy bugbear shaman") || has_familiar("Pygmy Bugbear Shaman");
		case  9: return has_item("Jekyllin hide belt");
		case 10: return has_item("doppelshifter egg") || has_familiar("Doppelshifter");
		case 11: return has_item("miniscule temporal rip") || has_familiar("Temporal Riftlet");
		case 12: return has_item("sweet nutcracker") || has_familiar("Sweet Nutcracker");
		default: return false;
		}
	case 2006:
		switch (m) {
		case  1: return has_item("Tome of Snowcone Summoning") || has_skill("Summon Snowcones");
		case  2: return has_item("iceberglet") || has_item("ice baby") || has_item("ice pick") || has_item("ice skates") || has_item("ice sickle");
		case  3: return has_item("March hat") || has_familiar("Wild Hare");
		case  4: return has_item("McPhee's Grimoire of Hilarious Object Summoning") || has_skill("Summon Hilarious Objects");
		case  5: return has_item("homeless hobo spirit") || has_familiar("Spirit Hobo");
		case  6: return has_item("astral badger") || has_familiar("Astral Badger");
		case  7: return has_item("jewel-eyed wizard hat");
		case  8: return has_item("Comma Chameleon egg") || has_familiar("Comma Chameleon");
		case  9: return has_item("travoltan trousers");
		case 10: return has_item("plastic pumpkin bucket");
		case 11: return has_item("pilgrim shield");
		case 12: return has_item("yuletide troll chrysalis") || has_familiar("Ancient Yuletide Troll");
		default: return false;
		}
	case 2007:
		switch (m) {
		case  1: return has_item("Great Ball of Frozen Fire") || has_item("liar's pants") || has_item("flaming juggler's balls") || has_item("flaming pink shirt") || has_item("flaming familiar doppelg&auml:nger") || has_item("evil flaming eyeball pendant");
		case  2: return has_item("Libram of Candy Heart Summoning") || has_skill("Summon Candy Heart");
		case  3: return has_item("dandy lion cub") || has_familiar("Dandy Lion");
		case  4: return has_item("bad penguin egg") || has_familiar("Penguin Goodfella");
		case  5: return has_item("Mayflower bouquet");
		case  6: return has_item("bottled green pixie") || has_familiar("Green Pixie");
		case  7: return has_item("bottle-rocket crossbow");
		case  8: return has_item("wizard action figure") || has_familiar("Wizard Action Figure");
		case  9: return has_item("navel ring of navel gazing");
		case 10: return has_item("class five ecto-larva") || has_familiar("Gluttonous Green Ghost");
		case 11: return has_item("V for Vivala mask");
		case 12: return has_item("Crimbo P. R. E. S. S. I. E.") || has_familiar("Crimbo P. R. E. S. S. I. E.");
		default: return false;
		}
	case 2008:
		switch (m) {
		case  1: return has_item("Libram of Divine Favors") || has_skill("Summon Party Favor");
		case  2: return has_item("naughty origami kit") || has_item("naughty paper shuriken") || has_item("origami pasties") || has_item("origami riding crop") || has_item("origami \"gentlemen's\" magazine") || has_item("naughty fortune teller");
		case  3: return has_item("sane hatrack") || has_familiar("Mad Hatrack");
		case  4: return has_item("Sp'n-Zor's Grimoire of &quot;Tasteful&quot; Gifts") || has_skill("Summon Tasteful Items");
		case  5: return has_item("packet of mayfly bait") || has_item("mayfly bait necklace");
		case  6: return has_item("llama lama cria") || has_familiar("Llama Lama");
		case  7: return has_item("little box of fireworks");
		case  8: return has_item("cotton candy cocoon") || has_familiar("Cotton Candy Carnie");
		case  9: return has_item("haiku katana");
		case 10: return has_item("spooky rattling cigar box") || has_familiar("Disembodied Hand");
		case 11: return has_item("Scratch 'n' Sniff Sticker Tome") || has_skill("Summon Stickers");
		case 12: return has_item("candy cornucopia") || has_familiar("Sugar Fruit Fairy");
		default: return false;
		}
	case 2009:
		switch (m) {
		case  1: return has_item("container of Spooky Putty") || has_item("Spooky Putty mitre") || has_item("Spooky Putty leotard") || has_item("Spooky Putty ball") || has_item("Spooky Putty sheet") || has_item("Spooky Putty snake") || has_item("Spooky Putty monster");
		case  2: return has_item("Libram of Love Songs") || has_skill("Summon Love Song");
		case  3: return has_item("Apathargic Bandersnatch") || has_familiar("Frumious Bandersnatch");
		case  4: return has_item("elvish sunglasses");
		case  5: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  6: return has_item("infant sandworm") || has_familiar("Baby Sandworm");
		case  7: return has_item("Bag o' Tricks");
		case  8: return has_item("floaty stone sphere") || has_familiar("He-Boulder");
		case  9: return has_item("Tome of Sugar Shummoning") || has_skill("Summon Sugar Sheets");
		case 10: return has_item("squamous polyp") || has_familiar("Squamous Gibberer");
		case 11: return has_item("moveable feast");
		case 12: return has_item("suspicious stocking") || has_familiar("Stocking Mimic");
		default: return false;
		}
	case 2010:
		switch (m) {
		case  1: return has_item("stinky cheese ball") || has_item("stinky cheese sword") || has_item("stinky cheese diaper") || has_item("stinky cheese wheel") || has_item("stinky cheese eye") || has_item("Staff of Queso Escusado");
		case  2: return has_item("Libram of BRICKOS") || has_skill("Summon BRICKOs");
		case  3: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  4: return has_item("panicked kernel") || has_familiar("Baby Bugged Bugbear");
		case  5: return has_item("crown of thrones");
		case  6: return has_item("glowing frisbee") || has_familiar("Rogue Program");
		case  7: return has_item("Juju Mojo Mask");
		case  8: return has_item("Schmalz's First Prize Beer") || has_familiar("Mini-Hipster");
		case  9: return has_item("Greatest American Pants");
		case 10: return has_item("organ grinder") || has_familiar("Knob Goblin Organ Grinder");
		case 11: return has_item("Grumpy Bumpkin's Pumpkin Seed Catalog") || has_item("packet of pumpkin seeds");
		case 12: return has_item("hibernating robot reindeer") || has_familiar("Robot Reindeer");
		default: return false;
		}
	case 2011:
		switch (m) {
		case 1: return has_item("Loathing Legion knife") || has_item("Loathing Legion abacus") || has_item("Loathing Legion can opener") || has_item("Loathing Legion chainsaw") || has_item("Loathing Legion corkscrew") || has_item("Loathing Legion defibrillator") || has_item("Loathing Legion double prism") || has_item("Loathing Legion electric knife") || has_item("Loathing Legion flamethrower") || has_item("Loathing Legion hammer") || has_item("Loathing Legion helicopter") || has_item("Loathing Legion jackhammer") || has_item("Loathing Legion kitchen sink") || has_item("Loathing Legion many-purpose hook") || has_item("Loathing Legion moondial") || has_item("Loathing Legion necktie") || has_item("Loathing Legion pizza stone") || has_item("Loathing Legion rollerblades") || has_item("Loathing Legion tape measure") || has_item("Loathing Legion tattoo needle") || has_item("Loathing Legion universal screwdriver");
		case  2: return has_item("a cute angel") || has_familiar("Obtuse Angel");
		case  3: return has_item("Sorcerers of the Shore Grimoire") || has_skill("Summon Alice's Army Cards");
		case  4: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  5: return has_item("My Own Pen Pal kit") || has_eudora("Pen Pal");
		case  6: return has_item("mysterious chest") || has_familiar("Li'l Xenomorph");
		case  7: return has_item("Operation Patriot Shield");
		case  8: return has_item("fairy-worn boots") || has_familiar("Pair of Stomping Boots");
		case  9: return has_item("Tome of Clip Art") || has_skill("Summon Clip Art");
		case 10: return has_item("Make-Your-Own-Vampire-Fangs kit") || has_item("plastic vampire fangs");
		case 11: return has_item("stuffed-shirt scarecrow") || has_familiar("Fancypants Scarecrow");
		case 12: return has_item("Mint Salton Pepper's Peppermint Seed Catalog") || has_item("Peppermint Pip Packet");
		default: return false;
		}
	case 2012:
		switch (m) {
		case  1: return has_item("Libram of Resolutions") || has_skill("Summon Resolutions");
		case  2: return has_item("can of Rain-Doh") || has_item("empty Rain-Doh can");
		case  3: return has_item("Small Medium") || has_familiar("Happy Medium");
		case  4: return has_item("Boris's Helm") || has_item("Boris's Helm (askew)");
		case  5: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  6: return has_item("Moping Artistic Goth Kid") || has_familiar("Artistic Goth Kid");
		case  7: return has_item("Camp Scout backpack");
		case  8: return has_item("Unagnimated Gnome") || has_familiar("Reagnimated Gnome");
		case  9: return has_item("box of bear arms") || (has_item("right bear arm") && has_item("left bear arm"));
		case 10: return has_item("Pete & Jackie's Dragon Tooth Emporium Catalog") || has_item("packet of dragon's teeth");
		case 11: return has_item("deactivated nanobots") || has_familiar("Nanorhino");
		case 12: return has_item("Thinknerd's Grimoire of Geeky Gifts") || has_skill("Summon Geeky Gifts");
		default: return false;
		}
	case 2013:
		switch (m) {
		case  1: return has_item("Snow Suit");
		case  2: return has_item("GameInformPowerDailyPro subscription card") || has_eudora("GameInformPowerDailyPro Magazine");
		case  3: return has_item("Jarlsberg's pan") || has_item("Jarlsberg's pan (Cosmic portal mode)");
		case  4: return has_item("Libram of Pulled Taffy") || has_skill("Summon Taffy");
		case  5: return has_item("Order of the Green Thumb Order Form") || page_contains("forestvillage.php", "friarcottage.gif");
		case  6: return has_item("adventurer clone egg") || has_familiar("Mini-Adventurer");
		case  7: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  8: return has_item("Folder Holder") || has_item("over-the-shoulder Folder Holder");
		case  9: return has_item("KoLHS Pep Squad Box") || has_familiar("Steam-Powered Cheerleader");
		case 10: return has_item("deanimated reanimator's coffin") || has_familiar("Reanimated Reanimator");
		case 11: return has_item("Pantsgiving");
		case 12: return has_item("The Smith's Tome") || has_skill("Summon Smithsness");
		default: return false;
		}
	case 2014:
		switch (m) {
		case  1: return has_item("Discontent™ Winter Garden Catalog") || has_item("packet of winter seeds");
		case  2: return has_item("Buddy Bjorn");
		case  3: return has_item("Sneaky Pete's leather jacket") || has_item("Sneaky Pete's leather jacket (collar popped)");
		case  4: return has_item("Little Geneticist DNA-Splicing Lab");
		case  5: return has_item("airplane charter: Spring Break Beach") || has_property("sleazeAirportAlways");
		case  6: return has_item("still grill") || has_familiar("Galloping Grill");
		case  7: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  8: return has_item("The Confiscator's Grimoire");
		case  9: return has_item("Thor's Pliers");
		case 10: return has_item("airplane charter: Conspiracy Island") || has_property("spookyAirportAlways");
		case 11: return has_item("fist turkey outline") || has_familiar("Fist Turkey");
		case 12: return has_item("Crimbo sapling") || has_familiar("Crimbo Shrub");
		default: return false;
		}
	case 2015:
		switch (m) {
		case  1: return has_item("Chateau Mantegna room key") || has_property("chateauAvailable");
		case  2: return has_item("bottle of lovebug pheromones") || has_property("lovebugsUnlocked");
		case  3: return has_item("Ed the Undying exhibit crate") || has_item("The Crown of Ed the Undying");
		case  4: return has_item("airplane charter: Dinseylandfill") || has_property("stenchAirportAlways");
		case  5: return has_item("portable Mayo Clinic");
		case  6: return has_item("yellow puck") || has_familiar("Puck Man") || has_item("yellow puck with a bow on it") || has_familiar("Ms. Puck Man");
		case  7: return has_item("Pack of Every Card") || has_item("Deck of Every Card");
		case  8: return has_item("airplane charter: That 70s Volcano") || has_property("hotAirportAlways");
		case  9: return has_item("shrine to the Barrel god") || has_property("barrelShrineUnlocked");
		case 10: return has_item("Haunted Doghouse");
		case 11: return has_item("airplane charter: The Glaciest") || has_property("coldAirportAlways");
		case 12: return has_item("machine elf capsule") || has_familiar("Machine Elf");
		default: return false;
		}
	case 2016:
		switch (m) {
		case  1: return has_item("X-32-F snowman crate") || has_property("snojoAvailable");
		case  2: return has_item("LT&T telegraph office deed") || has_property("telegraphOfficeAvailable");
		case  3: return has_item("Witchess Set");
		case  4: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  5: return has_item("disconnected intergnat") || has_familiar("Intergnat");
		case  6: return has_item("Source terminal");
		case  7: return has_item("detective school application") || has_property("hasDetectiveSchool");
		case  8: return has_item("DIY protonic accelerator kit") || has_item("protonic accelerator pack");
		case  9: return has_item("Dear Past Self Package") || has_item("Time-Spinner");
		case 10: return has_item("li'l orphan tot") || has_familiar("Trick-or-Treating Tot");
		case 11: return has_item("Granny Tood's Thanksgarden Catalog") || has_item("packet of thanksgarden seeds") || has_campground("A Thanksgarden");
		case 12: return has_item("Build-a-City Gingerbread kit") || has_property("gingerbreadCityAvailable");
		default: return false;
		}
	case 2017:
		switch (m) {
		case  1: return has_item("space planula") || has_familiar("Space Jellyfish");
		case  2: return has_item("heart-shaped crate") || has_property("loveTunnelAvailable");
		case  3: return has_item("unpowered Robortender") || has_familiar("Robortender");
		case  4: return has_item("Spacegate access badge") || has_property("spacegateAlways");
		case  5: return has_item("New-You Club Membership Form") || has_eudora("New-You Club");
		case  6: return has_item("suspicious package") || has_item("Kremlin's Greatest Briefcase");
		case  7: return has_item("LI-11 Motor Pool voucher") || has_item("Asdon Martin Keyfob");
		case  8: return has_item("Pocket Meteor Guide") || has_item("Pocket Meteor Guide (well-thumbed");
		case  9: return has_item("corked genie bottle") || has_item("genie bottle");
		case 10: return has_item("xo-skeleton-in-a-box") || has_familiar("XO Skeleton");
		case 11: return has_item("pantogram") || has_item("portable pantogram");
		case 12: return has_item("locked mumming trunk") || has_item("mumming trunk");
		default: return false;
		}
	case 2018:
		switch (m) {
		case  1: return has_item("January's Garbage Tote (unopened)") || has_item("January's Garbage Tote");
		case  2: return has_item("Clan VIP Lounge invitation") || has_item("Clan VIP Lounge key");
		case  3: return has_item("Pokéfam Guide to Capturing All of Them") || has_item("packet of tall grass seeds");
		case  4: return has_item("FantasyRealm membership packet") || has_property("frAlways");
		case  5: return has_item("God Lobster Egg") || has_familiar("God Lobster");
		case  6: return has_item("SongBoom&trade; BoomBox Box") || has_item("SongBoom&trade; BoomBox");
		case  7: return has_item("kitten burglar") || has_familiar("Cat Burglar");
		case  8: return has_item("Bastille Battalion control rig crate") || has_item("Bastille Battalion control rig");
		case  9: return has_item("Neverending Party invitation envelope") || has_property("neverendingPartyAlways");
		case 10: return has_item("latte lovers club card") || has_item("latte lovers member's mug");
		case 11: return has_item("voter registration form") || has_property("voteAlways");
		case 12: return has_item("Boxing Day care package") || has_property("daycareOpen");
		default: return false;
		}
	case 2019:
		switch (m) {
		case  1: return has_item("Kramco Industries packing carton") || has_item("Kramco Sausage-o-matic&trade;");
		default: return false;
		}
	default: return false;
	}
}

string build_json_iotm_list() {
	string ret = "[";
	if (has_iotm(2004, 10)) ret += "true"; else ret += "false";
	if (has_iotm(2004, 11)) ret += ",true"; else ret += ",false";
	if (has_iotm(2004, 12)) ret += ",true"; else ret += ",false";
	for (int y = 2005; y <= 2019; ++y) {
		for (int m = 1; m <= 12; ++m) {
			if (has_iotm(y, m)) ret += ",true"; else ret += ",false";
		}
	}
	return ret + "]";
}

string build_json_snapshot() {
	return "{\"iotm\":" + build_json_iotm_list() + "}";
}

string build_url_iotm_list() {
	string ret = "";
	if (has_iotm(2004, 10)) ret += "Y"; else ret += "N";
	if (has_iotm(2004, 11)) ret += "Y"; else ret += "N";
	if (has_iotm(2004, 12)) ret += "Y"; else ret += "N";
	for (int y = 2005; y <= 2019; ++y) {
		for (int m = 1; m <= 12; ++m) {
			if (has_iotm(y, m)) ret += "Y"; else ret += "N";
		}
	}
	return ret;
}

string build_url_snapshot(string server) {
	return server + "/coal/snapshot/mafia/" + get_player_id(my_name()) + "?iotms=" + build_url_iotm_list();
}

void main() {
	string server = "http://goodstuff.moe";
	if (contains_text(visit_url(build_url_snapshot(server), false, true), "successful")) {
		print("Snapshot upload success!", "green");
		print_html("<a href=\"" + server + "/coal/snapshot/#" + get_player_id(my_name()) + "\">Click here to view.</a>");
	} else {
		print("Failed to upload snapshot. Something is broken!", "red");
	}
}