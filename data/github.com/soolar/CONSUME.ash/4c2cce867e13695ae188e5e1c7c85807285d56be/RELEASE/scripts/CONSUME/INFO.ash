boolean is_salad(item it)
{
	boolean [item] saladList = $items[
		Crimbo salad,
		Delicious salad,
		Delicious star salad,
		Kudzu salad,
		Nutty organic salad,
		Primitive alien salad,
		Super salad,
		Tofu wonton,
	];
	return saladList contains it;
}

boolean is_beer(item it)
{
	boolean [item] beerList = $items[
		Alewife&trade; Ale,
		Amnesiac Ale,
		Bark rootbeer,
		Beertini,
		Blood Light,
		Bloody beer,
		Bottle of Fishhead 900-Day IPA,
		Bottle of Greedy Dog,
		Bottle of Lambada Lambic,
		Bottle of Old Pugilist,
		Bottle of Professor Beer,
		Bottle of Race Car Red,
		Bottle of Rapier Witbier,
		Breaded beer,
		can of Br&uuml;talbr&auml;u,
		Can of Drooling Monk,
		Can of Impetuous Scofflaw,
		Can of Swiller,
		Can of the cheapest beer,
		Cheap Chinese beer,
		Cinco Mayo Lager,
		Cobb's Knob Wurstbrau,
		Cold One,
		Cream stout,
		CSA cheerfulness ration,
		Cup of primitive beer,
		Day-old beer,
		Ginger beer,
		Green beer,
		Highest Bitter,
		Ice porter,
		Ice stein,
		Ice-cold fotie,
		Ice-cold Sir Schlitz,
		Ice-cold Willer,
		Imp Ale,
		Large tankard of ale,
		McMillicancuddy's Special Lager,
		Mt. Noob Pale Ale,
		Overpriced &quot;imported&quot; beer,
		Paint A Vulgar Pitcher,
		Party beer bomb,
		Pebblebr&auml;u,
		Plain old beer,
		Plastic cup of beer,
		Pumpkin beer,
		Ram's Face Lager,
		Red ale,
		Saison du Lune,
		Silver Bullet beer,
		Tankard of ale,
		Thriller Ice,
		TRIO cup of beer,
	];
	return beerList contains it;
}

boolean is_wine(item it)
{
	boolean [item] wineList = $items[
		Bartles and BRAAAINS wine cooler,
		Beignet Milgranet,
		Bilge wine,
		Blackfly Chardonnay,
		Blood-red mushroom wine,
		Bordeaux Marteaux,
		Bottle of cooking sherry,
		Bottle of fruity &quot;wine&quot;,
		Bottle of laundry sherry,
		Bottle of Pinot Renoir,
		Bottle of realpagne,
		Bottle of wine,
		Boxed champagne,
		Bucket of wine,
		Buzzing mushroom wine,
		Canteen of wine,
		Carrot claret,
		Complex mushroom wine,
		Cool mushroom wine,
		CRIMBCO wine,
		Cruelty-free wine,
		Dusty bottle of Marsala,
		Dusty bottle of Merlot,
		Dusty bottle of Muscat,
		Dusty bottle of Pinot Noir,
		Dusty bottle of Port,
		Dusty bottle of Zinfandel,
		Expensive champagne,
		Flaming mushroom wine,
		Flask of port,
		Flat mushroom wine,
		Flute of flat champagne,
		Fromage Pinotage,
		Gingerbread wine,
		Gloomy mushroom wine,
		High-end ginger wine,
		Icy mushroom wine,
		Knob mushroom wine,
		Knoll mushroom wine,
		Lumineux Limnio,
		Magnum of fancy champagne,
		Mid-level medieval mead,
		Missing wine,
		Morto Moreto,
		Mulled berry wine,
		Muschat,
		Oily mushroom wine,
		Overpowering mushroom wine,
		Plum wine,
		Pointy mushroom wine,
		Psychotic Train wine,
		Red red wine,
		Sacramento wine,
		Smooth mushroom wine,
		Space port,
		Spooky mushroom wine,
		Stinky mushroom wine,
		Supernova Champagne,
		Swirling mushroom wine,
		Temps Tempranillo,
		Thistle wine,
		Warbear bearserker mead,
		Warbear blizzard mead,
		Warbear feasting mead,
		White wine,
		Ye Olde Meade,
	];
	return wineList contains it;
}

boolean is_martini(item it)
{
	boolean [item] martiniList = $items[
		dry martini,
		martini,
		dry vodka martini,
		gibson,
		vodka martini,
		vodka gibson,
		rockin' wagon,
		soft green echo eyedrop antidote martini,
	];
	return martiniList contains it;
}

boolean is_saucy(item it)
{
	boolean [item] saucyList = $items[
		cold hi mein,
		devil hair pasta,
		Fettris,
		fettucini Inconnu,
		fleetwood mac 'n' cheese,
		fusillocybin,
		gnocchetti di Nietzsche,
		Hell ramen,
		hot hi mein,
		libertagliatelle,
		linguini immondizia bianco,
		linguini of the sea,
		prescription noodles,
		shells a la shellfish,
		sleazy hi mein,
		spagecialetti,
		spaghetti con calaveras,
		spaghetti with Skullheads,
		spooky hi mein,
		stinky hi mein,
		turkish mostaccioli,
	];
	return saucyList contains it;
}

boolean is_lasagna(item it)
{
	boolean [item] lasagnaList = $items[
		gnat lasagna,
		long pork lasagna,
		fishy fish lasagna,
	];
	return lasagnaList contains it;
}

boolean is_monday()
{
	return numeric_modifier($item[tuesday's ruby], "muscle percent") == 5.0;
}

int daily_limit(item it)
{
	switch(it)
	{
		case $item[mojo filter]:
			return 3 - get_property("currentMojoFilters").to_int();
		case $item[spice melange]:
			return get_property("spiceMelangeUsed").to_boolean() ? 0 : 1;
		case $item[ultra mega sour ball]:
			return get_property("_ultraMegaSourBallUsed").to_boolean() ? 0 : 1;
		case $item[sweet tooth]:
			return get_property("_sweetToothUsed").to_boolean() ? 0 : 1;
		case $item[fudge spork]:
			return get_property("_fudgeSporkUsed").to_boolean() ? 0 : 1;
		case $item[essential tofu]:
			return get_property("_essentialTofuUsed").to_boolean() ? 0 : 1;
		case $item[blue mana]:
			return 10 - get_property("_ancestralRecallCasts").to_int();
		case $item[alien animal milk]:
			return get_property("_alienAnimalMilkUsed").to_boolean() ? 0 : 1;
		case $item[alien plant pod]:
			return get_property("_alienPlantPodUsed").to_boolean() ? 0 : 1;
		case $item[affirmation cookie]:
			return get_property("_affirmationCookieEaten").to_boolean() ? 0 : 1;
		case $item[Hunger&trade; Sauce]:
			return get_property("_hungerSauceUsed").to_boolean() ? 0 : 1;
		case $item[cuppa Voraci tea]:
			return get_property("_voraciTeaUsed").to_boolean() ? 0 : 1;
		case $item[cuppa Sobrie tea]:
			return get_property("_sobrieTeaUsed").to_boolean() ? 0 : 1;
		case $item[lupine appetite hormones]:
			return get_property("_lupineHormonesUsed").to_boolean() ? 0 : 1;
		// batfellow consumables
		case $item[Kudzu salad]:
		case $item[Mansquito Serum]:
		case $item[Miss Graves' vermouth]:
		case $item[The Plumber's mushroom stew]:
		case $item[The Author's ink]:
		case $item[The Mad Liquor]:
		case $item[Doc Clock's thyme cocktail]:
		case $item[Mr. Burnsger]:
		case $item[The Inquisitor's unidentifiable object]:
			return 1;
		// TODO: MOOOOOOOOOOOOOORE
		default: return -1;
	}
}

int accordion_buff_duration(item accordion)
{
	switch(accordion)
	{
		case $item[stolen accordion]:
		case $item[toy accordion]:
			return 5;
		case $item[beer-battered accordion]:
			return 6;
		case $item[baritone accordion]:
		case $item[calavera concertina]:
			return 7;
		case $item[mama's squeezebox]:
			return 8;
		case $item[guancertina]:
			return 9;
		case $item[accord ion]:
		case $item[accordion file]:
		case $item[Aerogel accordion]:
		case $item[Antique accordion]:
		case $item[Bal-musette accordion]:
		case $item[Cajun accordion]:
		case $item[quirky accordion]:
		case $item[Rock and Roll Legend]:
		case $item[Skipper's accordion]:
		case $item[warbear exhaust manifold]:
			return 10;
		case $item[bone bandoneon]:
			return 11;
		case $item[pentatonic accordion]:
			return 12;
		case $item[Accordion of Jordion]:
			return 14;
		case $item[autocalliope]:
		case $item[non-Euclidean non-accordion]:
		case $item[Shakespeare's Sister's Accordion]:
		case $item[Squeezebox of the Ages]:
			return 15;
		case $item[ghost accordion]:
			return 16;
		case $item[pygmy concertinette]:
			return 17;
		case $item[accordionoid rocca]:
			return 18;
		case $item[peace accordion]:
			return 19;
		case $item[alarm accordion]:
		case $item[The Trickster's Trikitixa]:
		case $item[zombie accordion]:
			return 20;
		default:
			return 0;
	}
}

boolean is_legal_accordion(item it)
{
	boolean [item] legalAccordions = $items[
		toy accordion,
		antique accordion,
		aerogel accordion,
	];
	return legalAccordions contains it;
}

int my_accordion_buff_duration()
{
	int longest = 0;
	foreach it in get_inventory()
	{
		int duration = accordion_buff_duration(it);
		if(duration > longest && (my_class() == $class[Accordion Thief]
			|| is_legal_accordion(it)))
			longest = duration;
	}
	if(item_amount($item[jewel-eyed wizard hat]) > 0)
		longest += 5;
	return longest;
}

item get_class_chocolate(class c)
{
	switch(c)
	{
		case $class[Seal Clubber]: return $item[chocolate seal-clubbing club];
		case $class[Turtle Tamer]: return $item[chocolate turtle totem];
		case $class[Pastamancer]: return $item[chocolate pasta spoon];
		case $class[Sauceror]: return $item[chocolate saucepan];
		case $class[Disco Bandit]: return $item[chocolate disco ball];
		case $class[Accordion Thief]: return $item[chocolate stolen accordion];
		default: return $item[none];
	}
}

boolean is_bloody(item it)
{
	return $items[
		bloodstick,
		actual blood sausage,
		blood snowcone,
		blood roll-up,
		blood-soaked sponge cake,
		bottle of Sanguiovese,
		mulled blood,
		Red Russian,
		dusty bottle of blood,
		vampagne,
	] contains it;
}
