script "Badger.ash";

# A badge-creation script by Tinsin
# with Frankensteined bits from cc_snapshot.ash

	boolean debug = false;
	void debug(string s)
	{
		if (debug) { print(s, "blue"); }
	}
	
	boolean verbose = true;
	boolean badges = true;
	int totalamt;

	string [string] results;
	

int i_a(string name)
{
	item i = to_item(name);
	if(i == $item[none])
	{
		return 0;
	}

	int amt = item_amount(i) + closet_amount(i) + equipped_amount(i) + storage_amount(i);
	amt += display_amount(i) + shop_amount(i);

	//Make a check for familiar equipment NOT equipped on the current familiar.
	foreach fam in $familiars[]
	{
		if(have_familiar(fam) && fam != my_familiar())
		{
			if(i == familiar_equipped_equipment(fam))
			{
				amt += 1;
			}
		}
	}

	//Thanks, Bale!
	if(get_campground() contains i) amt += 1;
	return amt;
}

boolean hasItem(string name)
{
	int amt = i_a(name);
	if(amt > 0)
	{
	return true;
	}else{
	return false;
	}
}

void checkList(string filename, string src)
{
	string [int] mylist;
	file_to_map(filename, mylist);
	print("Checking:" + src + "...", "olive");
	totalamt = 0;
	foreach x in mylist
	{
		if (hasItem(myList[x])){
			totalamt+=1;
			
			if (verbose){
			print("Have " + myList[x] + "!", "green");
			}
			
			
		}else{
		if (verbose){
			print("Lacking " + myList[x] + "...", "red");
			}
		}
	}
	print("You have " + totalamt + " out of " + count(mylist) + ".", "olive");
		// calculate percentage
		float amount = totalamt;
		float total = count(mylist);
		int altprog = amount/total*100;

	if (badges){
	
		// make a badge
		print("Shields.IO badge:");
		string progress =  altprog + "%25";

		string color;

		//choose color
		if (altprog >= 0){
			color = "red";
		}
		if (altprog > 20){
			color = "orange";
		}
		if (altprog > 40){
			color = "yellow";
		}
		if (altprog > 60){
			color = "yellowgreen";
		}
		if (altprog > 80){
			color = "green";
		}
		if (altprog == 100){
			color = "brightgreen";
		}

		string final = "https://img.shields.io/badge/" + src + "-" + progress + "-" + color + ".svg";	
		print(final);
	}
	
	
		results[src] = altprog;
	
}

void checkAllLists()
{
	verbose = false;
	badges = false;
	string [int] masterpost;
	file_to_map("badger-data/full.txt", masterpost);
	print("booting masterposter...");
	foreach x in masterpost
	{
	checkList("badger-data/" + masterpost[x] + ".txt", masterpost[x]);
	}
	map_to_file(results, "badger-data/_personal-badge-results.txt");
	print_html("<a href='https://tinsin.party/kol/badger.html'>CLICK HERE</a>");
	
}

void checkAllBasic()
{
	verbose = false;
	badges = false;
	string [int] masterpost;
	file_to_map("badger-data/full.basic.txt", masterpost);
	print("booting masterposter...");
	foreach x in masterpost
	{
	checkList("badger-data/" + masterpost[x] + ".txt", masterpost[x]);
	}
	map_to_file(results, "badger-data/_personal-badge-results.txt");
	print_html("<a href='https://tinsin.party/kol/badger.html'>CLICK HERE</a>");	
}


void checkAllIOTMs()
{
	verbose = false;
	badges = false;
	string [int] masterpost;
	file_to_map("badger-data/full.iotm.txt", masterpost);
	print("booting masterposter...");
	foreach x in masterpost
	{
	checkList("badger-data/" + masterpost[x] + ".txt", masterpost[x]);
	}
	map_to_file(results, "badger-data/_personal-badge-results.txt");
	print_html("<a href='https://tinsin.party/kol/badger.html'>CLICK HERE</a>");
	
}

void main(string whichFile)
{
	if (whichFile == "")
	{
	checkAllLists();
	}
	else if (whichFile == "IOTMs")
	{
	checkAllIOTMs();
	}
	else if (whichFile == "basic")
	{
	checkAllBasic();
	}
	else
	{
	checkList("badger-data/" + whichFile + ".txt", whichFile);
	}
}