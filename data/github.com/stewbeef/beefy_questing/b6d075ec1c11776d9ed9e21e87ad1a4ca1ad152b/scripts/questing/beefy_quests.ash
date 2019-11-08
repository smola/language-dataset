//Questing assistance
//
import "beefy_tools.ash";
script "beefy_quests.ash";

/////////////////////////////////////
//records
	record place
	{
		string type; //location, url
		string dest;
	};

	record qreqs
	{//simple list of things to have/get
		int level; //0 for no level
		boolean [class] classes;
		int meat; //0 for no meat
		int [item] things; // items and number of each
		boolean [skill] sks; //skills to have
		string [string] properties; //properties
	
	};qlist [monster] acts;//actions against monsters, if any

	record qstep
	{
		qreqs reqs; //requirements before beginning

		qreqs goals; //lrequirements for ending

		//During:
		place zone;//zone if any
		string qmood; //mood to use for quest
		string qgear; //style to equip
		int [string, item] itdos; // items and number of each
		boolean [string, skill] sks; //skills to use if have
		string combat_script;
		qlist after;
		
	};

	record quest
	{
		string [int] steps;
	}
/////////////////////////////////////
//quest records

boolean reqs_met(qlist req)
{
	boolean satisfied = true;
	if(req.meat < my_meat())
	{
		satisfied = false;
	}
	if(req.level < my_level())
	{
		satisfied = false;
	}
	foreach sk in req.sks
	{
		if (have_skill(sk) != req.sks[sk])
		{
			satisfied = false;
		}
	}
	if(! req.classes contains my_class() || req.classes.count() == 0)
	{
		satisfied = false;
	}
	foreach thing in req.things
	{
		if (item_amount(thing) < req.things[thing])
		{
			satisfied = false;
		}
	}
	foreach prop in req.properties
	{
		matcher m = create_matcher(req.properties[prop],get_propery(prop))
		if (!m.find())
		{
			satisfied = false;
		}
	}
	return satisfied;
}

void advloc(location loc, string script)
{
	string page_text = to_url(place).visit_url();
	string choiceAdventure = "-1";
	matcher m_choice = create_matcher("whichchoice value=(\\d+)", page_text);
	while(page_text.contains_text("choice.php"))
	{
		m_choice.reset(page_text);
      	m_choice.find();
		choiceAdventure = m_choice.group(1);
		string choiceprop = "choiceAdventure"+ choiceAdventure;
      	string choice_num = get_property(choiceprop);
		if(choice_num == "0") abort("Manual Control for "+ choiceprop);
      	if(choice_num == "") abort("Unsupported Choice Adventure!");

		page_text = visit_url("choice.php?pwd&whichchoice="+ choiceAdventure +"&option="+ choice_num);
	}
	if(page_text.contains_text("Combat"))
	{
    	run_combat(script);//specialized script
		if(page_text.contains_text("Combat"))
		{
			run_combat();//default script
		}
	}
   return choiceAdventure.to_int();
}

void questing(qstep step)
{
	cli_exectute("mood questing extends " + step.qmood);
	while(!reqs_met(step.goals))
	{
		if(step.zone.type == location)
		{
			advloc(to_location(step.zone.dest),-1,step.combat_script);
		}
		foreach thing in step.itdos["make"]
		{
			int quant = tep.itdos"make",thing];
			create(min(creatable_amount(thing),quant),thing);
		}
		foreach thing in step.itdos["use"]
		{
			int quant = tep.itdos["use",thing];
			use(min(item_amount(thing),quant),thing);
		}
	}
}


void main()
{

}