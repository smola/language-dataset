mapping(string:mixed) win=([]);

int main()
{
	array networks = ({ });
	string lastkwd = "";
	foreach (Process.run(({"iwlist","scan"}))->stdout/"\n", string line)
	{
		sscanf(line, "%*[ ]%n%s", int indent, line);
		switch (indent)
		{
			case 10:
				//New network block
				networks += ({(["IE":({ })])});
				sscanf(line, "Cell %*d - %s", line);
				//And fall through to grab the address or whatever else is on the same line
			case 20:
				if (sscanf(line, "Quality=%d/70", int qual))
				{
					networks[-1]->Quality = qual; //This one's formatted differently. (Why?)
					continue;
				}
				else if (sscanf(line, "%s:%s", string kwd, line))
				{
					lastkwd = kwd = String.trim_all_whites(kwd);
					if (lastkwd == "IE") networks[-1]->IE += ({0});
				}
				else error("Unrecognized line %O\n",line);
				//And fall through again
			case 24:
				//Append it to the info, if there is any.
				if (lastkwd == "IE")
					//Hack: This one is an array.
					networks[-1][lastkwd][-1] += "; " + String.trim_all_whites(line);
				else
					networks[-1][lastkwd] += "; " + String.trim_all_whites(line);
			//Otherwise drop it. There are some unindented lines that we don't care about.
		}
	}
	//Every piece of string data (ie everything except Quality) will begin "0; ". This can be ignored.
	//Okay. So now we know about networks. Let's zip through this and key them all by SSID.
	mapping(string:mapping(string:int|string)) network_info = ([]);
	array(string) info_headings = ({"SSID", "Quality", "Encryption", "Channel", "MAC addr"});
	array(array(string)) info_table = ({ });
	foreach (networks, mapping nw)
	{
		foreach (nw; string key; array|int|string val)
			if (stringp(val)) nw[key] = nw[key][3..]; //Trim off the "0; "
			else if (arrayp(val)) nw[key] = lambda(string x) {return x[3..];}(nw[key][*]); //Trim off ALL the "0; ". For some reason "nw[key][*][3..]" doesn't work.
		if (has_value(nw->ESSID, "\\x00")) continue; //Nulls don't seem to work too well. We probably don't care anyway.
		sscanf(m_delete(nw, "ESSID"), "%O", string ssid); if (!ssid) continue;//error("FIXME: Couldn't figure out an SSID\n");
		network_info[ssid] = nw;
		string enc;
		if (nw["Encryption key"] == "off") enc = "(none)";
		else foreach (nw->IE, string ie)
		{
			if (has_value(ie, "WPA2") && has_value(ie, "PSK")) {enc = "WPA2-PSK"; break;}
			if (has_value(ie, "WPA") && has_value(ie, "PSK")) {enc = "WPA-PSK";}
			//Detect others maybe
		}
		info_table += ({({ssid, sprintf("%d/70", nw->Quality), enc, nw->Channel, nw->Address})});
	}
	//Maybe TODO: Go through /etc/wpa_supplicant.conf and filter network_info to ones we don't already know
	if (!sizeof(info_table)) exit(0, "No wireless networks found (did you run this as root?)\n");
	GTK2.setup_gtk();
	object ls=GTK2.ListStore(({"string"})*sizeof(info_headings));
	foreach (info_table, array info)
	{
		object iter=ls->append();
		foreach (info; int i; string cell) ls->set_value(iter, i, cell);
	}
	object list = win->list = GTK2.TreeView(ls);
	if (sizeof(info_table) > 15)
	{
		list = GTK2.ScrolledWindow()->add(list);
		list->set_size_request(600,300);
	}
	win->mainwindow=GTK2.Window(0)->set_title("Register with new network")
		->add(GTK2.Vbox(0,10)
			->add(list)
			//TODO: Support WPA2 Enterprise and the possible need for user name
			//as well as passphrase
			->pack_start(GTK2.Hbox(0,0)
				->pack_start(GTK2.Label("Passphrase (if encrypted):"),0,0,0)
				->add(win->passphrase=GTK2.Entry())
			,0,0,0)
			->pack_start(win->add=GTK2.Button("Add!"),0,0,0)
		);
	foreach (info_headings; int i; string hdr) win->list->append_column(GTK2.TreeViewColumn(hdr, GTK2.CellRendererText(), "text", i));
	win->mainwindow->show_all();
	foreach (indices(this),string key) if (sscanf(key,"sig_%s_%s",string obj,string sig))
		win[obj]->signal_connect(sig,this[key]);
	return -1;
}

void sig_mainwindow_destroy() {exit(0);}

void sig_add_clicked()
{
	[object iter,object store]=win->list->get_selection()->get_selected();
	if (!iter) return; //Nothing selected?
	string ssid = store->get_value(iter, 0);
	string data;
	if (store->get_value(iter,2) == "(none)")
	{
		data = sprintf(#"network={
	ssid=\"%s\"
	key_mgmt=NONE
}
", ssid);
		write("Added unsecured network %s\n", ssid);
	}
	else
	{
		data = Process.run(({"wpa_passphrase", ssid, win->passphrase->get_text()}))->stdout;
		//Remove the cleartext passphrase.
		sscanf(data, "%s\t#psk=%*s\n%s", string q, string w);
		if (!w) exit(1, "Password could not be encrypted");
		data = q + w;
	}
	//exit(0, data); //Dry run
	Stdio.append_file("/etc/wpa_supplicant.conf",data);
	exit(0, "Saved.\n");
}
