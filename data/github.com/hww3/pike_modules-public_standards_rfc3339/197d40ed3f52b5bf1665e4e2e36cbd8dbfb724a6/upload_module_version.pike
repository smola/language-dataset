// the original source of this file is the Tools.Monger source 
// distribution.

int main(int argc, array(string) argv)
{
  object m = Tools.Monger.MongerDeveloper();

  if(argc!=4) 
  {  
    werror("Usage: %s MODULE VERSION LICENSE\n", argv[0]);
    exit(1);
  }

  string module = argv[1];
  string version = argv[2];
  string license = argv[3];

  string changes = get_changes();

  object in = Stdio.FILE("stdin");

  Stdio.stdout.write("Have you updated the changelog and module version? ");
  string ans = in->gets();

  if(lower_case(ans[0..0]) != "y") exit(1);

  Stdio.stdout.write("Username: ");
  string user = in->gets();
  Stdio.stdout.write("Password: ");
  
  in->tcsetattr((["ECHO": 0]));
  string password = in->gets();
  in->tcsetattr((["ECHO": 1]));
  Stdio.stdout.write("\n");

  write("module: " + module + ", version " + version + "\n");
  write("license: " + license + "\n");
  write("changes: " + changes);

  m->set_auth(user, password);

  mixed err = catch(m->add_new_version(module, version, changes, license));
  if(err)
  {
    Stdio.stdout.write("An error occurred while adding the new version:\n" + 
      err[0] + "\nContinue anyway? ");
    string ans = in->gets();

    if(lower_case(ans[0..0]) != "y") exit(1);
    
  }
  m->set_dependency(module, version, "Pike", "7.6.0", "7.7.999", 1);
  m->set_module_source(module, version, replace(module, ".", "_") + "-" + version + ".tar.gz");

  return 0;
}

string get_changes()
{
  string changes = "";

  string changefile=Stdio.read_file("CHANGES");
  int started=0;

  foreach(changefile/"\n", string line)
  {
    if(!started && (Regexp("^Changes since ")->match(line)
             || Regexp("^Version [0-9]")->match(line)))
    {
      started = 1;
      continue;
    }
    else if(started && (Regexp("^Changes since ")->match(line)
             || Regexp("^Version [0-9]")->match(line)))
    {
      return changes;
    }

    else if(started) changes = changes + line + "\n";
    
  }

  return changes;
}
