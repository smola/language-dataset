inherit .Type;

import Public.Parser.XML2;
import .Constants;

string type="BOOLEAN";
string xsi_type = "boolean";

int contents = UNDEFINED;

void set(int|string val)
{
  if(val == 0 || val=="false")
    contents = 0;
  else if(intp(val) || val=="true")
    contents = 1;

  else
  {
    error("invalid boolean value %s\n", (string)val);
  }

  value_set = 1;
}

Node encode(Node b)
{
  Node n = b->add_child(new_node(name));

  if(ns) n->add_ns(ns, prefix);

  if(value_set)
    n->set_content((string)contents);
  n->set_attribute("xsi:type", "BODY-ENC:" + xsi_type);
  return n;
}

mixed get_value()
{
  return contents;
}

int get_native_type()
{
  return contents;
}
