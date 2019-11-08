protected {
    constant _BLOCK_ = "_BLOCK_.";
    constant FILE    = "FILE";

    bool debug = 1 || false;
    bool callbacks = false;
    bool ignore_missing = true;

    string content, filename, tpldir;
    string tag_start_delim   = "{",
           tag_end_delim     = "}",
           comment_delim     = "#",
           callback_delim    = "|",
           block_start_delim = "<!-- ",
           block_end_delim   = " -->",
           block_start_word  = "BEGIN:",
           block_end_word    = "END:",
           mainblock         = "main";

    mapping vars             = ([]);
    mapping blocks           = ([]);
    mapping parsed_blocks    = ([]);
    array(string) errors     = ({});
    string|array(string) tmpldir;
}

public void create(mapping(string:mixed) options)
{
    restart(options);
}

public void restart(mapping options)
{
    if (options["tpldir"])
        tpldir = options["tpldir"];

    filename= options->file;
    content = read_file(filename);
    blocks  = make_tree(content);
}

// FIXME: check object as var
public void assign(string|mapping var, void|mixed value, void|bool overwrite)
{
    if (false != overwrite)
        overwrite = true;

    if (mappingp(var))
    {
        foreach (var; mixed k; mixed v)
            vars[k] =v;
    }
    else if (mappingp(value) || arrayp(value))
    {
        if (overwrite || !vars[var])
            vars[var] = value;
        else
            if (arrayp(value))
                foreach (value, mixed v)
                    vars[var] += ({ v, });
            else
                foreach (value; mixed k; mixed v)
                    vars[var][k] = v;
    }
    else
        vars[var] = value;
}

public bool is_parsed(string block)
{
    return has_index(parsed_blocks, block);
}

public int out(void|string block)
{
    return write("%s", text(block));
}

public int out_file(string fn, void|string block)
{
    return Stdio.write_file( fn, text(block) );
}

public string text(void|string block)
{
    if (! block)
        block = mainblock;

    if (parsed_blocks[block])
        return parsed_blocks[block];
    return "";
}

public void rparse(string b)
{
    foreach (reverse(filter(indices(blocks), has_prefix, b)), string subb)
        parse(subb);
}

public void parse(string b)
{
    string input;
    string buf = "";
    int pos;

    if (blocks[b])
        input = blocks[b];
    else if (ignore_missing)
        set_error("parse: block [%s] does not exist", b);
    else
        error("parse: block [%s] does not exist", b);

    int done = 0;
    int len = sizeof(tag_start_delim+_BLOCK_);

    while (-1 < (pos = search(input, tag_start_delim+_BLOCK_, pos)))
    {
        // Store stuff up to marker
        buf  += input[..pos-1];
        input = input[pos+len..];
        pos   = search(input, tag_end_delim);
        // Append already parsed, found block
        if (parsed_blocks[ input[..pos-1] ])
            buf  += parsed_blocks[ input[..pos-1] ];
        input = input[pos+1..];
        // Reset pos, since we shift input,
        // otherwise it might seek out of range
        pos   = 0;

        done++;
    }
    buf = done ? buf + input : input;

    // TODO: add in depth recursive mech
    string get_value(array(string) varname) {
        if (sizeof(varname) == 2 &&
            vars[ varname[0] ]   &&
            vars[ varname[0] ][ varname[1] ])
            return vars[ varname[0] ][ varname[1] ];
        else if (sizeof(varname) == 1 &&
            vars[ varname[0] ])
            return vars[ varname[0] ];
        return 0;
    };

    // FIXME: no callbacks, no comments at this point
    sscanf(buf, "%{%*[^"+tag_start_delim+"]\{%[A-Za-z0-9\._\x7f-\xff]%}", array vs);
    if (sizeof(vs))
        foreach (Array.flatten(vs), string v)
            if (string val = get_value(v/"."))
                buf = replace(buf, tag_start_delim+v+tag_end_delim, val);

    parsed_blocks[b] = parsed_blocks[b] ? parsed_blocks[b] + buf : buf;
}

protected string read_file(string f)
{
    string fn;
    if (tpldir)
    {
        foreach (Array.arrayify(tpldir), string path)
            if (file_stat( fn = combine_path(path, f)))
                break;
    }
    string con = Stdio.read_file(fn);
    // FIXME: ignore error or die?
    if (! con)
        return "";

    string buf = "";
    // FIXME: triggers "Content without parent"
    if (false && debug)
        buf += "<!-- Start: "+f+" -->\n";

    int len = sizeof(tag_start_delim+FILE);
    int pos;

    while (-1 < (pos = search(con, tag_start_delim+FILE, pos)))
    {
        buf += con[..pos-1];
        con  = con[pos+len..];
        pos  = search(con, tag_end_delim);
        string subf = String.trim_all_whites( con[..pos-1] );
        if (subf[0] == '"')
        {
            subf = subf[1..sizeof(subf)-2];
            buf += read_file(subf);
        }
        /* FIXME: Variables in {FILE } includes are not supported yet
        else if (subf[0] == '{')
        {
            pos  = search(con, tag_end_delim, pos);
        } */
        // Special case were one can hit against a {FILENAME} variable
        else
            buf += tag_start_delim + FILE + subf + tag_end_delim;

        con  = con[pos+1..];
        if (con[0] == '}')
            con = con[1..];
    }
    buf += con;
    // FIXME: triggers "Content without parent"
    if (false && debug)
        buf += "<!-- End: "+f+" -->\n";
    return buf;
}

protected mapping make_tree(string con, void|string parent)
{
    int level = 0;
    string current;
    mapping blocks = ([]);
    array(string) block_path = ({});
    array(string) ar_content = con / block_start_delim;

    if (parent)
        block_path = parent / ".";

    foreach (ar_content, string l)
    {
        if (! sizeof(l))
            continue;

        parent = sizeof(block_path) ? block_path * "." : 0;
        if (has_prefix(l, block_start_word))
        {
            // "BEGIN: block3#You can comment a block like this -->\n"
            // FIXME simplify pattern
            if (4 == sscanf(l, block_start_word+"%*[ \t]%[^ \t"+comment_delim+"]%*s"+block_end_delim+"%s", string name, string body))
            {
                block_path += ({ name, });
                current = block_path * ".";
                blocks[current] = blocks[current] ? blocks[current] + body : body;
                if (blocks[parent])
                    blocks[parent] += tag_start_delim + _BLOCK_ + current + tag_end_delim;
            }
            else
                error("Match error: %s\n", l);
        }
        else if (has_prefix(l, block_end_word))
        {
            // <!-- END: block3#Or comment it here too -->foo
            // FIXME simplify pattern
            if (4 == sscanf(l, block_end_word+"%*[ \t]%[^ \t"+comment_delim+"]%*s"+block_end_delim+"%s", string name, string body))
            {
                [ name, block_path ] = Array.pop(block_path);
                parent = sizeof(block_path) ? block_path * "." : 0;
                if (blocks[parent])
                    blocks[parent] += body;
            }
            else
                error("Match error: %s\n", l);
        }
        else if (blocks[parent])
            blocks[parent] += block_start_delim + l;
        else
            error("Should not happen. Content without parent\n");
    }
    return blocks;
}

protected int set_error(mixed ... args)
{
    errors += ({ sprintf(@args), });
    return sizeof(errors);
}
