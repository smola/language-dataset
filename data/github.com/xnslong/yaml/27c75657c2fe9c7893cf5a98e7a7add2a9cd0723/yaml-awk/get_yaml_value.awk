BEGIN {
    if (substr(path,1,1) == sp) {
        path=substr(path,2);
    }
    max_matched_level=0;    # the max level that has matched on current path, the first node index is 1.
    checked_level=0;        # the level that has checked
    node_count=split(path, path_nodes, sp);
                            # after split, path_nodes[1] will be the part before first path node.
    result="";
}

decide_yaml_level($1) > checked_level {
    if (max_matched_level < checked_level) {
        next; # the upper level mismatched, just skip this line.
    } 

    level=decide_yaml_level($1);
    checked_level = level;   # forward checked_level level

    if (path_nodes[level] != $2) {
        next;                # this node mismatches
    }

    max_matched_level=level;
    if (level == node_count) {
        result=$3;
        exit 0;
    }
}

decide_yaml_level($1) <= checked_level {
    level=decide_yaml_level($1);
    checked_level=level;
    if (path_nodes[level] != $2) {
        max_matched_level=level - 1;
        next;
    }
    max_matched_level=level;
    if (level == node_count) {
        result=$3;
        exit 0;
    }
}

END { if (result != "") { print result; } }
