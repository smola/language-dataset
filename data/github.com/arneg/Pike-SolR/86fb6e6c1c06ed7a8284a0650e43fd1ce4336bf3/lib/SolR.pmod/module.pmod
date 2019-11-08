#ifdef SOLR_DEBUG
# define solr_debug(a ...)        werror(a)
#else
# define solr_debug(a ...)
#endif

typedef function(int(0..1),mapping|object(Error),mixed...:void) call_cb;

private void headers_ok() {}

class Error(mapping info) {
    string _sprintf(int type) {
        return sprintf("%O(%O)", this_program, info);
    }

    //!
    constant is_solr_error = 1;
}

class Timeout {
    inherit Error;

    //!
    constant is_timeout = 1;

    void create() {
        ::create(([ "name" : "http_timeout",
                    "error" : "Could not connect to SolR API." ]));
    }
}

class Instance {
    int timeout = 5;

    private void json_data_ok(object request, call_cb cb) {
        string s = request->data();
        mapping ret;
        
        // TODO: check return code

        if (mixed err = catch (ret = Standards.JSON.decode(s))) {
            solr_debug("Invalid JSON:\n%s\n----\n", s);
            // TODO: test this case for real
            cb(0, Error(([ "name" : "invalid_json", "error" : describe_error(err) ])));
            return;
        }

        if (ret->errors && sizeof(ret->errors) || ret->error) {
            cb(0, ret);
            return;
        }

        cb(1, ret);
    }

    private void xml_data_ok(object request, call_cb cb) {
        string s = request->data();
        mixed ret;
        
        // TODO: check return code
        //
        if (mixed err = catch (ret = Parser.XML.Tree.parse_input(s))) {
            // TODO: test this case for real
            cb(0, Error(([ "name" : "invalid_xml", "error" : describe_error(err) ])));
            return;
        }

        cb(1, ret);
    }

    private void fail(object request, call_cb cb) {
        cb(0, Timeout());
    }

    Protocols.HTTP.Session http = Protocols.HTTP.Session();
    Standards.URI url;

    void create(string host, int port) {
        url = Standards.URI(sprintf("http://%s:%d/solr/", host, port));
    }

    call_cb handle_timeout(call_cb cb, array(mixed) extra) {
        int called = 0;
        mixed timeout_id;

        if (!cb) error("Bad argument.\n");

        void my_cb(int ok, mapping|object(Error) res) {
            if (called) return;
            called = 1;
            remove_call_out(timeout_id);

            cb(ok, res, @extra);
        };

        void timeout_cb() {
            if (called) return;
            called = 1;

            cb(0, Timeout(), @extra);
        };

        timeout_id = call_out(timeout_cb, timeout);

        return my_cb;
    }

    //!
    void post(string path, mixed data, call_cb cb, mixed ... extra) {
        post_json(path, data, cb, @extra);
    }

    void post_json(string path, mixed data, call_cb cb, mixed ... extra) {
        Standards.URI rurl = Standards.URI(path, url);

        solr_debug("post_json: %O %O\n", rurl, data);

        http->async_do_method_url("POST", rurl, 0, string_to_utf8(Standards.JSON.encode(data)),
                                  ([ "Content-Type" : "application/json" ]),
                                  headers_ok, json_data_ok, fail, ({ handle_timeout(cb, extra) }));
    }

    void post_xml(string path, mixed data, call_cb cb, mixed ... extra) {
        Standards.URI rurl = Standards.URI(path, url);

        solr_debug("post_xml: %O %O\n", rurl, data);

        http->async_post_url(rurl, string_to_utf8(Standards.JSON.encode(data)),
                             headers_ok, xml_data_ok, fail, handle_timeout(cb, extra));
    }

    void get(string path, mapping args, call_cb cb, mixed ... extra) {
        get_json(path, args, cb, @extra);
    }

    void get_json(string path, mapping args, call_cb cb, mixed ... extra) {
        Standards.URI rurl = Standards.URI(path, url);

        if (!args) args = ([ "wt" : "json" ]);
        else args += ([ "wt" : "json" ]);

        solr_debug("get_json: %O\n", rurl);

        http->async_get_url(rurl, args||([]), headers_ok, json_data_ok, fail, handle_timeout(cb, extra));
    }

    void get_xml(string path, mapping args, call_cb cb, mixed ... extra) {
        Standards.URI rurl = Standards.URI(path, url);

        if (!args) args = ([ "wt" : "xml" ]);
        else args += ([ "wt" : "xml" ]);

        solr_debug("get_xml: %O\n", rurl);

        http->async_get_url(rurl, args||([]), headers_ok, xml_data_ok, fail, handle_timeout(cb, extra));
    }

    object admin = Admin(this);

    object collection(string name) {
        return Collection(this, name);
    }
}

mixed parse_node(object node) {
    switch (node->get_tag_name()) {
    case "lst": // mapping
        mapping ret = ([]);
        foreach (node->get_elements();; object elem) {
            string name = elem->get_attributes()->name;

            if (name) {
                ret[name] = parse_node(elem);
            }
        }
        return ret;
    case "arr": // array
        return map(node->get_elements(), parse_node);
    case "str":
        return node->value_of_node();
    case "int":
        return (int)node->value_of_node();
    }
}

mapping parse_response(object root) {
    mapping ret = ([ ]);

    mixed err = catch {
        root = root->get_elements("response")[0];

        foreach (root->get_elements();; object elem) {
            string name = elem->get_attributes()->name;

            if (name) {
                ret[name] = parse_node(elem);
            }
        }

    };

    if (err) return 0;
    return ret;
}

class Admin {
    Instance instance;

    void create(Instance instance) {
        this_program::instance = instance;
    }

    private void list_collections_cb(int(0..1) ok, mixed root, call_cb cb, mixed ... extras) {
        mapping response;

        response = parse_response(root);

        if (response) {
            cb(1, response->collections, @extras);
        } else {
            cb(0, root, @extras);
        }
    }

    void list_collections(call_cb cb, mixed ... extra) {
        instance->get_xml("admin/collections", ([ "action" : "LIST" ]), list_collections_cb, cb, @extra);
    }
}

class Collection {
    Instance instance;
    string name;

    void create(Instance instance, string name) {
        this_program::instance = instance;
        this_program::name = name;
    }

    //!
    void post(string path, mixed data, call_cb cb, mixed ... extra) {
        if (path) path = name + "/" + path;
        else path = name;

        instance->post(path, data, cb, @extra);
    }

    void get(string path, mapping args, call_cb cb, mixed ... extra) {
        if (path) path = name + "/" + path;
        else path = name;

        instance->get(path, args, cb, @extra);
    }

    void update(mapping|array(mapping) documents, call_cb cb, mixed ... extra) {
        post("update", documents, cb, @extra);
    }

    void commit(call_cb cb, mixed ... extra) {
        post("update", ([ "commit" : ([ ]) ]), cb, @extra);
    }

    void delete(array(mixed) ids, call_cb cb, mixed ... extra) {
        post("update", ([ "delete" : ids ]), cb, @extra);
    }

    object schema = Schema(this);

    void select(mapping args, call_cb cb, mixed ... extra) {
        get("select", args, cb, @extra);
    }
}

class Schema {
    Collection c;

    void create(Collection c) {
        this_program::c = c;
    }

    void post(string name, mixed|mapping data, call_cb cb, mixed ... extra) {
        if (name) name = "schema/" + name;
        else name = "schema";
        c->post(name, data, cb, @extra);
    }

    void get(string name, mapping args, call_cb cb, mixed ... extra) {
        if (name) name = "schema/" + name;
        else name = "schema";
        c->get(name, args, cb, @extra);
    }

    void retrieve(call_cb cb, mixed ... extra) {
        get(0, 0, cb, @extra);
    }

    void fields(call_cb cb, mixed ... extra) {
        get("fields", 0, cb, @extra);
    }

    void field(string name, call_cb cb, mixed ... extra) {
        get("fields/"+name, 0, cb, @extra);
    }

    void add_field_type(mapping config, call_cb cb, mixed ... extra) {
        post(0, ([ "add-field-type" : config ]), cb, @extra);
    }

    void delete_field_type(string name, call_cb cb, mixed ... extra) {
        post(0, ([ "delete-field-type" : ([ "name" : name ]) ]), cb, @extra);
    }

    void replace_field_type(mapping config, call_cb cb, mixed ... extra) {
        post(0, ([ "replace-field-type" : config ]), cb, @extra);
    }

    void add_field(mapping config, call_cb cb, mixed ... extra) {
        post(0, ([ "add-field" : config ]), cb, @extra);
    }

    void delete_field(string name, call_cb cb, mixed ... extra) {
        post(0, ([ "delete-field" : ([ "name" : name ]) ]), cb, @extra);
    }

    void replace_field(mapping config, call_cb cb, mixed ... extra) {
        post(0, ([ "replace-field" : config ]), cb, @extra);
    }

    void add_dynamic_field(mapping config, call_cb cb, mixed ... extra) {
        post(0, ([ "add-dynamic-field" : config ]), cb, @extra);
    }

    void delete_dynamic_field(string name, call_cb cb, mixed ... extra) {
        post(0, ([ "delete-dynamic-field" : ([ "name" : name ]) ]), cb, @extra);
    }

    void replace_dynamic_field(mapping config, call_cb cb, mixed ... extra) {
        post(0, ([ "replace-dynamic-field" : config ]), cb, @extra);
    }

    void add_copy_field(string src, array(string)|string dst, call_cb cb, mixed ... extra) {
        post(0, ([ "add-copy-field" : ([ "source" : src, "dest" : dst ]) ]), cb, @extra);
    }

    void delete_copy_field(string src, array(string)|string dst, call_cb cb, mixed ... extra) {
        post(0, ([ "delete-copy-field" : ([ "source" : src, "dest" : dst ]) ]), cb, @extra);
    }
}
