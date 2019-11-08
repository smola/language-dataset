Module:    httpi
Synopsis:  Serve static files and directory listings
Author:    Carl Gay
Copyright: See LICENSE in this distribution for details.


// Serve static file content from the given directory.
//
define open class <directory-resource> (<resource>)

  constant slot resource-directory :: <directory-locator>,
    required-init-keyword: directory:;

  // Whether to allow directory listings.
  // May be overridden for specific directories.
  // Default is to be secure.
  constant slot allow-directory-listing? :: <boolean> = #f,
    init-keyword: allow-directory-listing?:;

  // Whether to allow serving documents that are outside of the directory
  // and are accessed via a symlink from within the directory.  Default is
  // to be secure.
  constant slot follow-symlinks? :: <boolean> = #f,
    init-keyword: follow-symlinks?:;

  // The set of file names that are searched for when a directory URL is
  // requested.  They are searched in order, and the first match is chosen.
  // TODO: Probably these shouldn't specify a filename extension, so that
  //       the media type can be chosen via content negotiation.
  constant slot default-documents :: <list>
    = list(as(<file-locator>, "index.html"),
           as(<file-locator>, "index.htm")),
    init-keyword: default-documents:;

  // Name taken from Apache.  If #t then when a static file document 'foo'
  // is requested and doesn't exist, we search for foo.* files instead,
  // and choose one based on the Accept header.
  constant slot allow-multi-views? :: <boolean> = #f,
    init-keyword: allow-multi-views?:;

end class <directory-resource>;

// For convenience, convert the directory: init arg to <directory-locator>
//
define method make
    (class :: subclass(<directory-resource>), #rest args, #key directory)
 => (resource :: <directory-resource>)
  apply(next-method, class,
        directory: as(<directory-locator>, directory),
        args)
end;

// Don't err if unmatched suffix remains
define method unmatched-url-suffix
    (resource :: <directory-resource>, suffix :: <sequence>)
end;

define method respond-to-get
    (policy :: <directory-resource>, #key) => ()
  let suffix :: <string> = request-url-path-suffix(current-request());
  
  // remove leading slash
  if (suffix.size > 0 & suffix[0] = '/')
    suffix := copy-sequence(suffix, from: 1);
  end;

  let document :: <locator> = locator-from-relative-path(policy, suffix);
  log-debug("static file: document = %s", as(<string>, document));

  if (~file-exists?(document))
    document := find-multi-view-file(policy, document)
                  | %resource-not-found-error();
    log-debug("static file: multi-view document = %s", as(<string>, document));
  end;

  if (file-type(document) == #"link")
    if (follow-symlinks?(policy))
      document := follow-links(link-target(document), policy);
      log-info("static file: linked document = %s", document);
    else
      forbidden-error();
    end;
  end if;

  block (return)
    let ftype = file-type(document);
    if (ftype = #"directory")
      let index = find-default-document(policy, document);
      if (index)
        // Let the index file be processed like a regular file, below.
        document := index;
      elseif (policy.allow-directory-listing?)
        serve-directory(policy, document);
        return();
      else
        %resource-not-found-error()
      end;
    end;

    // It's a regular file...
    let (etag, weak?) = etag(document);
    let request :: <request> = current-request();
    let response :: <response> = current-response();
    set-header(response, iff(weak?, "W/ETag", "ETag"), etag);
    let client-etag = get-header(request, "If-None-Match");
    if (etag = client-etag)
      not-modified-redirect(headers: response.response-headers);
    else
      serve-static-file(policy, document);
    end;
  end block;
end method respond-to-get;

// Merges the given path against the resource directory and ensures that the
// resulting locator refers to a (possibly non-existent) document below the
// resource directory.  If not, it signals an error.
//
define function locator-from-relative-path
    (resource :: <directory-resource>, relative-path :: <string>)
 => (locator :: <locator>)
  if (empty?(relative-path))
    resource.resource-directory
  else
    // this is hacky.  really we need to check the file system
    // to know whether to make a directory locator or not.
    let class = iff(relative-path[relative-path.size - 1] = '/',
                    <directory-locator>,
                    <file-locator>);
    let locator = merge-locators(as(class, relative-path),
                                 resource.resource-directory);
    let locator = simplify-locator(locator);
    if (locator-below-root?(locator, resource.resource-directory))
      locator
    else
      %resource-not-found-error()
    end
  end
end function locator-from-relative-path;

// Follow symlink chain.  If the target is outside the policy directory and
// the given policy disallows that, signal 404 error.
//
define function follow-links
    (document :: <pathname>, policy :: <directory-resource>)
 => (target :: <pathname>)
  if ( ~(file-exists?(document)
           & locator-below-root?(document, policy.resource-directory)))
    %resource-not-found-error();
  elseif (file-type(document) == #"link")
    follow-links(link-target(document), policy)
  else
    document
  end
end function follow-links;

define method find-default-document
    (policy :: <directory-resource>, locator :: <directory-locator>)
 => (locator :: false-or(<file-locator>))
  block (return)
    for (default in policy.default-documents)
      let document = merge-locators(default, locator);
      if (~file-exists?(document))
        document := find-multi-view-file(policy, document);
      end;
      if (document
            & file-exists?(document)
            & file-type(document) = #"file")
        return(document)
      end;
    end;
    #f
  end
end method find-default-document;

// I can't make any sense of this.  --cgay
define method locator-below-root?
    (locator :: <physical-locator>, root :: <directory-locator>)
 => (below? :: <boolean>)
  let relative = relative-locator(locator, root);
  // do they at least share a common ancestor?
  if (locator-relative?(relative))
    let relative-parent = locator-directory(relative);
    // is it a file directly in the root dir?
    ~relative-parent
      | begin
          let relative-path = locator-path(relative-parent);
          // again, is it directly in the root dir?
          empty?(relative-path)
            | relative-path[0] ~= #"parent"  // does it start with ".."?
        end;
  end if;
end method locator-below-root?;

define method locator-media-type
    (locator :: <locator>, resource :: <directory-resource>)
 => (media-type :: <media-type>)
  let extension = locator.locator-extension;
  let mtype = extension & extension-to-mime-type(extension,
                                                 *server*.server-media-type-map);
  if (mtype)
    mtype
  else
    let mtype :: <mime-type>
                 = string-to-mime-type(default-content-type(resource));
    make(<media-type>,
         type: mtype.mime-type,
         subtype: mtype.mime-subtype)
  end
end method locator-media-type;

define method serve-static-file
    (policy :: <directory-resource>, locator :: <locator>)
  log-debug("Serving static file: %s", as(<string>, locator));
  let response = current-response();
  with-open-file(in-stream = locator, direction: #"input", if-does-not-exist: #f,
                 element-type: <byte-character>)
    set-header(response, "Content-Type",
               mime-name(locator-media-type(locator, policy)));
    let mod-date = file-prop(locator, #"modification-date");
    if (mod-date)
      set-header(response, "Last-Modified", as-rfc1123-string(mod-date));
    end;
    copy-to-end(in-stream, response);
  end;
end method serve-static-file;

define method copy-to-end
    (in-stream :: <stream>, out-stream :: <stream>)
  let buffer-size :: <integer> = 8092;
  let buffer :: <sequence> = make(stream-sequence-class(in-stream),
                                  size: buffer-size);
  iterate loop ()
    let count = read-into!(in-stream, buffer-size, buffer, on-end-of-stream: #f);
    write(out-stream, buffer, end: count);
    if (count = buffer-size)
      loop()
    end;
  end;
end method copy-to-end;

define method etag 
    (locator :: <locator>)
 => (etag :: <string>, weak? :: <boolean>)
  //generate an etag (use modification date and size)
  // --TODO: algorithm should be changed (md5?), because a file can
  //change more than once per second without changing size.
  let now = current-date();
  let timestamp = file-prop(locator, #"modification-date");
  let time = (date-hours(timestamp) * 60 +
              date-minutes(timestamp)) * 60 +
             date-seconds(timestamp);
  let date = (date-year(timestamp) * 1000 +
              date-month(timestamp)) * 100 +
             date-day(timestamp);
  let weak :: <boolean> = #f;
  let dur :: <day/time-duration> = make(<day/time-duration>, days: 0, seconds: 1);
  if (now < timestamp + dur)
    weak := #t;
  end if;
  values(concatenate("\"", integer-to-string(date, base: 16), "-",
                     integer-to-string(time, base: 16), "-",
                     integer-to-string(file-prop(locator, #"size"), base: 16), "\""),
                     weak);
end method etag;

// Serves up a directory listing as HTML.  The caller has already verified that this
// locator names a directory, even though it may be a <file-locator>, and that the
// directory it names is under the document root.
//---TODO: add image links.  deal with access control.
define method serve-directory
    (resource :: <directory-resource>, locator :: <locator>)
  let response :: <response> = current-response();  
  let loc :: <directory-locator>
    = iff(instance?(locator, <directory-locator>),
          locator,
          subdirectory-locator(locator-directory(locator), locator-name(locator)));
  let mod-date = file-prop(locator, #"modification-date");
  set-header(response, "Last-Modified", as-rfc1123-string(mod-date));
  set-header(response, "Content-type", "text/html");
  let stream = response;
  local
    method show-file-link (directory, name, type)
      unless (name = ".." | name = ".")
        let locator = iff(type = #"directory",
                          subdirectory-locator(as(<directory-locator>, directory), name),
                          merge-locators(as(<file-locator>, name),
                                         as(<directory-locator>, directory)));
        let link = iff(type = #"directory",
                       concatenate(name, "/"),
                       name);
        write(stream, "\t\t\t\t<tr>\n");
        format(stream, "\t\t\t\t<td class=\"name\"><a href=\"%s\">%s</a></td>\n",
               link, link);
        let mime-type = iff(type = #"file",
                            mime-name(locator-media-type(locator, resource)),
                            "");
        format(stream, "\t\t\t\t<td class=\"mime-type\">%s</td>\n", mime-type);
        for (key in #[#"size", #"modification-date", #"author"],
             alignment in #["right", "left", "left"])
          let prop = file-prop(locator, key);
          format(stream, "\t\t\t\t<td align=\"%s\" class=\"%s\">",
                 alignment, as(<string>, key));
          display-file-property(stream, key, prop | "-", type);
          write(stream, "\t\t\t\t</td>\n");
        end;
        write(stream, "\t\t\t</tr>\n");
      end;
    end;
  let url = request-url(current-request());
  format(stream,
         "<?xml version=\"1.0\"?>\n"
         "<!DOCTYPE html PUBLIC \"-//W3C/DTD XHTML 1.0 Strict//EN\""
         " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
         "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
         "\t<head>\n"
         "\t\t<title>Index of %s</title>\n"
         "\t</head>\n", url);
  format(stream, "\t<body>\n");
  format(stream, "\t\t<table cellspacing=\"4\">\n");
  format(stream, "\t\t\t<caption>Directory listing for %s</caption>\n", url);
  write(stream,  "\t\t\t<col id=\"name\" />\n"
                 "\t\t\t<col id=\"mime-type\" />\n"
                 "\t\t\t<col id=\"size\" />\n"
                 "\t\t\t<col id=\"modification-date\" />\n"
                 "\t\t\t<col id=\"author\" />\n");
  write(stream,  "\t\t\t<thead>\n"
                 "\t\t\t\t<tr>\n"
                 "\t\t\t\t\t<th align=\"left\">Name</th>\n"
                 "\t\t\t\t\t<th align=\"left\">MIME Type</th>\n"
                 "\t\t\t\t\t<th align=\"right\">Size</th>\n"
                 "\t\t\t\t\t<th align=\"left\">Date</th>\n"
                 "\t\t\t\t\t<th align=\"left\">Author</th>\n"
                 "\t\t\t\t</tr>\n"
                 "\t\t\t</thead>\n");
  write(stream,  "\t\t\t<tbody>\n");
  let docroot :: <directory-locator> = resource.resource-directory;
  unless (loc = docroot
          | (instance?(loc, <file-locator>)
             & locator-directory(loc) = docroot))
    write(stream,
          "\t\t\t\t<tr>\n"
          "\t\t\t\t\t<td class=\"name\"><a href=\"../\">../</a></td>\n"
          "\t\t\t\t\t<td class=\"type\"></td>\n"
          "\t\t\t\t\t<td class=\"size\"></td>\n"
          "\t\t\t\t\t<td class=\"modification-date\"></td>\n"
          "\t\t\t\t\t<td class=\"author\" />\n"
          "\t\t\t\t</tr>\n");
  end unless;
  do-directory(show-file-link, loc);
  write(stream,
        "\t\t\t</tbody>\n"
        "\t\t</table>\n"
        "\t</body>\n"
        "</html>\n");
end method serve-directory;

define method display-file-property
    (stream, key, property, file-type :: <file-type>) => ()
end;

define method display-file-property
    (stream, key, property :: <date>, file-type :: <file-type>) => ()
  write(stream, as-iso8601-string(property));
end;

define method display-file-property
    (stream, key == #"size", property, file-type :: <file-type>) => ()
  if (file-type == #"file")
    let kilobyte = round/(property, 1024);
    let megabyte = round/(kilobyte, 1024);
    let gigabyte = round/(megabyte, 1024);
    if (gigabyte > 0)
      format(stream, "%d GB", gigabyte);
    elseif (megabyte > 0)
      format(stream, "%d MB", megabyte);
    elseif (kilobyte > 0)
      format(stream, "%d KB", kilobyte);
    else
      format(stream, "%d B", property);
    end if;
  else
    write(stream, "");
  end if;
end;

define method display-file-property
    (stream, key, property :: <string>, file-type :: <file-type>) => ()
  format(stream, property);
end;

define function file-prop
    (locator :: <locator>, property-name :: <symbol>)
 => (property-value)
  block ()
    file-property(locator, property-name)
  exception (ex :: <file-system-error>)
    #f
  end
end function file-prop;
