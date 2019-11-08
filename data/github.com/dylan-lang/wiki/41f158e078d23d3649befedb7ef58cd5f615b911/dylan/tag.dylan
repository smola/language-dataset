Module: %wiki

// Pages can have a set of tags associated with them, which are
// arbitrary strings and are stored in the "categories" slot of
// the <entry> superclass of <wiki-page>.  The idea is apparently
// to use them for search filters.  e.g., there is some evidence
// that the News sections of the wiki was going to be implemented
// via <wiki:list-pages tags="news" order-by="published"/>.

define wf/object-test (tag) in wiki end;

define function parse-tags
    (tag-string :: <string>) => (tags :: <sequence>)
  choose(complement(empty?),
         remove-duplicates!(map(strip, split(tag-string, ",")), test: \=));
end;

define function unparse-tags
    (tags :: <sequence>) => (tags :: <string>)
  join(tags, ", ")
end;

define tag show-tag in wiki
    (page :: <wiki-dsp>)
    ()
  if (*tag*)
    output("%s", escape-xml(*tag*));
  end if;
end;

define named-method query-tagged? in wiki
    (page :: <wiki-dsp>)
  get-query-value("tagged");
end;

define body tag list-query-tags in wiki
    (page :: <wiki-dsp>, do-body :: <function>)
    ()
  let tagged = get-query-value("tagged");
  if (instance?(tagged, <string>))
    for (tag in parse-tags(tagged))
      dynamic-bind(*tag* = tag)
        do-body();
      end;
    end for;
  end if;
end;
