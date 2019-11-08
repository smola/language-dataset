(defcc <element>
  <stag> <content> <etag> := (@s <stag> <content> <etag>);)

(compile <element> (explode "<bad>content<bad>"))

(defcc <emptyelemtag>
  <startchar> <name> <whitespace> <attributes> <endchar> := (@s <startchar> <name> <attributes> <endchar>);)

(defcc <whitespace>
  " " := " "; )

(compile <emptyelemtag> (explode "<name attr='value'atttoo='value2'>"))
(compile <attributes> (explode "name='attr'name="))

(defcc <attributes>
  <attribute> <attributes> := (@s <attribute> <attributes>);
  <e> := ""; )

(defcc <attribute>
  <name> <eq> <attvalue> := (@s <name> <eq> <attvalue>);)

(defcc <attvalue>
  <quote> <string-chars> <quote> := (@s <quote> <string-chars> <quote>);)

(defcc <quote>
  "'" := "'";)

(defcc <eq>
  "=" := "=";)
(defcc <content>
  <string-chars> := <string-chars>;)

(defcc <name>
  <string-chars> := <string-chars>;)

(defcc <stag>
  <startchar> <string-chars> <endchar> := (@s  <startchar> <string-chars> <endchar>);) \\ i'm dumb

(defcc <etag>
  <startchar> <string-chars> <endchar> := (@s  <startchar> <string-chars> <endchar>);)

(defcc <string-char>
  Char := Char where (not (element? Char ["c#34;" "\" "<" ">" "=" " " "'"]));)

(defcc <string-chars>
  <string-char> <string-chars>              := (@s <string-char> <string-chars>);
  <e>                                       := "";)

(compile <content> (explode "string"))

(defcc <startchar>
  "<" := "<";)

(defcc <endchar>
  ">" := ">";)

