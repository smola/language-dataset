; Textile for Arc, version 0.1
; by David Kendal
; still todo as of this version: links, images, lists, span attributes, footnotes
;   also: internal preflight routine to normalise line endings, strip BOM, etc.
; known bugs: subscript not working due to use of tilde sign in function name

(load "lib/re.arc")

(= txt-block-names* (list "h[1-6]" "bq" "fn[0-9]+" "p" "bc" "pre")
   txt-block-re*    (string "(" (joinstr txt-block-names* "|") ")")
   
   txt-blocktag-re* (string "^" txt-block-re* "(.*?)" #\\ ".(" #\\ ".?)" #\\ "s+")
   
   txt-attr-lang*   (string #\\ "[([a-zA-Z-]{1,5})" #\\ "]")
   txt-attr-class*  (string #\\ "((.+?)" #\\ ")")
   txt-attr-style*  (string #\\ "{(.+?)" #\\ "}")
   
   txt-std-spans*   (obj "**" "b" "__" "i"
                         "*" "strong" "_" "em"
                         "-" "del" "+" "ins" "??" "cite"
                         "^" "sup"  "~" "sub"
                         "%" "span")
   
   txt-shelf*       (table)
   txt-link-shelf*  (table)
   txt-footnotes*   (table))

(def textile (text)
  (let text (txt-preflight text)
    (trim (txt-unshelve:txt-links:txt-spans:txt-block text) 'both)))

(def txt-preflight (text)
  (trim
    (re-replace "\n{3,}" ; two blank lines or more
      (re-replace "(?m:^[\t ]+$)" ; lines with only whitespace
        (re-replace "\r\n?" ; standardise Unix line-ends
          (multisubst '(("\xEF\xBB\xBF" "") ("\xFF\xFE" "") ("\xFE\xFF" "")) text) ; byte order marks
        "\n")
      "")
    "\n\n")
  'both [is _ #\newline]))

(def txt-block (text)
  (with (ext? nil tag "p" xattrs "" content "" ntext "")
    (each block (str-split text "\n\n")
      (= ntext (string ntext "\n\n" 
        (with (bname (cadr (re-match-pat (string "^" txt-block-re*) block)) retcon "")
          (if bname ; if there's a tag name...
            (let (_ _ txtattrs txtext?) (re-match-pat txt-blocktag-re* block)
              (when ext? ; when the previous tag is an extended block
                (= retcon (string (txt-do-tag content tag (txt-pa xattrs)) "\n\n")
                   ext?   nil tag "p" content "" xattrs "" tag))
              
              (with (txtcontent (re-replace txt-blocktag-re* block "") battr (txt-pa txtattrs))
                (if (not (empty txtext?))
                  (do (= ext? t  tag bname   xattrs txtattrs    content txtcontent) nil)
                  
                  (string retcon (if
                    (re-match-pat "^h([1-6])$" bname)
                    (txt-h txtcontent bname battr)
                    
                    (re-match-pat "^fn([0-9]+)$" bname)
                    (txt-fn txtcontent bname battr)
                    
                    (txt-do-tag txtcontent bname battr))))))
            
            ext? ; else, if there's no tag name and we're in an extended block
            (do (= content (string content "\n\n" block)) nil)
            
            (re-grep "^\\[(\\S+)\\](.+)$" (tokens block #\newline))
            (each line (tokens block #\newline)
              (let (_ name url) (re-match-pat "^\\[(\\S+)\\](.+)$" line)
                (= txt-link-shelf*.name url)))
            
            (txt-p block "p" "") ; otherwise, do an ordinary paragraph tag
          ))))) ntext))

(def txt-pa (attr) ; "Parse Attributes"
  (with (class nil id "" lang "" style nil)
    (while (re-match (string "^(?:" txt-attr-lang* "|" txt-attr-class* "|" txt-attr-style* ")") attr)
      (if
        (re-match-pat (string "^" txt-attr-lang*) attr)
        (= lang (txt-lang attr)
           attr (re-replace (string "^" txt-attr-lang*) attr ""))
          
        (re-match-pat (string "^" txt-attr-class*) attr)
        (= class (cons (txt-class attr) class)
           attr (re-replace (string "^" txt-attr-class*) attr ""))
        
        (re-match-pat (string "^" txt-attr-style*) attr)
        (= style (cons (txt-style attr) style)
           attr (re-replace (string "^" txt-attr-style*) attr ""))))
    
  ;  (= class
  ;    (flat (map [tokens _] class)))
    
    ; assemble html attributes
    (string
      (when (isnt "" lang) (string " lang=\"" lang "\""))
      (when class (string " class=\"" (joinstr class " ") "\""))
      (when style (string " style=\"" (joinstr style ";") "\"")))))
(def txt-strip-attrs (text)
  (let are (string "^(?:" txt-attr-lang* "|" txt-attr-class* "|" txt-attr-style* ")")
    (while (re-match are text)
      (= text (re-replace are text ""))) text))

(def txt-simple-block (text)
  (string "<p>" (joinstr (str-split text "\n\n") "</p>\n\n<p>") "</p>"))

(def txt-spans (text)
  (= text (re-replace (string "(?<=\\W|^)@(\\S.*?\\S?)@(?=\\W|$)") text
    (fn (txt content)
      (txt-shelve (string "<code" (txt-pa content) ">" (txt-strip-attrs content) "</code>")))))
  (each (span tag) txt-std-spans*
    (zap txt-span text span span tag)) ; thanks to rocketnia for suggesting this way of doing it.
  text)

(def txt-span (text st et tag) ; st = start textile; et = end textile
  (re-replace (string "(?<=\\W|^)" (txt-re-quote st) "(\\S.*?\\S?)" (txt-re-quote et) "(?=\\W|$)") text
    (fn (txt content)
      (string "<" tag (txt-pa content) ">" (txt-strip-attrs content) "</" tag ">"))))

(def txt-links (text)
  (re-replace "(?<=^|\\W)\"(\\S.*?\\S?)\":([^\\s<>]+)(?=[\\s<>]|$)" text
    (fn (_ title href)
      (with (attr (txt-pa title) cont (txt-strip-attrs title)) 
        (if (txt-link-shelf* href)
          (string "<a href=\"" (txt-link-shelf* href) "\"" attr ">" cont "</a>")
          (string "<a href=\"" href "\"" attr ">" cont "</a>"))))))

(def txt-re-quote (text)
  (re-replace "(\\.|\\\\|\\+|\\*|\\?|\\[|\\]|\\$|\\(|\\)|\\{|\\}|\\=|\\!|\\<|\\>|\\||\\:|-)" text (string #\\ #\\ #\\ 1)))
(def txt-html-chars (text)
  (multisubst '(("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;")) text)) ; "&amp; must go before all others

(def txt-lang (attr)
  (cadr (re-match-pat (string "^" txt-attr-lang*)  attr)))
(def txt-class (attr)
  (cadr (re-match-pat (string "^" txt-attr-class*) attr)))
(def txt-style (attr)
  (cadr (re-match-pat (string "^" txt-attr-style*) attr)))

(def txt-do-tag (text bname battr)
  (eval `(,(sym (string "txt-" bname)) ,text ,bname ,battr)))

(def txt-h (text bname battr)
  (string "<" bname battr ">" text "</" bname ">"))
(def txt-p (text bname battr)
  (string "<p" battr ">" text "</p>"))
(def txt-bq (text bname battr)
  (string "<blockquote" battr ">" (txt-simple-block text) "</blockquote>"))
(def txt-bc (text bname battr)
  (txt-shelve (string "<pre" battr "><code" battr ">" (txt-html-chars text) "</code></pre>")))
(def txt-pre (text bname battr)
  (txt-shelve (string "<pre" battr ">" (txt-html-chars text) "</pre>")))

(def txt-shelve (text)
  (let id (string (uniq))
    (= txt-shelf*.id text)
    (string "<textile:shelf " id " />")))
(def txt-unshelve (text)
  (re-replace "<textile:shelf (.+?) />" text
    (fn (tag id)
      (txt-shelf* id))))

(def str-split (blob delim) ; hacky
  (tokens
    (subst #\u0001 delim blob) #\u0001))

(def not (what) (if what nil t))

(def re-grep (pat texts)
  (keep [re-match-pat pat _] texts))
