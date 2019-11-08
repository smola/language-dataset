(class +Board +Entity)
(rel boardid (+Key +Number))
(rel title (+String))
(rel abrev (+Idx +String))
(rel threads (+List +Joint) board (+Thread))
(dm url> () (pack "/public/board.l?" (: abrev)))

(class +Thread +Entity)
(rel threadid (+Key +Number))
(rel sticky (+Bool))
(rel locked (+Bool))
(rel title (+String))
(rel op (+Joint) isop (+Post))
(rel replies (+List +Joint) thread (+Post))
(rel board (+Joint) threads (+Board))

(dm url> () (pack "/public/thread.l?" "+" (: threadid)))

(class +Post +Entity)
(rel postid (+Key +Number))
(rel content (+String))
(rel isop (+Joint) op (+Thread))
(rel thread (+Joint) replies (+Thread))
(rel image (+Joint) post (+Image))
(rel user (+String))
(rel timestamp (+String))
(rel email (+String))

(dm url> () (pack  (send 'url> (or (: thread) (: isop))) "#p" (: postid)))

(class +Image +Entity)
(rel imgid (+Key +Number))
(rel post (+Joint) image (+Post))
(rel filename (+String))
(rel filesize (+Number))
(rel filetype (+String))

(dm url> ("pack?" "full?")
   (pack 
      (when "pack?" 
         (if "full?" "/img/" "/thumb/")) 
      (: imgid) "." (: filetype)))
