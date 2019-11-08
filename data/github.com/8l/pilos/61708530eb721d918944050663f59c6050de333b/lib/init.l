# 02jul15abu
# (c) Software Lab. Alexander Burger

# Intial files to load
(de *IniFiles
   ("lib.l" . T)
   ("lib/misc.l" . T)
   ("lib/btree.l" . T)
   ("lib/db.l" . T)
   ("lib/pilog.l" . T)
   ("lib/dbg.l" . T)
   ("lib/sq.l")
   ("misc/bench.l")
   ("misc/fibo.l")
   (".pil/history") )

# Init one SSD (block size 1024)
(call 'rm "-f" "ssd@")
(pool "ssd" (4))

# Store list of initial files in boot object
(put *DB T
   (make
      (for X *IniFiles
         (let S (new T 0)
            (commit)
            (lib/pilos:Out S
               (in (pack "init/" (car X))
                  (echo) ) )
            (store NIL (car X) S)
            (and (cdr X) (link S)) ) ) ) )
(commit)

(call 'mv "ssd@" "db.bin")

# vi:et:ts=3:sw=3
