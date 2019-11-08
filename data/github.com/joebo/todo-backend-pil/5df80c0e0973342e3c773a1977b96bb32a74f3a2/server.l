(load "@lib/json.l")

(default *App "app.l")
(default *BaseURL "http://csilo.com")

# reloads the app. useful after each request in debug mode to support hot-reloading (no need to restart pil)
(de reload-routes ()
    (if *Dbg (load *App) ) )

(de http (Sock Forked)
   (use (Request HeaderLine Header ContLen Posted HeaderParts)
      (in Sock
         (setq Request (line))
         (if *Dbg (msg (pack Request)))
         (while (setq HeaderLine (line))
            (setq HeaderParts (split HeaderLine " "))
            (setq Header (lowc (pack (car HeaderParts))))
            (if (= Header "content-length:")
               (setq ContLen (format (cdr HeaderParts))) ) )
         (when (or (pre? "POST" Request) (pre? "PATCH" Request))
            (setq Posted (pack (ht:Read ContLen))) ) )
      (reload-routes)
      (route Sock Request Posted)
      (close Sock)
      (if Forked (bye)) ) )

(de server-nofork (P)
   (use (Port Sock)
      (setq *App "app-nopersist.l")
      (setq *BaseURL (pack *BaseURL ":" P))
      (setq Port (port P))
      (loop
         (setq Sock (listen Port))
         (http Sock)
         (close Sock) ) ) )

(de server-fork (P)
   (pool "todo.db")
   (use (Port Sock)
      (setq *App "app-persist.l")
      (setq *BaseURL (pack *BaseURL ":" P))
      (setq Port (port P))
      (loop
         (setq Sock (listen Port))
         (NIL (fork) (close Port))
         (close Sock) )
      (http Sock T) ) )

# (server-fork 8088)
