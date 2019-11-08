(require acid.language)


(defmacro/g! rule [host &rest my-checks]
  `((fn []

    (.update
      (get db "sites")
      {"_id" ~host}
      {"_id" ~host
       "set" daemon-set-id
       "checks" (list-comp
                  (. x --name--)
                  [x [~@my-checks]])}
      true
      true)

    (for [~g!check [~@my-checks]]
      (emit :start-checking {:site ~host :check ~g!check})))))


(defmacro rules [set-id &rest my-rules]
  `((fn []
    (import [snitch.informants [pingable httpable]]
            [snitch.core [db]] random datetime syslog)

    (trip
      (let [[*min-ping-length* (acid-time 1 minute)]
            [*max-ping-length* (acid-time 10 minutes)]
            [daemon-set-id ~set-id]]

        (on :startup (syslog.syslog syslog.LOG_INFO
          (.join " " ["snitch daemon for" ~set-id "online"])))

        (on :update  ;; This is debug information
          (syslog.syslog
            syslog.LOG_DEBUG (.join " "
                  [(:site event) (str (:failed event)) (:info event) "(done in"
                   (str (:runtime event)) "seconds)" "next check is in"
                   (str (:retry-delay event)) "seconds"])))

        (on :update  ;; store the event in memory
          (let [[oid (.insert (get db (% "snitch.%s" ~set-id))
                  {"checked_at" (:checked-at event)
                   "set"        ~set-id
                   "failed"     (:failed event)
                   "site"       (:site event)
                   "check"      (. (:check event) --name--)
                   "info"       (:info event)
                   "value"      (.total-seconds (:runtime event))})]]
            (syslog.syslog syslog.LOG_DEBUG (+ "stored report as" (str oid)))))

        (on :start-checking
          (schedule-in-seconds (.randint random 0 60)
            (defns [wait]
              (let [[start (.utcnow datetime.datetime)]
                    [(, is-up info) ((:check event) (:site event))]
                    [end (.utcnow datetime.datetime)]
                    [response-time (- end start)]
                    [time (if is-up (* wait 2) (/ wait 2))]
                    [retry-time (cond [(< time *min-ping-length*) *min-ping-length*]
                                      [(> time *max-ping-length*) *max-ping-length*]
                                      [true time])]]
                  (emit :update {:site (:site event)
                                 :checked-at start
                                 :check (:check event)
                                 :runtime (- end start)
                                 :failed (not is-up)
                                 :retry-delay retry-time
                                 :info info})
                    (reschedule-in-seconds retry-time retry-time))) 0))
            ~@my-rules)))))
