

(defmacro/g! trip [&rest body]
  "Root node. Sets up the async world."
  `(do (import collections asyncio)
       (let [[loop (.get-event-loop asyncio)]]
         (setattr loop "handlers" (.defaultdict collections list))
         ~@body
         (run (emit :startup nil))
         (.run-forever loop))))

(defmacro defns [sig &rest body]
  "Define a function with a `self' pointer pointing at itself"
  (with-gensyms [fnn]
    `(defn ~fnn ~sig
      (let [[self ~fnn]] ~@body))))

(defmacro fn/coroutine [sig &rest body]
  `(with-decorator asyncio.coroutine (fn ~sig ~@body)))

(defmacro defn/coroutine [fnn sig &rest body]
  `(setv ~fnn (fn/coroutine ~sig ~@body)))

(defmacro go [&rest body]
  `(yield-from ~@body))

(defmacro go-setv [name &rest body]
  `(setv ~name (yield-from ~@body)))

(defmacro spawn [&rest body]
  `(.async asyncio ~@body))

(defmacro acid-time [time order]
  "compute the time defined by the time/order"

  (cond [(= order 'miliseconds) (* time 0.001)]
        [(= order 'miliseconds) (* time 0.001)]
        [(= order 'seconds)     (* time 1)]
        [(= order 'second)      (* time 1)]
        [(= order 'minutes)     (* time 60)]
        [(= order 'minute)      (* time 60)]
        [(= order 'hours)       (* time 3600)]
        [(= order 'hour)        (* time 3600)]))

(defmacro schedule [func &rest args]
  "Run a function async-like"
  `(.call-soon loop ~func ~@args))

(defmacro schedule-coroutine [func]
  "Run a coroutine"
  `(.run-until-complete loop ~func))

(defmacro schedule-in [time order func &rest args]
  "Run a function in a few time"
  `(schedule-in-seconds (acid-time ~time ~order) ~func ~@args))

(defmacro schedule-in-seconds [time func &rest args]
  `(.call-later loop ~time ~func ~@args))

(defmacro reschedule [&rest args]
  "rerun the current function (requires defns)"
  `(schedule self ~@args))

(defmacro reschedule-in [time order &rest args]
  "rerun the current function (requires defns) in time"
  `(reschedule-in-seconds (acid-time ~time ~order) ~@args))

(defmacro reschedule-in-seconds [time &rest args]
  `(schedule-in-seconds ~time self ~@args))

(defmacro run [&rest body]
  "run some code async'd"
  `(schedule (defns [] ~@body)))

(defmacro run-in [time order &rest body]
  `(run-in-seconds (acid-time time order) ~@body))

(defmacro run-in-seconds [time &rest body]
  `(schedule-in ~time (defns [] ~@body)))

(defmacro run-every [time order &rest body]
  "run some code once in a while"
  `(run ~@body (reschedule-in ~time ~order)))

(defmacro/g! -emit [obj event e]
  "Given the defaultdict of handlers, handle emit"
  `(for [~g!handler (get ~obj ~event)]
     (spawn (~g!handler ~e))))

(defmacro -on [obj event &rest body]
  "Given the defaultdict of handlers, handle register"
  `(.append (get ~obj ~event)
    (fn/coroutine [event] ~@body)))

(defmacro/g! emit [event e]
  `(-emit loop.handlers ~event ~e))

(defmacro on [event &rest body]
  `(-on loop.handlers ~event ~@body))

(defmacro disown [&rest forms]
  `(.async asyncio ((fn/coroutine [] ~@forms))))
