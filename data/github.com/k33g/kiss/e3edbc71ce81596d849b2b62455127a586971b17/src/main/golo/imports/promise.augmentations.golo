module kiss.promise.augmentations

import gololang.Async
import gololang.concurrent.workers.WorkerEnvironment

----
  Promise helper: it's easier to make asynchronous work

  Augmentation of gololang.concurrent.async.Promise
----
augment gololang.concurrent.async.Promise {
----
 `env` is a worker environment
----
  function initializeWithWorker = |this, env, closure| {
    env: spawn(|message| {
      this: initialize(closure)
    }): send("")
    return this: future()
  }
----
 ...
----
  function initializeWithThread = |this, closure| {
    Thread({
      this: initialize(closure)
    }): start()
    return this: future()
  }
----
 ...
----
  function initializeWithJoinedThread = |this, closure| {
    let t = Thread({
      this: initialize(closure)
    })
    t: start()
    t: join()
    return this: future()
  }
}

