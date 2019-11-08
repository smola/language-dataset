package sync

type joiner struct {
	lock Lock
	ret  int

	condJoined Cond
	condExited Cond
	joinable   bool
	joined     bool
	exited     bool
	parent     *Thread
}

func (j *joiner) join(t *Thread) int {
	cur := CurThread()
	assert(j.joinable)
	assert(cur != t)
	assert(t.alive)
	assert(cur == j.parent)

	// cur will be waiting on t to finish
	// so to handle priority inversion
	// we need to adjust the priority if t
	// is a lower priority thread
	hold := intr.Disable()
	phold := t.priority
	if cur.priority < phold {
		t.SetPriority(cur.priority)
	}
	intr.Restore(hold)

	j.lock.Lock()
	assert(!j.joined)
	for !j.exited {
		j.condExited.Wait(&j.lock)
	}

	hold = intr.Disable()
	t.SetPriority(phold) // joined, we can restore the priority now
	intr.Restore(hold)

	// Need to copy this out to the stack
	// After the Unlock(), the thread will be
	// put back to pool and might reborn as a new
	// thread, and the this.ret could be anything.
	save := j.ret
	j.joined = true
	j.condJoined.Notify(&j.lock)

	j.lock.Unlock()

	return save
}

func (j *joiner) exit(v int) {
	if !j.joinable {
		return
	}

	j.lock.Lock()
	j.ret = v
	j.exited = true
	j.condExited.NotifyAll(&j.lock)

	for !j.joined {
		j.condJoined.Wait(&j.lock)
	}
	j.lock.Unlock()
}

func (j *joiner) init(joinable bool) {
	j.joinable = joinable
	j.joined = false
	j.exited = false
	j.ret = 0
	j.parent = CurThread()
}

var joiners [Nthread]joiner

// Join blocks until the thread exits.
// It panics when t is not joinable, is CurThread(),
// or is not even scheduled with Ready().
func Join(t *Thread) int { return joiners[t.id].join(t) }

func joinInit(t *Thread, joinable bool) {
	joiners[t.id].init(joinable)
}

func joinExit(v int) {
	joiners[CurThread().id].exit(v)
}
