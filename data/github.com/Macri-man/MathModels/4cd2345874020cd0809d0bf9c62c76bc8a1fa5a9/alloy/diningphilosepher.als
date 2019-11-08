open util/ordering[State]

// A philosepher
sig Phil{ 
	//has left  and right fork
	leftfork, rightfork: lone Fork,
	//sits next to left and right philosepher
	philleft,philright: one Phil
 }

// A fork on the table
sig Fork {
	//fork can be owns by the left or right philosepher
	leftphil, rightphil: one Phil
}

fact{
	//set the number of philosephers and forks
	#Phil = 5
	#Fork = 5
	#Phil = #Fork
	//fork and philosophers are set properly
	//all philosphers there leftfork is equal to left philosephers rightfork and 
	//all philosphers there rightfork is equal to right philosephers leftfork
	all p: Phil | p.leftfork = p.philleft.rightfork and p.rightfork = p.philright.leftfork 
	all f: Fork | f = f.leftphil.rightfork and f = f.rightphil.leftfork
	//one table all philosephers are connected and all forks are connected
	all p: Phil | Phil in p.^philright and Phil in p.^philleft
	all f: Fork | Fork in f.^( leftphil.leftfork ) and Fork in f.^( rightphil.rightfork )
}

//state fork is in
sig State {
	//owned when fork is hold by one philosepher
	owned: Fork -> lone Phil
}{
	//each fork is owned by only their neighbors.
	all f:Fork | owned[f] in f.(leftphil+rightphil)
	//makes sure philosephers do not own forks at the same time
	all f:Fork | not owned[f] in f.leftphil and not owned[f] in f.leftphil
}
//free if state owned on fork f is no
pred free (s: State, f: Fork) {
	no s.owned[f]
}

//eating philosophers is eating if state owned is true for philosophers right and left forks
pred eating (s: State, p: Phil) {
	p  = s.owned[p.rightfork] and p =  s.owned[p.leftfork]
}

//thinking philosophers is thinking if state owned is false for philosophers either right and left forks
pred thinking (s: State, p: Phil) {
	p  != s.owned[p.rightfork]  or p !=  s.owned[p.leftfork]
}

//checks to see if pholsepher can take the left fork
pred CanTakeLeft(s: State, p: Phil){
	free[s,p.leftfork]
}

//philosepher takes left fork
pred TakeLeft (s: State, s': State, p: Phil) {
	CanTakeLeft[s,p] and s'.owned[p.leftfork] = p
	and (all f: (Fork - p.leftfork) | s.owned[f] = s'.owned[f])
}

//checks to see if pholsepher can take the right fork
pred CanTakeRight(s:State, p:Phil){
	free[s,p.rightfork]
}

//philosepher takes right fork
pred TakeRight ( s: State, s': State, p: Phil ) {
	CanTakeRight[s,p] and s'.owned[p.rightfork] = p
	and (all f: (Fork - p.rightfork) | s.owned[f] = s'.owned[f])
}

//check to see if philospher will release left fork
pred CanReleaseLeft(s:State, p:Phil){
	p= s.owned[p.leftfork] and not eating[s,p]
}

//releases left fork
pred ReleaseLeft(s:State, s':State, p:Phil){
	CanReleaseLeft[s,p] and free[s',p.leftfork]
	and (all f: (Fork - (p.leftfork)) | s.owned[f] = s'.owned[f])
}

//check to see if philospher will release right fork
pred CanReleaseRight(s:State, p:Phil){
	p= s.owned[p.rightfork] and not eating[s,p]
}

//releases right fork
pred ReleaseRight(s:State, s':State, p:Phil){
	CanReleaseRight[s,p] and free[s',p.rightfork]	
	and (all f: (Fork - (p.rightfork)) | s.owned[f] = s'.owned[f])
}

//checks if philosepher is eating
pred CanRelease(s:State, p:Phil){
	eating[s,p]
}


//done eating
pred Release(s:State, s':State, p:Phil){
	CanRelease[s,p] and (free[s',p.rightfork] and free[s',p.leftfork])	
	and (all f: (Fork - (p.leftfork + p.rightfork)) | s.owned[f] = s'.owned[f])
}

//define initial state all forks are free an on table
pred init ( s: State ) {
	all f: Fork | free[s,f]
}

//determine next state
//for some philospher takeleft or takeright or releasesleft or releasesright or done eating
pred NextState ( s: State, s': State ) {
	some p: Phil | TakeLeft [ s, s', p ] or TakeRight [ s, s', p ] or Release[s,s',p] or ReleaseRight[s,s',p] or ReleaseLeft[s,s',p]
}

//checks to see if philosepher can change state
pred CanMove(s:State){
	some p: Phil | 
		CanTakeLeft[s,p] or CanTakeRight[s,p] or CanRelease[s,p] or CanReleaseLeft[s,p] or CanReleaseRight[s,p]
}
//Make sure the state change the ownership of the forks
fact{
	init [first] 
	all s: State - last |
 		CanMove[s] => NextState[s,next[s]] 
		else s.owned = next[s].owned
}

//for all philosephors and or some state philosphers eat
//and for all state philosphers move
assert NoStarvationAndNoDeadLock{
	all p: Phil | some s:State | eating[s,p] or thinking[s,p] and CanMove[s] 
}

//make sure not philosephers starve
assert NoStarvation{
	all p: Phil | some s:State | eating[s,p] or thinking[s,p]
}

//make sure all philosephers do not have rightforks and all philosepher do not have left forks and can eat think and move
assert NoDeadLock{
	all p: Phil | all f:Fork | some s:State |
		// p = The state philosepher's left fork -> philospher
		// and p is not equal to the state forks left philosepher's right fork -> philospher 
		((p = s.owned[p.leftfork] and  p != s.owned[f.leftphil.rightfork] ) 
		// p = The state philosepher's right fork -> philospher
		// and p is not equal to the state forks right philosepher's left fork -> philospher 
			and (p = s.owned[p.rightfork]  and p != s.owned[f.rightphil.leftfork]))
		and CanMove[s]
}
//Should have no counter example
check NoStarvationAndNoDeadLock for exactly 5 Fork, 5 Phil, 21 State
//Should have no counter example
check NoStarvation for exactly 5 Fork, 5 Phil, 21 State
//Should have no counter example
check NoDeadLock for exactly 5 Fork, 5 Phil, 21 State
