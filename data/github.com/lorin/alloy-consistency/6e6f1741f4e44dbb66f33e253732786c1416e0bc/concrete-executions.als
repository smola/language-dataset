let id[A] = A<:iden

// events
sig E {
    eo: set E,
    tr: Tr,
    role: R,
    del : lone E,
}

fact NoOrphans {
    all t : Tr | some tr.t
    all r : R | some role.r
    all m : Message | some snd.m
    all o : Op | some op.o
    all s : State | some (pre+post).s
    all v : V | some rval.v
    all p : P | some proc.p
}


// Transition
// We use none to model ⊥

abstract sig Tr {
    op: lone Op,
    rcv: lone Message,
    proc: lone P,
    pre: lone State,
    post: State,
    snd: set Message,
    rval: lone V
}

sig init extends Tr {
    σ': State,
    M : set Message
}

sig call extends Tr {
    o : Op,
    σ: State,
    σ': State,
    M : set Message
}

sig rcv extends Tr {
    m : Message,
    σ: State,
    σ': State,
    M : set Message
}

sig step extends Tr {
    p : P,
    σ: State,
    σ': State,
    M: set Message
}

sig callret extends Tr {
    o : Op,
    σ: State,
    σ': State,
    M : set Message,
    v : V
}

sig rcvret extends Tr {
    m : Message,
    σ: State,
    σ': State,
    M : set Message,
    v : V
}

sig stepret extends Tr {
    p : P,
    σ: State,
    σ': State,
    M: set Message,
    v : V
}


// values
sig V {}

// processes
sig P {}

// operations
sig Op {}

// roles
sig R {}

// messages
sig Message {}

sig State {}


pred isEnumeration[es: E, r:E->E] {
     // enumeration is total order and natural
     // alloy models are finite so we just need to check for total order


     // Total orders are partial orders which are also total
     // partial ordres are irreflexive and transitive


    // irreflexive
    no id[es] & r

    // transitive
    r.r in r

    // total
     es->es in r + ~r + id[es] 
}

fact eoIsEnumeration { 
    isEnumeration[E,eo]
}



// predecessor
fun prd[E': set E, r:E->E, e: E] : lone E {
    let es = r.e | {e' : es | no f: es |  e'->f in r}
}

fun calls[E' : set E] : set E {
    {e: E' | some e.tr.op}
}

fun returns[E' : set E] : set E {
    {e : E' | some e.tr.rval}
}

pred isTrajectory[E' : set E, eo' : E->E, tr': E->Tr] {
    // (t1) eo is an enumeration of E.
    isEnumeration[E', eo']

    // (t2) tr : E → Transitions specifies the transition of each event. 
    // This is our tr relation

    // (t3) The first (and only the first) transition is an initialization
    // transition, and the pre-state of each transition matches the
    // post-state of the previous transition:
    // ∀e ∈ E : 􏰁 pre(e) = ⊥ = pred(E,eo,e)
    // ∨ pre(e) = post(pred(E, eo, e)) 􏰂
    all e : E' | no (e.tr'.pre + prd[E',eo,e])  or (e.tr'.pre = prd[E',eo',e].tr.post)

    // (t4) A call transition may not follow another call transition unless there is a return transition in between them:
    // ∀c1, c2 ∈ calls(E) : c1 <eo c2 ⇒
    // ∃r ∈ returns(E) : c1 ≤eo r <eo c2
    all disj c1,c2 : calls[E'] | c1->c2 in eo' => some r : returns[E'] | {c1->r in eo'  r->c2 in eo'}
}

pred isWellFormedTrajectory[E': set E, eo':E->E, tr': E->Tr] {
    // p86
    // Definition 7.4 (Well-formed Trajectories). A trajectory (E,eo,tr) is well-formed if each event is preceded by no more returns than calls:
    // ∀e∈E:􏰈􏰈{r∈returns(E)|r≤eo e}􏰈􏰈≤􏰈􏰈{c∈calls(E)|c≤eo e}􏰈􏰈
    all e : E' | #{r : returns[E'] | r->e in eo'} =< #{c : calls[E'] | c->e in eo'}
}

// 7.3 (p87)
// (c4) The events for each role are a trajectory
fact eventsForEachRoleAreWellFormedTrajectories { 
    // ∀r ∈ R : G|E(r),eo,tr ∈ T
    all r : R | let E'=role.r
              | let eo'=E'<:eo:>E'
              | let tr'=E'<:tr {
        isTrajectory[E', eo', tr']

// 7.3.2 p89
// Definition 7.8 (Well-formed Executions).
// A concrete execution G = (E, eo, tr, role, del) is well-formed if
// all its trajectories traj(G, r) are well-formed.
// We define Ewellformed to be the set of well-formed executions.
        isWellFormedTrajectory[E', eo', tr']
    }
}


// 7.3 (p87)
// (c5) del is a binary, injective relation that satisfies
//
//         del     eo
// ∀s,r∈E: s−→r ⇒ s−→r ∧ rcv(r)∈snd(s)
fact delConstraints {
    // injective
    no disj e1,e2 : E | e1.del=e2.del

    //         del     eo
    // ∀s,r∈E: s−→r ⇒ s−→r ∧ rcv(r)∈snd(s)
    all s,r : E | s->r in del => {
        some tr.rcv[r] // some message is received
        s->r in eo
        tr.rcv[r] in tr.snd[s]
    }
}

run {some E}
