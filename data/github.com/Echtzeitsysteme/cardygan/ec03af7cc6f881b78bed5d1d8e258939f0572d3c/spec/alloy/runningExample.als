open util/integer

/**
	Meta-model
*/

one sig FM {
	root: one Feature,
	group: set Feature,
	require: ConstraintNode -> ConstraintNode,
	exclude: ConstraintNode -> ConstraintNode,
	config: set Instance
}
abstract sig Feature{
	groupCardinality: set Intervall,
	groupInstanceCardinality: set Intervall,
	cardinality: some Intervall,
	parent: lone Feature,
	instances: set Instance
}
sig Instance {
	feature: one Feature,
	instanceParent: lone Instance
}
sig Intervall {
	lowerBound: Int,
	upperBound: Int + KleeneStar
}
abstract sig ConstraintNode {
	feature: one Feature,
	cardinality: some Intervall
}
one sig KleeneStar {}

pred IsRoot (f: Feature) {
	f = FM.root
}

pred IsPossibleCardinality (f: Feature, d: Int) {
	one i: Intervall | i in f.cardinality && d >= i.lowerBound && (d <= i.upperBound || i.upperBound = KleeneStar)
}

pred IsPossibleGroupCardinality (f: Feature, d: Int) {
	one i: Intervall | i in f.groupCardinality && d >= i.lowerBound && (d <= i.upperBound || i.upperBound = KleeneStar)
}

pred IsPossibleGroupInstanceCardinality (f: Feature, d: Int) {
	one i: Intervall | i in f.groupInstanceCardinality && d >= i.lowerBound && (d <= i.upperBound || i.upperBound = KleeneStar)
}

pred IsInsideConstraintIntervall (c: ConstraintNode, d: Int) {
	one i: Intervall | i in c.cardinality && d >= i.lowerBound && (d <= i.upperBound || i.upperBound = KleeneStar)
}
//Intervall constraints

fact intervallPositive {
	all i: Intervall | i.lowerBound >= 0 && (i.upperBound >= i.lowerBound || i.upperBound = KleeneStar)
}

//wenn zwei Intervalle die gleichen Grenzen haben, ist es das gleiche Intervall
//fact noEqualIntervall {
//	all i, j: Intervall | i.lowerBound = j.lowerBound && i.upperBound = j.upperBound implies i=j
//}

//kein intervall lose im FM
fact intervallIsAttribute {
	all i: Intervall | (i in Feature.cardinality || i in Feature.groupCardinality || i in Feature.groupInstanceCardinality || i in ConstraintNode.cardinality)
}


//keine sich überschneidenden oder direkt angrenzenden Intervalle
fact noClustering {
	all i, j: Intervall | no f: Feature | i != j && i in f.cardinality && j in f.cardinality && OverlappingIntervall[i, j]
	all i, j: Intervall | no f: Feature | i != j && i in f.groupCardinality && j in f.groupCardinality && OverlappingIntervall[i, j]
	all i, j: Intervall | no f: Feature | i != j && i in f.groupInstanceCardinality && j in f.groupInstanceCardinality && OverlappingIntervall[i, j]
	all i, j: Intervall | no c: ConstraintNode | i != j && i in c.cardinality && j in c.cardinality && OverlappingIntervall[i, j]
}

pred OverlappingIntervall(i: Intervall, j: Intervall) {
	(i.lowerBound <= j.lowerBound && (i.upperBound >= j.lowerBound || i.upperBound = KleeneStar)) || i.lowerBound = j.upperBound.plus[1]
}

//FM constaints

fact rootTree {
	all f: Feature | FM.root in f.*parent	//genau 1 root!!
	all f: FM.root | no f.parent
	all f: Feature - FM.root | one f.parent
	no f: Feature | f in f.^parent	//zyklenfrei
}

fact configuration {
	all i: Instance | i in FM.config
}

//root muss im FM aktiv sein
fact root{
	all f: FM.root | IsPossibleCardinality[f, #(f.instances)]
	all f: Feature | f in FM.group || f in FM.root
}

fact inGroupOrSingleton {
	all f: Feature | some (f.~parent & FM.group) implies #(f.~parent & FM.group) = #f.~parent
}

fact onlyGroupNodeCanHaveGroupCardinalities {
	all f: Feature | all i: Intervall | #(f.~parent & FM.group) = 0 implies !(i in f.groupCardinality || i in f.groupInstanceCardinality)
}

fact childParentConstraint{
	all i: Instance | not i in FM.root.instances implies #i.instanceParent = 1 else #i.instanceParent = 0
}

fact constraintNodeOnlyInConstraintEdge {
	all c1: ConstraintNode | some c2: ConstraintNode | c1 in FM.require[c2] || c2 in FM.require[c1] || c1 in FM.exclude[c2] || c2 in FM.exclude[c1]
}
// in Gruppe sind mindestens/maximal so viele Features aktiv, wie Gruppenkardinalität spezifiziert
fact groupCardinalityConstraint {
	all i: Instance | some (i.feature.~parent & FM.group) implies IsPossibleGroupCardinality[i.feature, #(i.~instanceParent.feature & FM.group)]
}

// in Gruppe sind mindestens/maximal so viele Instances aktiv, wie Gruppeninstanzkardinalität spezifiziert
fact groupInstanceCardinalityConstraint {
	all i: Instance | some (i.feature.~parent & FM.group) implies IsPossibleGroupInstanceCardinality[i.feature, #(i.~instanceParent & FM.group.instances)]
}

fact instanceParent{
	all i: Instance | not IsRoot[i.feature] implies #(i.instanceParent & i.feature.parent.instances) = 1
	all i: Instance | all f: Feature | f in i.feature.~parent implies IsPossibleCardinality[f, #(i.~instanceParent & f.instances)] || #(i.~instanceParent & f.instances) = 0
}

fact instanceFeatureMapping {
	all i: Instance, f: Feature | f in i.feature implies i in f.instances else not i in f.instances
}
// Require edges
fact requireConstraint {
	all f: Feature | all c: ConstraintNode | f in c.feature implies !(f in FM.require[c].feature)
	all c1, c2: ConstraintNode | IsInsideConstraintIntervall[c1, #(c1.feature.instances)] && c2 in FM.require[c1] implies IsInsideConstraintIntervall[c2, #(c2.feature.instances)] 
}
// Exclude edges
fact excludeConstraint {
	all f: Feature | all c: ConstraintNode | f in c.feature implies !(f in FM.exclude[c].feature)
	all c1, c2: ConstraintNode | IsInsideConstraintIntervall[c1, #(c1.feature.instances)] && (c2 in FM.exclude[c1] || c1 in FM.exclude[c2]) implies !(IsInsideConstraintIntervall[c2, #(c2.feature.instances)]) 
}
one sig Platoon, AddressingScheme, Channels, Movement, Node, Regular, Compact, HighThroughput, Reliable, SlowMoving, FastMoving, Follower, Leader extends Feature{}
one sig i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33 extends Intervall{}
one sig c0, c1, c2, c3, c4, c5, c6, c7, c8, c9 extends ConstraintNode{}
fact {
i1.lowerBound = 1
i1.upperBound = 1
Platoon.cardinality = i1
i2.lowerBound = 4
i2.upperBound = KleeneStar
Platoon.groupInstanceCardinality = i2
i3.lowerBound = 4
i3.upperBound = KleeneStar
Platoon.groupCardinality = i3
AddressingScheme.parent = Platoon
i4.lowerBound = 1
i4.upperBound = 1
AddressingScheme.cardinality = i4
i5.lowerBound = 1
i5.upperBound = 1
AddressingScheme.groupInstanceCardinality = i5
i6.lowerBound = 1
i6.upperBound = 1
AddressingScheme.groupCardinality = i6
Channels.parent = Platoon
i7.lowerBound = 1
i7.upperBound = KleeneStar
Channels.cardinality = i7
i8.lowerBound = 1
i8.upperBound = KleeneStar
Channels.groupInstanceCardinality = i8
i9.lowerBound = 1
i9.upperBound = KleeneStar
Channels.groupCardinality = i9
Movement.parent = Platoon
i10.lowerBound = 1
i10.upperBound = 1
Movement.cardinality = i10
i11.lowerBound = 1
i11.upperBound = 1
Movement.groupInstanceCardinality = i11
i12.lowerBound = 1
i12.upperBound = 1
Movement.groupCardinality = i12
Node.parent = Platoon
i13.lowerBound = 1
i13.upperBound = KleeneStar
Node.cardinality = i13
i14.lowerBound = 1
i14.upperBound = 1
Node.groupInstanceCardinality = i14
i15.lowerBound = 1
i15.upperBound = 2
Node.groupCardinality = i15
Regular.parent = AddressingScheme
i16.lowerBound = 0
i16.upperBound = 1
Regular.cardinality = i16
Compact.parent = AddressingScheme
i17.lowerBound = 0
i17.upperBound = 1
Compact.cardinality = i17
HighThroughput.parent = Channels
i18.lowerBound = 1
i18.upperBound = 8
HighThroughput.cardinality = i18
Reliable.parent = Channels
i19.lowerBound = 1
i19.upperBound = 32
Reliable.cardinality = i19
SlowMoving.parent = Movement
i20.lowerBound = 0
i20.upperBound = 1
SlowMoving.cardinality = i20
FastMoving.parent = Movement
i21.lowerBound = 0
i21.upperBound = 1
FastMoving.cardinality = i21
Follower.parent = Node
i22.lowerBound = 0
i22.upperBound = 1
Follower.cardinality = i22
Leader.parent = Node
i23.lowerBound = 0
i23.upperBound = 1
Leader.cardinality = i23
c0.feature = Compact
i24.lowerBound = 1
i24.upperBound = KleeneStar
c0.cardinality = i24
c5.feature = HighThroughput
i25.lowerBound = 10
i25.upperBound = 20
c5.cardinality = i25
c1.feature = HighThroughput
i26.lowerBound = 1
i26.upperBound = KleeneStar
c1.cardinality = i26
c6.feature = Reliable
i27.lowerBound = 3
i27.upperBound = KleeneStar
c6.cardinality = i27
c2.feature = FastMoving
i28.lowerBound = 1
i28.upperBound = 1
c2.cardinality = i28
c7.feature = Reliable
i29.lowerBound = 2
i29.upperBound = KleeneStar
c7.cardinality = i29
c3.feature = Follower
i30.lowerBound = 3
i30.upperBound = KleeneStar
c3.cardinality = i30
c8.feature = FastMoving
i31.lowerBound = 1
i31.upperBound = KleeneStar
c8.cardinality = i31
c4.feature = Platoon
i32.lowerBound = 1
i32.upperBound = 1
c4.cardinality = i32
c9.feature = Leader
i33.lowerBound = 1
i33.upperBound = 1
c9.cardinality = i33
FM.require = c2 -> c7 + c4 -> c9 
FM.exclude = c0 -> c5 + c1 -> c6 + c3 -> c8 
}
pred show{}
run show for 10 but 7 int, 0 Feature, 0 ConstraintNode
