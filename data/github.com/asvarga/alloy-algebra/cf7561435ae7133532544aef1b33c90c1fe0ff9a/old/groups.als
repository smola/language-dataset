
open util/ternary

--------

sig Elem {
	rel: Elem -> one Elem
}{
	-- total
	rel.Elem = Elem
}
one sig Id extends Elem {}

fact {
	-- identity
	//all x: Elem | rel[Id][x] = x and rel[x][Id] = x
	Id.rel in iden and Id.(flip12[rel]) in iden
	-- associativity
	all x,y,z: Elem | rel[rel[x][y]][z] = rel[x][rel[y][z]]
	-- inverse
	//all x: Elem | some inv: Elem | rel[x][inv] = Id and rel[inv][x] = Id
	(rel.Id).Elem = Elem and Elem.(rel.Id) = Elem
}

--------

fun closure(gen: set Elem): set Elem {
	Id.*(gen.rel)
}
pred generator(gen: set Elem) {
	closure[gen] = Elem
}
pred gen1 {		-- aka cyclic
	some c: Elem | generator[c]
}
pred gen2 {
	some disj c1,c2: Elem | generator[c1+c2]
}
pred gen3 {
	some disj c1,c2,c3: Elem | generator[c1+c2+c3]
}

--------

run Some { some Elem } for exactly 6 Elem
run Gen1 { gen1 } for exactly 6 Elem
run Gen2 { gen2 and not gen1 } for exactly 6 Elem
run Gen3 { gen3 and not gen2 } for exactly 8 Elem
run D6 {
	some disj s,r: Elem-Id {
		rel[s][s] = Id
		rel[r][rel[r][r]] = Id
	}
} for exactly 6 Elem











