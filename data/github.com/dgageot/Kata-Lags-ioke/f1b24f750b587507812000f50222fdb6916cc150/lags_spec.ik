use("ispec")

Demande = Origin mimic do(
	initialize = method(debut, fin, prix, @debut = debut. @fin = fin. @prix = prix)
)

chiffre_affaire = method(demandes,
	if (demandes empty?, 0, 
		let (
			first, demandes first,
			rest, demandes rest,
			[first prix + chiffre_affaire(rest filter(debut >= first fin)), chiffre_affaire(rest)] max
		)
	)
)

;p = method(+rest,
;	rest length println)

p = dmacro([+>rest] rest length println)

ca = dmacro(
	[] 0,
	[>first] first prix,
	[>first, +>rest]
	p(*rest)
	[ca(first) + chiffre_affaire(rest filter(debut >= first fin)), chiffre_affaire(rest)] max
)

describe("avionneur",
	it("chiffre_affaire_pour_un_carnet_de_commande",
		ca(Demande mimic(0,5,10), Demande mimic(3,7,5), Demande mimic(5,9,7), Demande mimic(6,9,8)) should == 18
	)
  
	it("chiffre_affaire_sans_demande_equal_zero",
		ca() should == 0
	)
  
	it("chiffre_affaire_une_demande_equale_prix_demande",
		ca(Demande mimic(0,1,10)) should == 10
	)

	it("chiffre_affaire_deux_demandes_compatibles_equal_sommes_des_prix",
		ca(Demande mimic(0,1,10), Demande mimic(1,2,5)) should == 15
	)
  
	it("chiffre_affaire_deux_demandes_incompatibles_equal_prix_max",
		ca(Demande mimic(0,2,10), Demande mimic(1,2,5)) should == 10
	)
)