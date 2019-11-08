module projet

open util/ordering[Time] as to
open util/integer

/**-----SIGNATURES-----**/
abstract sig Coordonnees{
	x: Int, y: Int,
}

sig Noeud extends Coordonnees {}	--Coordonnées où il n'y a ni entrepôt ni réceptacle.

one sig Entrepot extends Coordonnees{}

some sig Receptacle extends Coordonnees{
	poidsCourant: Int one -> Time,
	poidsMax: Int		--Poids maximum que le réceptacles peut contenir.
}

some sig Drone{
	coord: Coordonnees one -> Time, 	--Coordonnées à laquelle se trouve un drone, dépend du temps.
	batterie: Int one -> Time,				--Taux de charge de la batterie d'un drone, dépend du temps.
	poidsMax : Int,								--Poids maximum pouvant être transporté par un drone.
	cmd : Commande							--Commande que le drone doit livrer.
}

some sig Commande{
	destination: one Receptacle,
	poids: Int,
	chemin : seq Coordonnees,	--Liste des coordonnées (noeuds ou réceptacles) par lesquelles le drone devra passer pour atteindre la destination de la commande
											--chemin[0] = entrepôt et chemin[end] = commande.destination
	livree : Int one ->Time
}

sig Time{}

/**-----FONCTIONS UTILITAIRES-----**/
fun abs[n: Int] : Int {n<0 => (negate[n]) else (n) }

fun livrerCommande[d : Drone, t, t':Time ] : Int -- fonction déterminant de quelle manière parcourir le chemin de la commande: à l'aller idx++, au retour idx--, à la destination on ne bouge pas
																	{
																		d.cmd.livree.t = 1 && (d.coord.t'.x != d.cmd.destination.x || d.coord.t'.y != d.cmd.destination.y ) implies (1) 
																		else (d.cmd.livree.t = 1 && d.coord.t'.x = d.cmd.destination.x && d.coord.t'.y = d.cmd.destination.y  implies (0) 
																		else (-1))
																	}

fun batterieAEnlever[c1,c2 : Coordonnees] : Int
																	{
																		(c1.x != c2.x || c1.y != c2.y ) implies (-1)
																		else(0)
																	}

fun distanceDeManhattan[n,m: Coordonnees] : Int{
	add[abs[sub[m.x,n.x]], abs[sub[m.y,n.y]]]
}

/**-----PREDICATS-----**/
pred Atteignable[n, m: Coordonnees] {
	lte[distanceDeManhattan[n, m], 3]
}

pred Voisin[n,m: Coordonnees]{
	eq[distanceDeManhattan[n, m], 1]
}

pred Superpose[n1, n2: Coordonnees]{
	n1 != n2 && eq[distanceDeManhattan[n1, n2], 0]
}

pred ObjetSurCoord[o, c: Coordonnees]{
	eq[o.x,c.x] && eq[o.y,c.y]
}

pred DronesSimilaires[d1,d2 : Drone]{
	d1 = d2
}

pred Init [t: Time, d: Drone, e: Entrepot] { 	--On doit faire l'init pour un Time t
	d.coord.t = e && 	d.batterie.t = 3 	 && (all r:Receptacle | r.poidsCourant.t = 0)		--Tous les drones a l'entrepot et chargés
}

pred Deplacer [t, t': Time, d: Drone]{
	let IndActuellesCoord = d.cmd.chemin.idxOf[d.coord.t] |
		d.coord.t =  d.cmd.chemin[IndActuellesCoord] &&
		d.coord.t' = d.cmd.chemin[add[IndActuellesCoord,d.cmd.livree.t]] && --on regarde l'attribut livree pour déterminer si on doit avancer, reculer ou ne pas bouger dans le chemin de la commande
		d.batterie.t' = add[d.batterie.t, batterieAEnlever[d.coord.t,d.coord.t']] &&
		d.cmd.livree.t' = livrerCommande[d,t, t']
}

pred InstancierChemin [s: seq Coordonnees]{
	let n = s.inds |
	all x: Int & n |
	Voisin[s[x], s[add[x,1]]] --entre les éléments (coordonnées) d'un chemin(seq) il y a une distance de manhattan de 1
}

pred Attendre [t, t' :Time, d:Drone]{
	d.coord.t' = d.coord.t &&
	d.batterie.t' = d.batterie.t
}

pred RechargerBatterie [t, t': Time, d: Drone, c: Coordonnees]{ 														--Si le drone est à l'entrepôt ou sur un receptacle et que sa batterie 
	d.coord.t = c and d.batterie.t<3 and d.coord.t' = d.coord.t and d.batterie.t' = add[d.batterie.t,1] 	--n'est pas pleine alors il y reste jusqu'à la charge complète
}

pred ActionDrone [t, t': Time, d: Drone] {
	some r:Receptacle | all e:Entrepot |
	((d.batterie.t < 3 && (d.coord.t in r || d.coord.t in e))
	implies (
		RechargerBatterie[t, t', d, d.coord.t] &&
		d.cmd.livree.t' = d.cmd.livree.t
	)else (
		Deplacer [t, t', d]
	))
}

pred DepotCommande [t, t': Time, d:Drone]{
	((eq[ livrerCommande[d,t, t'] , 0 ] )
	implies (
	d.cmd.destination.poidsCourant.t' = add[d.cmd.destination.poidsCourant.t,d.cmd.poids]
	)else(
	d.cmd.destination.poidsCourant.t' = d.cmd.destination.poidsCourant.t
	))
}

/**-----Invariants-----**/
--Limites coordonnées
fact {all n: Coordonnees| n.x >=0 && n.x <= 7 && n.y >= 0 && n.y <= 7}
--Entrepôt
fact EntrepotNonIsole {all e: Entrepot | some r: Receptacle | Atteignable[e, r]}
fact EntrepotDisjointR{one e: Entrepot | all r: Receptacle | ! eq[distanceDeManhattan[e ,r], 0]}
fact EntrepotDisjointN{one e: Entrepot | all n: Noeud | ! eq[distanceDeManhattan[e ,n], 0]}
--Réceptacles et noeuds
fact EcartReceptacles {all r: Receptacle | some r2: Receptacle | Atteignable[r,r2] &&r!=r2}
fact NoeudsDisjoints{all n1: Coordonnees| no n2: Coordonnees | Superpose[n1, n2]}
fact UnDroneReceptacle {all d1:Drone | all r:Receptacle | all t:Time | no d2 : Drone | d1 != d2 && d1.coord.t = r && d2.coord.t = r }
fact UnDroneNoeud {all d1:Drone | all n:Noeud |all t:Time |no d2 : Drone |d1 != d2 && d1.coord.t = n && d2.coord.t = n }
--Poids
fact PoidSupZero{no c: Commande | c.poids <= 0}
fact PoidCourantSupZero{all t:Time | no r: Receptacle | r.poidsCourant.t < 0}
fact PoidMaxReceptacleSupZero {no r: Receptacle | r.poidsMax <= 0}
fact PoidMaxDroneSupZero {no d: Drone | d.poidsMax <= 0}
fact PoidCommandInfPoidMaxReceptacle {all c: Commande | c.poids <= c.destination.poidsMax}
fact PoidCommandInfPoidMaxDrone {all d: Drone | d.cmd.poids <= d.poidsMax}
--Batteries
fact BatterieMaxMin{all d: Drone | all t:Time | d.batterie.t>=0 && d.batterie.t<=3}
fact BatterieVide{all d: Drone | some r: Receptacle | some e: Entrepot | all t: Time | (d.batterie.t = 0 && (d.coord.t = r || d.coord.t = e)) || (d.batterie.t > 0)} --Si batterie = 0 alors le drone doit être sur "r" ou "e"
--Livraisons
fact LivraisonDerniereCoord {all c: Commande | all e: c.chemin.elems | c.chemin.last = c.destination && c.chemin.idxOf[e] = c.chemin.lastIdxOf[e]}
fact LivraisonPremiereCoord {all c: Commande | all e: Entrepot | c.chemin.first = e}
fact LivraisonNonCompletee {all c:Commande | c.livree.first = 1}
fact LivraisonLivreeOuNon {all c:Commande | all t:Time | (c.livree.t = -1 ||c.livree.t = 1 || c.livree.t = 0)}
fact LivraisonEcartCoord {all c: Commande | InstancierChemin[c.chemin]}
fact DeportCommande {	all d: Drone | all t: Time-last | let t' = t.next | DepotCommande [t, t', d]}

--Start
fact Start{
	all d: Drone | all e: Entrepot | Init [first, d, e] --Init pour le premier time de l'ordering Time
	all t: Time-last | let t' = t.next | --Pour tous les Time t on definit le Time suivant t'
		all d: Drone |
			ActionDrone [t, t', d]
}

/**-----ASSERTIONS-----**/
assert EntrepotSeul{all r: Receptacle | one e: Entrepot | !ObjetSurCoord[r,e]}
--check EntrepotSeul for 5 but 5 Int
assert DistanceManthattanPositive{all c1: Coordonnees | no c2: Coordonnees | distanceDeManhattan[c1, c2]<0}
--check DistanceManthattanPositive for 5 but 5 Int expect 0
assert RNBsupZero {some c: Coordonnees | some r: Receptacle | ObjetSurCoord[r,c]}
--check RNBsupZero for 5 but 5 Int
assert DNBsupZero{all t: Time | some c: Coordonnees | some d: Drone | eq[distanceDeManhattan[d.coord.t , c],0]}
--check DNBsupZero for 5 but 5 Int
assert CoordonneeSansReceptacle {some c: Coordonnees | all r: Receptacle | !ObjetSurCoord[r,c]}
--check CoordonneeSansReceptacle for 5 but 5 Int
assert CoordonneeUniqueReceptacle {no c: Coordonnees | some r1: Receptacle | some r2: Receptacle | ObjetSurCoord [r1,c] && ObjetSurCoord[r2,c] && r1 != r2}
--check CoordonneeUniqueReceptacle for 5 but 5 Int
assert ReceptacleNonEntrepot {all e: Entrepot | no r: Receptacle | eq[distanceDeManhattan[e ,r], 0]}
--check ReceptacleNonEntrepot for 5 but 5 Int
assert BatterieSupZero{all d: Drone | all t:Time | d.batterie.t >= 0}
--check BatterieSupZero for 5 but 5 Int
assert BatterieNonNulle{all d:Drone | some r:Receptacle | some e:Entrepot | all t:Time | 
										(d.batterie.t =0) =>(distanceDeManhattan[d.coord.t, r]=0 
										or distanceDeManhattan[d.coord.t, e]=0)} --Si batterie = 0, drone sur un receptacle ou un entrepot
--check BatterieNonNulle for 5 but 5 Int
assert DroneEntrepotFirst{all d: Drone | all e: Entrepot | d.coord.first = e}
--check DroneEntrepotFirst for 5 but 5 Int
assert Poids{all c:Commande | c.poids > 0}
--check Poids for 5 but 5 Int
assert ReceptaclesAtteignables {all r: Receptacle | some r2: Receptacle | some e: Entrepot | Atteignable[r,r2] || Atteignable[r,e]}
--check ReceptaclesAtteignables for 5 but 5 Int
assert ReceptacleUnDrone{all r:Receptacle | all t:Time|lone d:Drone| d.coord.t = r}
--check ReceptacleUnDrone for 5 but 5 Int
assert NoeudUnDrone{all n:Noeud | all t:Time|lone d:Drone| d.coord.t = n}
--check NoeudUnDrone for 5 but 5 Int
assert accesDestination{all d:Drone | one t: Time | d.coord.t = d.cmd.destination} --Est ce qu'on va bien a la destination ? oui mais seulement si on a assez de temps
--check accesDestination for 5 but 5 Int
assert UniciteEtapeLivraison{all d:Drone | !(d.cmd.chemin.hasDups)}
--check UniciteEtapeLivraison for 5 but 5 Int
assert DeplacementDroneDeUn{all d:Drone | all t:Time-last | let t' = t.next | distanceDeManhattan[d.coord.t, d.coord.t'] <=1}
--check DeplacementDroneDeUn for 5 but 5 Int
assert IndexEntrepotChemin{all d:Drone | all e:Entrepot | d.cmd.chemin.idxOf[e] = 0} --index de l'entrepot dans un chemin est bien 0
--check IndexEntrepotChemin


/**-----RUN-----**/
pred go {}
run go for 8 but exactly 2 Drone, exactly 2 Commande, exactly 8 Time, exactly 2 Receptacle, 5 Int


