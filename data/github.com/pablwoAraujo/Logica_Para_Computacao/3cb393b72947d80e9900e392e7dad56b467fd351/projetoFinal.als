module parabrisa

one sig  Parabrisa{
	paletas: set Paleta
}

sig Paleta{
	bicos : set BicoEjetor,
	velocidade: one Velocidade
}

sig BicoEjetor{
	estado: one Estado
}


abstract sig Estado{
}

sig Ativado extends Estado{
}

sig Desativado extends Estado{
}


abstract sig Velocidade{
}

sig Desligado extends Velocidade{
}

sig BaixaVelocidade extends Velocidade{
}

sig AltaVelocidade extends Velocidade{
}


fun QuantDePaletas[p:Parabrisa]: set Paleta{
p.paletas
}

fun QuantDeParabrisas[p:Paleta]: set Parabrisa{
p.~paletas
}

fun QuantDeBicos[p:Paleta]: set BicoEjetor{
p.bicos
}


pred MesmoEstadoDosBicosEjetores[a:BicoEjetor, b:BicoEjetor] {
a.estado = b.estado
}

pred MesmaVelocidadeDasPaletas[a:Paleta, b:Paleta]{
a.velocidade = b.velocidade
}


assert TodoParabrisaTemDuasPaletas{
all p:Parabrisa | #p.paletas = 2
}
check TodoParabrisaTemDuasPaletas for 5

assert TodaPaletaPertenceSomenteUmParabrisa{
all p:Paleta | one p.~paletas
}
check TodaPaletaPertenceSomenteUmParabrisa for 5

assert TodaPaletaTemVelocidadeIgual{
all a,b:Paleta | a.velocidade = b.velocidade
}
check TodaPaletaTemVelocidadeIgual for 5

assert TodoBicoTemEstadoIgual{
all a,b:BicoEjetor | a.estado = b.estado
}
check TodoBicoTemEstadoIgual for 5


fact{
	
	all b:Parabrisa| #QuantDePaletas[b] = 2
	all x:Paleta |  #QuantDeParabrisas[x] =1
	all a,b:Paleta | (#QuantDeBicos[a] = 1 and #QuantDeBicos[b] = 1) or (#QuantDeBicos[a] = 2 and #QuantDeBicos[b] = 2)
	all a,b:BicoEjetor | MesmoEstadoDosBicosEjetores[a,b]
	all a,b:Paleta | MesmaVelocidadeDasPaletas[a,b]
	all b: BicoEjetor | one b.~bicos
	all a: Velocidade | some a.~velocidade
	all a: Estado | some a.~estado

}

pred show[]{}
run show for 5
