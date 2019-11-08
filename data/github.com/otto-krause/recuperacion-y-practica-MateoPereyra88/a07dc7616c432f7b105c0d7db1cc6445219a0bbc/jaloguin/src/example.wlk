class Barrio{
	var ciudadanos
	constructor(ciu){
		ciudadanos = [] + ciu
	}
	method RegistrarCiudadano(ciu){
		if(ciu.all({c => c.esta_solito()}))
		ciudadanos.add(ciu)
		else
		throw new Exception("debe ingresar ninios")
	}
	method ciudadanos() = ciudadanos
	method vestimentasUsadas() = ciudadanos.flatMap({c => c.vestimenta()})
	method NiniosConMasCaramelos() = ciudadanos.sortedBy({a,b => a.bolsa() > b.bolsa()}).take(3)
	method ElementosNoRepetidos() = self.vestimentasUsadas().filter({v=>self.vestimentasUsadas().occurrencesOf(v) == 1})
	
	// esto es algo que se me ocurrio pero no lo puse por que pense que me podria traer problemas
	//method ElementosUsadosPorLosGolosos() = new Barrio(ciudadanos.filter({c=>c.bolsa()>10})).vestimentas().asSet()
	method ElementosUsadosPorLosGolosos() = ciudadanos.filter({c=>c.bolsa()>10}).flatMap({g=>g.vestimenta()}).asSet()
}
class Legion{
	var integrantes
	constructor(i){
		if(i.size()<2)
		throw new Exception("la legion debe tener almenos 2 integrante")
		else
		integrantes = i
	}
	method susto() = integrantes.sum({i => i.susto()})
	method bolsa() = integrantes.sum({i => i.bolsa()})
	method BolsasPorSeparado() = self.IntegrantesSeparados().map({i => i.bolsa()})
	method recibir_caramelos(caramelos){
		self.IntegrantesSeparados().max({i => i.susto()}).recibir_caramelos(caramelos)
	}
	method asustar(adulto){
		adulto.entregar_caramelos(self)
	}
	method IntegrantesSeparados() = integrantes.flatMap({i => i.IntegrantesSeparados()})
}
class Ninio{
	var vestimenta
	var actitudBase
	var estado = saludale
	var bolsa = 0
	constructor(ves, ac){
		vestimenta = [] + ves
		if(ac.between(1,10))
		actitudBase = ac
		else
		throw new Exception("la actitud debe ser un numero del 1-10")
	}
	method vestimenta() = vestimenta
	method bolsa() = bolsa
	method actitud() = actitudBase * estado.afecta()
	method susto() = self.actitud()*vestimenta.sum({v=>v.susto()})
	method BolsasPorSeparado() = [bolsa]
	method IntegrantesSeparados() = [self]
	method recibir_caramelos(caramelos){
		bolsa += caramelos
	}
	method asustar(adulto){
		adulto.entregar_caramelos(self)
	}
	method comer_caramelos(cantidad){
		if(cantidad.min(bolsa) >= 10)
		estado.empeorar(self)
		bolsa = 0.max(bolsa-cantidad)
	}
	method cambiar_estado(estadoNuevo){
		estado = estadoNuevo
	}
}
object saludale{
	method afecta() = 1
	method empeorar(ninio){
		ninio.cambiar_estado(empachado)
	}
}
object empachado{
	method afecta() = 0.5
	method empeorar(ninio){
		ninio.cambiar_estado(enLaCama)
	}
}
object enLaCama{
	method afecta() = 0
	method empeorar(ninio){}
}
class Maquillaje{
	var susto = 3
	method cambiar_susto(valor){
		susto = valor
	}
	method susto() = susto
}
class Traje{
	var susto
	constructor(s){
		susto = s
	}
	method susto() = susto
}
class Tierno inherits Traje{
	constructor() = super(2)
}
class Terrorifico inherits Traje{
	constructor() = super(5)
}
class Adulto{
	var sobrecargados = 1
	method asustado(susto) = self.tolerancia() < susto
	method caramelos_que_entrega() = self.tolerancia()/2
	method tolerancia() = sobrecargados*10
	method entregar_caramelos(ninioos){
		sobrecargados += ninioos.BolsasPorSeparado().count({n => n > 15})
		if(self.asustado(ninioos.susto()))
		ninioos.recibir_caramelos(self.caramelos_que_entrega())
	}
}
class Abuelo inherits Adulto{
	override method asustado(susto) = true
	override method caramelos_que_entrega() = super()/2
}
class Necio inherits Adulto{
	override method asustado(susto) = false
}