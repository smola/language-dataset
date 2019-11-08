class Pokemon {
	
	var property stamina
	var estado
	var item
	
	
	var caracteriticas
	var especie
	
	// Punto 1
	
	method puedeRealizar(rutina){
		return rutina.puedeRealizarla(self) &&
		       estado.equials(knockout).negate()
	}
	
	method tieneItem(unItem) {
		return unItem.equals(item)
	}
	
	// Punto 2
	method ternura() = caracteriticas.ternura() + especie.ternura()
	method velocidad() = caracteriticas.velocidad() + especie.velocidad()
	method inteligencia() = caracteriticas.inteligencia() + especie.inteligencia()
	
	//Punto 3
	method estaMaximizado() {
		return caracteriticas.estaMaximizado()
	}
	
	//Punto 4
	method realizar(unaRutina) {
		if (self.puedeRealizar(unaRutina).negate()){
			throw new RutinaException(message = "No puede hacer la rutina")
		}
		unaRutina.realizatePara(self)
	}
	
	
}

class RutinaException inherits Exception {
	
}

class Especie {
	//var nombre
	//var numeroPokedex
	
	var caracteristicas
	
	method ternura() = caracteristicas.ternura()
	method velocidad() = caracteristicas.velocidad()
	method inteligencia() = caracteristicas.inteligencia()
	
	//var evolucion
}

class CaracteristicasFundamentales {
	var property ternura
	var property velocidad
	var property inteligencia
	
	method estaMaximizado(){
		return self.suma().equals(510)
	}
	
	method suma() = ternura + velocidad + inteligencia
}

class EspecieFinal inherits Especie {
	
}

object knockout {
	
}

class Rutina {
	var staminaNecesaria
	var stanimaADisminuir
	
	method puedeRealizarla(pokemon){
		return pokemon.stamina() >= staminaNecesaria
	}
	
	method realizatePara(pokemon) {
		//pokemon
	}	
}

class Bucear inherits Surfear {
	var metros
	
	override method puedeRealizarla(pokemon){
		return super(pokemon) && pokemon.tiene("snorkel")
	}
	
	// Hace override del getter de Surfear
	override method kilometros() {
		return metros / 1000
	}
}

class Surfear inherits Rutina {
	var kilometros
	
	// Getter de kilometros
	method kilometros() = kilometros
	
	override method realizatePara(unPokemon){
		super(unPokemon)
		unPokemon.aumentarVelocidad(kilometros / 5)
		if (kilometros > 20) {
			unPokemon.knockeate()
			unPokemon.disminuirTernura(2)	
		}
	}
}