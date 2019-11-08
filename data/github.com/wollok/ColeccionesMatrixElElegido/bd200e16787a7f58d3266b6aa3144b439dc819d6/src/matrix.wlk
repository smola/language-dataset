object nave {
	var pasajeros = [morfeo,neo,trinity]
	
	method sube(alguien){
		pasajeros.add(alguien)
	}
	method baja(alguien){
		pasajeros.remove(alguien)
	}
	method cantidadPasajeros() {
		return pasajeros.size()
	}
	
	method chocar() {
		pasajeros.forEach({pasajero=>pasajero.saltar()})
		pasajeros.clear()
	}
	
	method estaElElegido(){
		return pasajeros.any({pasajero => pasajero.esElElegido()})
	}
	method acelerar(){
		pasajeros.filter({pasajero => not pasajero.esElElegido()})
			.forEach({pasajero=>pasajero.saltar()})
	}
	method pasajeroMasVital() {
		return pasajeros.max{ pasajero=> pasajero.vitalidad()}
	}
	method pasajeroMenosVital() {
		return pasajeros.min{ pasajero=> pasajero.vitalidad()}
	}
	method estaEquilibrada(){
		return self.pasajeroMasVital().vitalidad() < self.pasajeroMenosVital().vitalidad()*2  
	}
	 
}

object neo {
 	var energia = 100
 	method esElElegido(){
 		return true
 	}
 	method saltar() {
 		energia = energia/2
 	}
 	method vitalidad() {
 		return energia/10
 	}
}
object morfeo{
	var property cansancio = false
	var property vitalidad = 8
	
	method esElElegido(){
 		return false
 	}
 	method saltar() {
 		cansancio = not cansancio
 		vitalidad = vitalidad -1
 	}
}

object trinity {
	method vitalidad(){
 		return 0
 	}
 	method saltar() {}
 	method esElElegido() = false 	
}