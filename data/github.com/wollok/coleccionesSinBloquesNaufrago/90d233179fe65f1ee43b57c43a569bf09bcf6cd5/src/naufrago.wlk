object chuck {
	var amigos = [diego,claudio]
	
	method nuevoAmigo(_amigo) {
		amigos.add(_amigo)
	}
	
	method elegidos(){
		return amigos + [wilson.ultimaNovia()] 
	}
	/* Mas complicado
	method elegidos1() {
		var elegidos = []
		elegidos.addAll(amigos)
		elegidos.add(wilson.ultimaNovia())
		return elegidos
	}*/
	
	/* Solucion no valida. Retorna bien, pero tiene efecto: modifica a chuck y no queremos eso.
	method elegidos3(){
		amigos.add(wilson.ultimaNovia())
		return amigos 
	} 
	*/
	
	method primerAmigo() {
		return amigos.first()
	}
	
	method puedenIrJuntos(){
		return self.primerAmigo().seLlevaBien(wilson.segundaNovia())
	}
	/* Mas complicado, con variables auxiliares
	method puedenIrJuntos(){
		var amigo =  amigos.first()
		var novia = wilson.segundaNovia()
		return amigo.seLlevaBien(novia)
	} */
		
	method robarNovias(){
		amigos.addAll(wilson.mitadNovias())
		wilson.quitarMitadNovias()
	}
	
}


object wilson {
	var novias = [claudia, erika, ivana]
	
	method nuevaNovia(_novias) {
		novias.add(_novias)
	}
	
	method ultimaNovia() {
		return novias.last()
	}
	
	method segundaNovia() {
		return novias.get(1)
	}
		
	method mitadNovias(){
		return novias.take(novias.size()/2)
	}
	
	/* Mas complicado, con una list auxiliar 
	method mitadNovias(){
		var mitad = novias.size()/2
		var sublista = []
		sublista.addAll(novias.take(mitad))
		return sublista
	}*/
		
	method quitarMitadNovias(){
		novias.removeAll(self.mitadNovias())
	}
	/* Variante interesante 
	method quitarMitadNovias3(){
		novias = novias.drop(novias.size()/2)
	}*/
	
	/*Mas complicado
	method quitarMitadNovias(){
		var mitad = novias.size() / 2 
		var fin = novias.size()
		var sublista = []
		sublista.addAll(novias.subList(mitad, fin)) 
		novias.clear()
		novias.addAll(sublista)
	}*/
}
object diego{
	var amigos=[cintia,silvia,erika]
	method seLlevaBien(novia){
		return amigos.contains(novia)
	}
}
object claudio{
	method seLlevaBien(novia){
		return false
	}
}
object jose{
	method seLlevaBien(novia){
		return novia == erika
	}
}

object carlos{
	method seLlevaBien(_novia){
		return true
	}
}

object erika{}
object silvia{}
object claudia{}
object ivana{}
object cintia{}
	
