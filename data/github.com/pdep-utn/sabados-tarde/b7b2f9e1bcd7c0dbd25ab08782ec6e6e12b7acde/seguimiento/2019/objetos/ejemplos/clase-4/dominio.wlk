class Libro {
 	var precio = 100
 	var property nombre 
 	method esCaro() = precio > 500
	method aumentar(aumento) {  
		precio += aumento
	}
	method precio() = precio
}

class DVD {
	var property duracion = 60
	method esCaro() = self.precio() > 800
	method precio() = duracion * 3
}

object dolar {
	method esCaro() = true
}

object mariano{
	
	var apodo = "marian"
	const property cosas = []
	
	method comprar(cosa){
		if (!cosa.esCaro()) {
			//.. en algun momento se paga el libro
			cosas.add(cosa)
		}	
	}
	
}