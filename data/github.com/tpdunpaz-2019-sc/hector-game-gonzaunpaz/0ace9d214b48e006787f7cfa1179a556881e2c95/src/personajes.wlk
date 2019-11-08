import wollok.game.*
object hector {
	var property position
	var energia = 100
	var pasos = 0
	
	method image() = "tony.png"
	
	method decirEnergia() {
		game.say(  self  , "Energia: " + energia  + " Pasos: " + pasos  )
	}
	
	method contarPasos() {
		pasos++
		if ( pasos % 10 == 0) energia--
	}
	
	method moveteHaciaArriba(){
		if (self.position().y() != game.height() - 1) {
			self.position( self.position().up(1)   )
			self.contarPasos()
		}
	}
	method moveteHaciaAbajo(){
		if (self.position().y() != 0 ) {
			self.position( self.position().down(1)   )
			self.contarPasos()
		}
	}
	method moveteHaciaDerecha(){
		if (self.position().x() != game.width() - 1 ){
			self.position( self.position().right(1)   )
			self.contarPasos()
		}
	}
	method moveteHaciaIzquierda(){
		if (self.position().x() != 0 ){
			self.position( self.position().left(1)   )
			self.contarPasos()
		}
	}	
	
}
