import wollok.game.*
import powerUps.*
import players.*
import bombas.*

//HUD
class HudJugador{
	
	var elementos = []
	
	method generar(jugador,y){ game.onTick(500, "hudJugador1", { self.mostrarHUD(jugador,y) }
	) }
	
	method mostrarHUD(jugador,y){
		self.limpiarPantalla()
		self.mostrarCorazones(jugador,y)
		self.mostrarPowerUPs(jugador,y)
		elementos.forEach({ corazon => corazon.generar() })
	}
	
	method mostrarCorazones(jugador,y){

		(1..jugador.vidas()).forEach{ 
			n => elementos.add((powerUpAgregarVida.construir(self.calcularDondeColocarCorazon(n,y))))
		}
	}
	
	method limpiarPantalla(){
		elementos.forEach({ elemento => game.removeVisual(elemento) })
		elementos.clear()
		
	}
	
	method calcularDondeColocarCorazon(n,y) = game.at( 21+(n-1)%3,((33-n)/3)-y) 
	
	method mostrarPowerUPs(jugador,y){
	 	if (jugador.tipoDeBomba() == constructorDeBombaFuerte) self.mostrarBombaFuerte(y)
		if (jugador.bombaSticky()) self.mostrarBombaSticky(y)
		if (jugador.bombaRemota()) self.mostrarBombaRemota(y)
		
	}
	
	method mostrarBombaFuerte(y){
		elementos.add(new PowerUpBombaFuerteHUD(position = game.at(21,7-y)))
	}	
	
	method mostrarBombaSticky(y){
		elementos.add(new PowerUpBombaStickyHUD(position = game.at(22,7-y)))
	}
	
	method mostrarBombaRemota(y){
		elementos.add(new PowerUpBombaRemotaHUD(position = game.at(23,7-y)))
	}	
}

//ELEMENTOS DEL HUD
class PowersUPsHUD {
	
	var property position
	
	method generar(){
		game.addVisual(self)
	}
}

//ICONO DE BOMBA FUERTE
class PowerUpBombaFuerteHUD inherits PowersUPsHUD{
	
	method image() = "bomb03.png"

}


//ICONO DE BOMBA STCIKY
class PowerUpBombaStickyHUD inherits PowersUPsHUD{
	
	method image() = "powerupBombaSticky.png"

}

//ICONO DE BOMBA REMOTA
class PowerUpBombaRemotaHUD inherits PowersUPsHUD{
	
	method image() = "powerupBombaRemota.png"

}


