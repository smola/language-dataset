import especies.*
import wollok.game.*

object hector{
	var property plantasCosechadas = []
	var property position = game.at(2,3)
	var property ganancias 
	
	method move(nuevaPosicion)= self.position(nuevaPosicion)
	method image()= "player.png"
	method sembrar(especie){ return game.addVisualIn(especie,self.position())}
	
	method regar(){
		const colisiones = game.colliders(self)
				if (colisiones.isEmpty()){
					game.say(self, "No hay nada que regar")
				}else colisiones.head().crecer()
	}
	
//////////////////////////////////////////////////////////////////////////////////////////////////	
	method cosechar(){
		const colisiones = game.colliders(self)
			if (colisiones.isEmpty()){
					game.say(self,"No hay nada que cosechar")
			}else self.cosecha(colisiones)	
			}
			
	method cosecha(colisiones){if (colisiones.head().puedeSerCosechada()){ 
									plantasCosechadas.add(colisiones.head())}}		

///////////////////////////////////////////////////////////////////////////////////////////////////			
	
	method vender() {
	 self.ganancias(plantasCosechadas.forEach {planta => planta.precio() })
			}
}


