import wollok.game.*
import otros_obj.*
import tablero.*
// NOTAS:

// Estos enemigos comparten las mismas caracteristicas pero van a diferir en la cantidad de vida y ataque que tienen, asi como tambien el objeto que 
// guardan, que puede ser un "HongoSalud" que varia en efecto o una llave , para que el jugador se vea obligado a atacarlos.


class Enemigo inherits NoColisionable {
	var  imagenes = []
	var  hp
	var  atk 
	var position
	var contador = 0 //cuando el contador llega a 3, ataca al personaje y reinicia el contador. Va sumando uno cada vez que recibe un ataque.
	const inventario = [] 
	var imagen 
		
	method asignarPosicion(pos){position = pos}
	
	method image() = imagen
	
	method imagenAtk()
	
	method aniadir(objeto)= inventario.add(objeto)

	method serInteractuadoPor(jugador) {
		self.reciboAtaque(jugador)
	}
	
	method reciboAtaque(personaje) {
		imagen = self.imagenAtk()
		hp -= personaje.ataque()
		contador += 1
		self.atacoSiEsPosible(personaje)
		if (not self.sigoVivo()) {
			self.morir()
		}
	}
	
	method atacoSiEsPosible(personaje){
		if (contador == 3 ) {
			self.ataco(personaje)
			contador = 0
			}
		}
	
	method ataco(personaje){
		      personaje.esAtacado(atk)
	}

	method sigoVivo() {
		return hp > 0
	}

	method morir() {
		
		game.removeVisual(self)
		observerEnemigos.eliminar(self)
		game.addVisualIn(inventario.head(),position)
		
	}
	
	 
	method tipo()
	
	method nacer(_hp,_atk) {
		hp = _hp
		atk = _atk
	}
	 
	 method imagenesE(){
	 
	 }
	 method animacion() {
		 
			self.imagenesE()	
			imagen=  imagenes.first()
			imagenes.remove(imagen)
			imagenes.add(imagen)	
}
	
}
// Estos objetos son para que los diferentes enemigos hereden sus respectivas imagenes para usar de animaciones.

class Bowser inherits Enemigo {
	
	override method imagenAtk() = "bowser_atk.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "bowser"
	override method imagenesE(){
		imagenes.add("bowser1.png")
		imagenes.add("bowser2.png")
	}
	method crear()=new Bowser()
}
const browser=new Bowser()
class Zelda inherits Enemigo {

	override method imagenAtk() = "zelda_atk.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "zelda"
	override method imagenesE(){
		imagenes.add("zelda1.png")
		imagenes.add("zelda2.png")
	}
	method crear()=new Zelda()
}
const zelda=new Zelda()


class Pacman inherits Enemigo {

	override method imagenAtk() = "pacman_atk.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "pacman"
	
	override method imagenesE(){
		imagenes.add("pacman1.png")
		imagenes.add("pacman2.png")
	}
    method crear()=new Pacman()
}
const pacman = new Pacman()
class Metroid inherits Enemigo {

	override method imagenAtk() = "metroid_atk.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "metroid"
	override method imagenesE(){
		imagenes.add("metroid1.png")
		imagenes.add("metroid2.png")
	}
	method crear()= new Metroid()
}
const metroid= new Metroid()
class Donkey inherits Enemigo {

	override method imagenAtk() = "donkey_atk.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "donkey"
	override method imagenesE(){
		imagenes.add("donkey1.png")
		imagenes.add("donkey2.png")
	}
 method crear()= new Donkey()
}
const donkey=new Donkey()
class MegaMan inherits Enemigo {
	
	override method imagenAtk() = "megaManH.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "megaMan"
	override method imagenesE(){
		imagenes.add("megaMan1.png")
		imagenes.add("megaMan2.png")
	}
	method crear()=new MegaMan()	
	}
const megaMan = new MegaMan()	
class Link inherits Enemigo{
	override method imagenAtk() = "link_atk.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "link"
	override method imagenesE(){
		imagenes.add("link1.png")
		imagenes.add("link2.png")
	}
method crear()=new Link()
}
const link=new Link()
class Dragon inherits Enemigo{
	override method imagenAtk() = "dragonAtacado.png" //la imagen que se muestra cuando es atacado por el jugador
	
	override method tipo() = "dragon"
	override method imagenesE(){
		imagenes.add("dragon1.png")
		imagenes.add("dragon2.png")
	}
method crear()=new Dragon()
}
const dragon= new Dragon()