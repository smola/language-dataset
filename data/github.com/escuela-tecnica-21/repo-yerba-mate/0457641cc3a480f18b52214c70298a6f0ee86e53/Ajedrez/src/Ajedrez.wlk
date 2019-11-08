object juego
{
	var tablero = []
	
	method agregarPieza(pieza)
	{
		tablero.add(pieza)
	}
	method sacarPieza(pieza)
	{
		tablero.remove(pieza)
	}
	method saberPiezas()
	{
		return tablero
	}
	method saberNegras()
	{
		return tablero.filter({pieza => pieza.comprobarColor("Negro")})
	}
	method saberBlancas()
	{
		return tablero.filter({pieza => pieza.comprobarColor("Blanco")})
	}
	method comprobarPosicion(_x, _y)
	{
		return tablero.any({pieza => pieza.posicionColumna() == _y and pieza.posicionFila() == _x})
	}
	method iniciacion()
	{
		self.agregarPieza (new Peon    (color="Blanco",x=1,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=2,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=3,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=4,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=5,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=6,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=7,y=2))
		self.agregarPieza (new Peon    (color="Blanco",x=8,y=2))
		self.agregarPieza (new Peon    (color="Negro", x=1,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=2,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=3,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=4,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=5,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=6,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=7,y=7))
		self.agregarPieza (new Peon    (color="Negro", x=8,y=7))

		self.agregarPieza (new Rey     (color="Blanco",x=4,y=1))
		self.agregarPieza (new Rey     (color="Negro", x=4,y=8))

		self.agregarPieza (new Torre   (color="Blanco",x=1,y=1))
		self.agregarPieza (new Torre   (color="Blanco",x=8,y=1))
		self.agregarPieza (new Torre   (color="Negro", x=1,y=8))
		self.agregarPieza (new Torre   (color="Negro", x=8,y=8))

		self.agregarPieza (new Alfil   (color="Blanco",x=3,y=1))
		self.agregarPieza (new Alfil   (color="Blanco",x=6,y=1))
		self.agregarPieza (new Alfil   (color="Negro", x=3,y=8))
		self.agregarPieza (new Alfil   (color="Negro", x=6,y=8))

		self.agregarPieza (new Reina   (color="Blanco",x=5,y=1))
		self.agregarPieza (new Reina   (color="Negro", x=5,y=8))

		self.agregarPieza (new Caballo (color="Blanco",x=2,y=1))
		self.agregarPieza (new Caballo (color="Blanco",x=7,y=1))
		self.agregarPieza (new Caballo (color="Negro", x=2,y=8))
		self.agregarPieza (new Caballo (color="Negro", x=7,y=8))
	}
	
}

class Pieza
{
	var property color
	
	method mostrarColor()
	{
		return color
	}
	method comprobarColor(colorr)
	{
		return (colorr == self.mostrarColor())
	}
}

class Peon inherits Pieza 
{	
	const nombre = "Peon"
	var property x
	var property y
	var movimientoInicialDisponible = true
	
	const valor = 1

	method valorPieza()
	{
		return valor
	}
	method comprobarMovimiento(_x, _y)
	{
		if ((_x > 8) or (_x < 1) or (_y > 8) or (_y < 1))
		{
			return false
		}
		if (x == _x and y == _y)
		{
			return false
		}
		if (movimientoInicialDisponible)
		{
			movimientoInicialDisponible = false
			return ((_x == self.posicionFila() and _y == self.posicionColumna()+1) or (_x == self.posicionFila() and _y == self.posicionColumna()+2))
		}
		return (_x == self.posicionFila() and _y == self.posicionColumna()+1)
	}
	method posicionFila()
	{
		return x
	}
	method posicionColumna()
	{
		return y
	}
	method posicion()
	{
		return ("x = " + x + " y = " + y)
	}
	method cambiarFila(_x)
	{
		x = _x
	}
	method cambiarColumna(_y)
	{
		y = _y
	}
	method mover(_x, _y)
	{
		if (self.comprobarMovimiento(_x, _y))
		{
			self.cambiarFila(_x)
			self.cambiarColumna(_y)
			return "Se ha movido con exito"
		}
		return "Posicion Invalida" 
	}
	
}






class Caballo inherits Pieza
{
	const nombre = "Caballo"
	var property x
	var property y
	
	const valor = 3

	method valorPieza()
	{
		return valor
	}
	method comprobarMovimiento(_x, _y)
	{
		if ((_x > 8) or (_x < 1) or (_y > 8) or (_y < 1))
		{
			return false
		}
		if (x == _x and y == _y)
		{
			return false
		}
		var conteoX = x - _x 
		var conteoY = y - _y
		var absX = (conteoX).abs()
		var absY = (conteoY).abs()
		if ((absX == 2 and absY == 1) or (absY == 2 and absX == 1))
		{
			return true
		}
		return false
	}
	
	method posicionFila()
	{
		return x
	}
	method posicionColumna()
	{
		return y
	}
	method posicion()
	{
		return ("x = " + x + " y = " + y)
	}
	method cambiarFila(_x)
	{
		x = _x
	}
	method cambiarColumna(_y)
	{
		y = _y
	}
	method mover(_x, _y)
	{
		if (self.comprobarMovimiento(_x, _y))
		{
			self.cambiarFila(_x)
			self.cambiarColumna(_y)
			return "Se ha movido con exito"
		}
		return "Posicion Invalida" 
	}	
}




class Alfil inherits Pieza
{
	const nombre = "Alfil"
	var property x
	var property y
	
	const valor = 3

	method valorPieza()
	{
		return valor
	}
	method comprobarMovimiento(_x, _y)
	{
		if ((_x > 8) or (_x < 1) or (_y > 8) or (_y < 1))
		{
			return false
		}
		if (x == _x and y == _y)
		{
			return false
		}
		var conteoX = x - _x 
		var conteoY = y - _y
		var absX = (conteoX).abs()
		var absY = (conteoY).abs()
		return (absX == absY)
	}
	
	method posicionFila()
	{
		return x
	}
	method posicionColumna()
	{
		return y
	}
	method posicion()
	{
		return ("x = " + x + " y = " + y)
	}
	method cambiarFila(_x)
	{
		x = _x
	}
	method cambiarColumna(_y)
	{
		y = _y
	}
	method mover(_x, _y)
	{
		if (self.comprobarMovimiento(_x, _y))
		{
			self.cambiarFila(_x)
			self.cambiarColumna(_y)
			return "Se ha movido con exito"
		}
		return "Posicion Invalida" 
	}	
}




class Torre inherits Pieza
{
	const nombre = "Torre"
	var property x
	var property y
	
	const valor = 5

	method valorPieza()
	{
		return valor
	}
	method comprobarMovimiento(_x, _y)
	{
		if ((_x > 8) or (_x < 1) or (_y > 8) or (_y < 1))
		{
			return false
		}
		if (x == _x and y == _y)
		{
			return false
		}
		if ((_x != x and _y == y) or (_y != y and _x == x))
		{
			return true
		}
		return false
	}
	
	method posicionFila()
	{
		return x
	}
	method posicionColumna()
	{
		return y
	}
	method posicion()
	{
		return ("x = " + x + " y = " + y)
	}
	method cambiarFila(_x)
	{
		x = _x
	}
	method cambiarColumna(_y)
	{
		y = _y
	}
	method mover(_x, _y)
	{
		if (self.comprobarMovimiento(_x, _y))
		{
			self.cambiarFila(_x)
			self.cambiarColumna(_y)
			return "Se ha movido con exito"
		}
		return "Posicion Invalida" 
	}
}




class Reina inherits Pieza
{
	const nombre = "Reina"
	var property x
	var property y
	
	const valor = 9

	method valorPieza()
	{
		return valor
	}
	method comprobarMovimiento(_x, _y)
	{
		if ((_x > 8) or (_x < 1) or (_y > 8) or (_y < 1))
		{
			return false
		}
		if (x == _x and y == _y)
		{
			return false
		}
		var conteoX = x - _x 
		var conteoY = y - _y
		var absX = (conteoX).abs()
		var absY = (conteoY).abs()
		if (((_x != x and _y == y) or (_y != y and _x == x)) or (absX == absY))
		{
			return true
		}
		return false
	}
	
	method posicionFila()
	{
		return x
	}
	method posicionColumna()
	{
		return y
	}
	method posicion()
	{
		return ("x = " + x + " y = " + y)
	}
	method cambiarFila(_x)
	{
		x = _x
	}
	method cambiarColumna(_y)
	{
		y = _y
	}
	method mover(_x, _y)
	{
		if (self.comprobarMovimiento(_x, _y))
		{
			self.cambiarFila(_x)
			self.cambiarColumna(_y)
			return "Se ha movido con exito"
		}
		return "Posicion Invalida" 
	}
}




class Rey inherits Pieza
{
	const nombre = "Rey"
	var property x
	var property y
	
	const valor = "Partida"

	method valorPieza()
	{
		return valor
	}
	method comprobarMovimiento(_x, _y)
	{
		if ((_x > 8) or (_x < 1) or (_y > 8) or (_y < 1))
		{
			return false
		}
		if (x == _x and y == _y)
		{
			return false
		}
		if ((x-_x==0 or x-_x==1 or x-_x==-1) and ( y-_y==0 or y-_y==1 or y-_y==-1))
		{
			return true
		}
		return false
	}
	
	method posicionFila()
	{
		return x
	}
	method posicionColumna()
	{
		return y
	}
	method posicion()
	{
		return ("x = " + x + " y = " + y)
	}
	method cambiarFila(_x)
	{
		x = _x
	}
	method cambiarColumna(_y)
	{
		y = _y
	}
	method mover(_x, _y)
	{
		if (self.comprobarMovimiento(_x, _y))
		{
			self.cambiarFila(_x)
			self.cambiarColumna(_y)
			return "Se ha movido con exito"
		}
		return "Posicion Invalida" 
	}	
}
