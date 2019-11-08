class Archivo {

	var nombre
	var contenido

	method nombre() {
		return nombre
	}

	method agregarContenido(nuevoContenido) {
		contenido = contenido + nuevoContenido
	}

	method eliminarContenido(contenidoAEliminar) {
		if (!self.existeContenido(contenidoAEliminar)) {
			self.error("No existe el contenido especificado en el archivo")
		}
		contenido.reverse()
		contenido.drop(contenidoAEliminar.size())
		contenido.reverse()
	}

	method existeContenido(unContenido) {
		var contenidoDelFinal = contenido.takeRight(unContenido.size())
		return contenidoDelFinal == unContenido
	}

}

class Carpeta {

	var nombre
	var property archivos = #{}

	method existeArchivoConNombre(nombreArchivo) {
		return archivos.any({ archivoDeLaCarpeta => archivoDeLaCarpeta.nombre() == nombreArchivo })
	}

	method agregarArchivo(archivo) {
		if (self.existeArchivoConNombre(archivo.nombre())) {
			self.error("No se puede agregar un archivo con un nombre ya existente.")
		}
		archivos.add(archivo)
	}

	method eliminarArchivo(nombreArchivo) {
		self.verificarArchivoExistente(nombreArchivo)
		var archivoAEliminar = self.buscarArchivo(nombreArchivo)
		archivos.remove(archivoAEliminar)
	}

	method verificarArchivoExistente(nombreArchivo) {
		if (!self.existeArchivoConNombre(nombreArchivo)) {
			self.error("No existe el archivo especificado")
		}
	}

	method buscarArchivo(nombreArchivo) {
		return archivos.find({ archivoDeLaCarpeta => archivoDeLaCarpeta.nombre() == nombreArchivo })
	}

	method agregarContenido(nombreArchivo, contenido) {
		self.verificarArchivoExistente(nombreArchivo)
		var archivo = self.buscarArchivo(nombreArchivo)
		archivo.agregarContenido(contenido)
	}

	method eliminarContenido(nombreArchivo, contenido) {
		self.verificarArchivoExistente(nombreArchivo)
		var archivo = self.buscarArchivo(nombreArchivo)
		archivo.eliminarContenido(contenido)
	}

}

class Commit {
	var carpeta
	var descripcion
	var cambios = []
	
	method carpeta(){
		return carpeta
	}
	
	method aplicarCambios(){
		cambios.forEach({cambio => cambio.aplicar(carpeta)})
	}
}

class Crear {

	var nombreDelArchivo

	method aplicar(carpeta) {
		var archivoAAgregar = new Archivo(nombre = nombreDelArchivo, contenido = "")
		carpeta.agregarArchivo(archivoAAgregar)
	}

}

class Eliminar {

	var nombreDelArchivo

	method aplicar(carpeta) {
		var archivoAAgregar = new Archivo(nombre = nombreDelArchivo, contenido = "")
		carpeta.agregarArchivo(archivoAAgregar)
	}

}

class Agregar {

	var nombreDelArchivo
	var nuevoContenido

	method aplicar(carpeta) {
		carpeta.agrearContenido(nombreDelArchivo, nuevoContenido)
	}

}

class Sacar {

	var nombreDelArchivo
	var nuevoContenido

	method aplicar(carpeta) {
		carpeta.eliminarContenido(nombreDelArchivo, nuevoContenido)
	}

}

