object fiat600 {
	var combustible = 10
	var aceite = 5
	var maxaceite = 7
	var motor = 0
	var vidaruedas = 1000
	var volante = 0
	var velocidad = 0
	var velocidadmaxima = 60
	var maxcombustible = 15
	var frenado = 0
	var direccion = 0
	var luces = 0
	var vidabateria = 5
	
	method girarllave(estadoMotor){
		motor = estadoMotor
		if(motor==1){
		if(vidabateria == 0){return "No se puede encender el motor, no hay bateria"}
		if(vidabateria > 0){
		combustible -= 0.25
		vidabateria -= 1
		return "Motor prendido"
	}
	}
	if(motor==0){
	return "Motor apagado"
	}
	return motor
	}
	method acelerar() {
		if(motor==0){
			velocidad = 0
			return "El motor no pudo encender"
		}
		aceite -= 0.5
		if(aceite <= 0){
			return "No hay aceite, es posible que el motor se dañe si este es encendido"
		}
	        if(combustible <= 0){
	        combustible = 0
			velocidad = 0
			return "No podemos acelerar más, no queda combustible disponible."
		}
		direccion = 1
		velocidad += 10
		if(velocidad>=1 && velocidad <=19)  combustible -= 0.25
		if(velocidad>=20 && velocidad <=39) combustible -= 0.5  
		if(velocidad>=40 && velocidad <=59) combustible -= 1    
		if(velocidad>=60) combustible -= 1.5  
		vidaruedas -= 20
		if(velocidad == velocidadmaxima) {
			velocidad -= 10
			return "No podemos acelerar más, ya alcanzamos el limite de velocidad"
		}
	        return "Estamos avanzando a " + velocidad + "km/h" 
		}
		method frenar() {
                frenado = 1
		velocidad -= 10
		vidaruedas -= 50
		if(velocidad <= 0 && frenado == 1) {
		        direccion = 3
		        velocidad +=5
			return "Estamos retrocediendo marcha atras" 
			}
		frenado = 0
		return "Estamos frenando"
	}
	
    method doblar(volantedir){
    	volante = volantedir
    	if(volante == 4 && direccion == 1){
    		return "Girando hacia la izquierda, direccion Norte"
    	}else if (volante == 4 && direccion == 3){
    		return "Girando hacia la izquierda, direccion Sur"
    	}
    	
    	if(volante == 2 && direccion == 1){
    		return "Girando hacia la derecha, direccion Norte"
    	}else if (volante == 2 && direccion == 3){
    		return "Girando hacia la derecha, direccion Sur"
    	}
    	if(motor == 0) {
	    return "El motor no pudo encender"
	    }
    	return "Girando"
    	}
    method cargacombustible(cantcombustible){
    	combustible += cantcombustible
    	if(combustible >= maxcombustible){
    	combustible = 15
    	return "No se puede cargar más combustible, tanque lleno"
    	}
    	return "Se han recargado " + combustible + "L"
    }
    method cargaaceite(cantaceite){
    aceite += cantaceite	
    	if(aceite >= maxaceite){
    		aceite = 7
    		return "No es posible cargar.El tanque de aceite ya esta lleno" 
    	}
    	return "El tanque de aceite esta lleno con " + aceite + "L"
    }
    method luces(estadoLuces){
    	luces = estadoLuces
    	if(luces == 1){
    	if(vidabateria == 0){return "No hay bateria, las luces no pueden encender"}	
    	if(vidabateria > 0){
    	vidabateria -= 1
    	return "Luces prendidas"
    	}}
    	if(luces == 0){
    		return "Luces apagadas"
    	}
    return luces}
    method cambiobateria(){
    if(vidabateria == 5){return "La bateria no necesita ser cambiada"}
    if(vidabateria <= 4){
    vidabateria = 5
    return "La bateria se cambio correctamente"	
    }return vidabateria 
    }
    }
object ferrari430 {
	var combustible = 30
	var aceite = 20
	var maxaceite = 30
	var motor = 0 
	var vidaruedas = 4000
	var volante = 0
	var velocidad = 0
	var velocidadmaxima = 300
	var maxcombustible = 40
	var frenado = 0
	var direccion = 0
	var estadotecho= 0
	var techoabierto = 1
	var techocerrado = 0
	var luces = 0
	var vidabateria = 10
	
	method girarllave(estadoMotor){
		motor = estadoMotor
		if(motor==1){
		if(vidabateria == 0){return "No se puede encender el motor, no hay bateria"}
		if(vidabateria > 0){
		combustible -= 0.5
		vidabateria -= 1
		return "Motor prendido"
	}
	}
	if(motor==0){
	return "Motor apagado"
	}
	return motor
	}
	method acelerar() {
		if(motor==0){
			velocidad = 0
			return "El motor no pudo encender"
		}
		aceite -= 1
		if(aceite <= 0){
			return "No hay aceite, es posible que el motor se dañe si este es encendido"
		}
	        if(combustible <= 0){
			velocidad = 0
			return "No podemos acelerar más, no queda combustible disponible."
		}
		direccion = 1
		velocidad += 30
		if(velocidad>=1 && velocidad <=49)  combustible -= 0.5     
		if(velocidad>=50 && velocidad <=99) combustible -= 1      
		if(velocidad>=100 && velocidad <=199) combustible -= 1.5   
		if(velocidad>=200) combustible -= 2     
		vidaruedas -= 50
		if(velocidad == velocidadmaxima) {
			velocidad -= 30
			return "No podemos acelerar más, ya alcanzamos el limite de velocidad"
		}
	        return "Estamos avanzando a " + velocidad + "km/h" 
		}
		method frenar() {
                frenado = 1
		velocidad -= 30
		vidaruedas -= 70
		if(velocidad <= 0 && frenado == 1) {
		        direccion = 3
		        velocidad +=5
			return "Estamos retrocediendo marcha atras"                                                            
		}
		frenado = 0
		return "Estamos frenando"
	}
	
    method doblar(volanteDir){
    	volante = volanteDir
    	if(volante == 4 && direccion == 1){
    		return "Girando hacia la izquierda, direccion Norte"
    	}else if (volante == 4 && direccion == 3){
    		return "Girando hacia la izquierda, direccion Sur"
    	}
    	
    	if(volante == 2 && direccion == 1){
    		return "Girando hacia la derecha, direccion Norte"
    	}else if (volante == 2 && direccion == 3){
    		return "Girando hacia la derecha, direccion Sur"
    	}
    	if( motor == 0){
	     return "El motor no pudo encender"
	     }
    	return "Girando"
    	}
    method cargacombustible(cantcombustible){
    	combustible += cantcombustible
    	if(combustible >= maxcombustible){
    	combustible = 40
    	return "No se puede cargar más combustible, tanque lleno"
    	}
    	return "Se han recargado " + combustible + "L"
    }
    method abrirtecho(){
    if(estadotecho == techocerrado){
    estadotecho = techoabierto
    return ("El techo se abre")
    }
    else{
    return "El techo ya está abierto" 	
    }   
    }
    method cerrartecho(){
    if(estadotecho == techoabierto){
    estadotecho = techocerrado
    return ("El techo se cierra")
    }
    else{
    return "El techo ya está cerrado" 	
    }   
    }
    method cargaaceite(cantaceite){
    aceite += cantaceite	
    	if(aceite >= maxaceite){
    		aceite = 30
    		return "No es posible cargar. El tanque de aceite ya esta lleno" 
    	}
    	return "Tanque de aceite lleno"
    }
     method luces(estadoLuces){
    	luces = estadoLuces
    	if(luces == 1){
    	if(vidabateria == 0){return "No hay bateria, las luces no pueden encender"}	
    	if(vidabateria > 0){
    	vidabateria -= 1
    	return "Luces prendidas"
    	}}
    	if(luces == 0){
    		return "Luces apagadas"
    	}
    return luces}
    method cambiobateria(){
    if(vidabateria == 5){return "La bateria no necesita ser cambiada"}
    if(vidabateria <= 4){
    vidabateria = 5
    return "La bateria se cambio correctamente"	
    }return vidabateria 
    }
    }