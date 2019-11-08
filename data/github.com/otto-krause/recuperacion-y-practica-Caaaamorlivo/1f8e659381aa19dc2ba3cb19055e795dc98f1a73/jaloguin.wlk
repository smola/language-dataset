class Barrio{
	var ciudadanos;
	constructor(c){
		ciudadanos = c;
	}
	method niniosConMasCaramelos(){
		return ciudadanos.sortedby({c=>c.CaramelosIndividuales()>c.CaramelosIndividuales()}).take(3);
	}
	method elementosUsados(){
		if(ciudadanos.CaramelosIndividuales()>10){
			ciudadanos.devolverVestimenta();
			ciudadanos.devolverMaquillaje();
		}
	}
}
class Legion{
	var ninios;
	constructor(i){
		if(i.size()<2){
			throw new Exception("La legion debe tener al menos 2 integrantes");
		}
		else{
		ninios = i;	
		}
	}
	method capacidadDesusto(){
		return ninios.sum({i=>i.susto()});
	}
	method devolverCaramelos(){
		return ninios.sum({i=>i.bolsa()});
	}
	method CaramelosPorSeparado(){
		return self.integrantes().map({i=>i.bolsa()});
	} 
	method obtenerCaramelos(caramelos){
		self.integrantes().max({i=>i.susto()}).obtenerCaramelos(caramelos);
	}
	method asustar(adulto){
		adulto.entregarCaramelos(self)
	}
	method integrantes(){
		return ninios.flatMap({i=>i.integrantes()});
	} 
}
class Ninio{
	var traje;
	var maquillaje;
	var actitud;
	var caramelos=0;
	var estado = sano; 
	constructor(t,a,m){
		traje = t;
		maquillaje = m;
		if(a.between(1,10)){
			actitud = a;	
		}
		else{
			throw new Exception("los parametros de la actitud no se encuentran en el rango (1 - 10)")
		}
	}
	method devolverVestimenta() {
		return traje;
	}
	method devolverMaquillaje(){
		return maquillaje;
	} 
	method devolverCaramelos(){
		return caramelos;
	} 
	method capacidadDeAsustar() {
		return self.calcularActitud()*traje.sum({t=>t.capacidadDesusto()});
	}
	method CaramelosIndividuales(){
		return [caramelos]; 
	} 
	method Integrantes(){
		return [self]; 
	} 
	method calcularActitud(){
		return actitud * estado.afectarActitud();
	}
	method obtenerCaramelos(cantidad){
		caramelos += cantidad;
	}
	method asustar(adulto){
		adulto.entregarCaramelos(self);
	}
	
	method comerCaramelos(cantidad){
		caramelos = 0.max(caramelos - cantidad);
		if(cantidad.min(caramelos)>10){
			estado.empeorar(self);
		}
	}
	method cambiarEstado(nuevoestado){
		estado = nuevoestado;
	}
}
class Maquillaje{
	var susto = 3;
	method capacidadDeSusto(){
		return 3;
	}
	method cambiarSusto(valor){
		susto = valor;
	}
}
class Traje{
	var susto;
	constructor(s){
		susto = s;
	}
	method capacidadDeSusto(){
		return susto; 
	}
}
class TrajeTierno inherits Traje{
	constructor()=super(2)
}
class TrajeTerrorifico inherits Traje{
	constructor()=super(5)
}
class AdultoComun{
	var sobrecargados = 1;
	method asustado(susto){
		return self.tolerancia()<susto;
	} 
	method tolerancia(){
		return sobrecargados*10;
	}
	method caramelosAentregar(){
		return self.tolerancia () /2 ;
	} 
	method entregarCaramelos(legion){
		sobrecargados += legion.BolsasPorSeparado().count({l=>l > 15});
		if(self.asustado(legion.susto())){
			legion.obtenerCaramelos(self.caramelosAentregar());
		}
	}
}
class Abuelo inherits AdultoComun{
	override method asustado(susto){
		return true;
	}
	override method caramelosAentregar(){
		return super()/2;
	}
}
class AdultoNecio inherits AdultoComun{
	override method asustado(susto) = false;
}
object sano{
	method afectarActitud(){
		return 1;
	} 
	method empeorar(ninio){
		ninio.cambiarEstado(empachado);
	}
}
object empachado{
	method afectarActitud(){
	 return	0.5;
	}
	method empeorar(ninio){
		ninio.cambiarEstado(enLaCama);
	}
}
object enLaCama{
	method afectarActitud(){
		return 0;
	}
	method empeorar(ninio){
		ninio.cambiarEstado(self);
	}
}
