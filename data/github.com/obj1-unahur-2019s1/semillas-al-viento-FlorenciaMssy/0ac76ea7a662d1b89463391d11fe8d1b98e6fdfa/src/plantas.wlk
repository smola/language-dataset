import parcelas.*
//SuperClase
class Planta {
	
var property altura
var property anioObtencionSemillas

method esFuerte () {
	return self.horasToleranciaDeSol() > 10
}
method daSemillas(){
	return self.esFuerte()
}
method horasToleranciaDeSol()

method meAsocioBien(parcela){
	return parcela.plantaSeAsociaBien(self)
}
}

//Plantas

//Menta
class Menta inherits Planta{

override method horasToleranciaDeSol(){
	return 6
}
method cuantoEspacioOcupa(){
	return altura * 3
}
override method daSemillas(){
	return super() || altura > 0.40

}
method parcelaIdeal (parcela){
	return parcela.superficie() > 6
}
}
//Soja
class Soja inherits Planta {
	
override method horasToleranciaDeSol(){
	if(altura < 0.5){return 6}
	else if(altura.between(0.5,1)){return 7}
		else(return 9)
}

override method daSemillas(){
	return super() || (anioObtencionSemillas > 2007) && altura > 1
}
method cuantoEspacioOcupa(){
	return altura / 2
}
method parcelaIdeal(parcela){
	return parcela.horasDeSolPorDia() == self.horasToleranciaDeSol()
}
}
//Quinoa
class Quinoa inherits Planta {
	
var property horasToleranciaDeSol

override method daSemillas(){
	return super() || anioObtencionSemillas < 2005
}
method espacioQueOcupa(){
	return 0.5
}
method parcelaIdeal(parcela){
	return not parcela.plantas().any({planta => planta.altura()>1.5})
}
}
//SojaTransegnica
class SojaTransgenica inherits Soja{

override method daSemillas(){
	return false
}
override method parcelaIdeal(parcela){
	return parcela.plantas().size() == 1
}
}
//HierbaBuena
class HierbaBuena inherits Menta{

override method cuantoEspacioOcupa(){
	return (super()) * 2
}
}