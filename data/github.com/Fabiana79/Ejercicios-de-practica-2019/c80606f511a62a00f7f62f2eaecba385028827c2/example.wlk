/*Ejercicio tipo parcial Dr.Casa 2019 
  Parte 1: Clases y Colecciones
*/
class Persona
   {	
	var enfermedades=[]
	var  property temp
	var  property celulas
	
	//cualquier persona puede contraer enfermedad,no puede contraer mas de 5
	method contraerEnfermedad(enfer) 
	 {
	 if (enfermedades.size()<5 ){enfermedades.add(enfer)}
	 else throw new Exception ("No se puede agregar la enfermedad,superaste 
                               la cantidad de enfermedades")
	 }
	//maximo de temperatura que puede tener,y lo deja en coma
	method estaEnComa() {return (temp >= 45 || celulas<1000000)}
	
	//la enfermedad Infecciosa aumenta la temperatura
	method incrementarTemp(cambioDeTemp) {temp+=cambioDeTemp}
	
    //cada día que vive una persona con su enfermedad se producen sus efectos.
	method viviUnDia() { enfermedades.forEach({enfer=> enfer.causaEfecto(self)})}
	
	
	/*es utilizada por la enfermedad autoInmune */
	method destruirCelulas(c) {celulas = celulas - c}
	
	/*4a.enfermedad que más células afecte */
	method enfermedadQueMasAfecta() 
	{return enfermedades.max({enfermedad => enfermedad.celulasQueAmenazo()})}
	
	
	//REVISAR
	method recibirMedicina(dosis)
	{
	enfermedades.forEach( {enfermedad => enfermedad.atenauda(dosis*15)} )
	enfermedades.removeAllSuchThat( {enf => enf.estaCurada()} )
	}
	
	//REVISAR
	method curado()
	{enfermedades.isEmpty()}	
		
	}

class EnfermedadInfecciosa  //malaria y otitis son infecciosas
   { 
	var  property celulasQueAmenazo
	
	/*efecto que provoca en la persona.Debe permitir recibir cualquier persona 
	como parámetro para que exista polimorifismo con cualquier enfermedad que causa 
	efecto.Deben tener mismo nombre y si retornan un valor deben respetarlo en la
	 estructura*/
	method causaEfecto(persona) 
	{persona.incrementarTemp(celulasQueAmenazo/1000)}
	
	/*se reproducen a si mismas*/
	method reproducirCelulas(){celulasQueAmenazo = 2*celulasQueAmenazo}
	
	/*es agresiva cuando la cantidad de células afectadas 
	supera el 10% de las células totales del cuerpo*/
	method agresiva(persona) {return celulasQueAmenazo > (0.1)*persona.celulas()}	
	
	
	
	/*tendria que reducir las células en función de la dosis
	que la persona reciba.Como hago?*/
	
	method atenuada(dosis){celulasQueAmenazo = celulasQueAmenazo - dosis}
	
   }
   
class EnfermedadAutoinmune  //lupus es autoInmune
   { 	
	var  property celulasQueAmenazo
	var property dia = 0
	
	 /*efecto que provoca en la persona,destruye la cantidade de celulas amenazadas 
	 Debe permitir recibir cualquier persona como parámetro para que exista
	 polimorifismo con cualquier enfermedad que causa efecto.Deben tener mismo 
	 nombre y si retornan un valor deben respetarlo en la estructura*/
	 method causaEfecto(persona)
	 {	
	  persona.destruirCelulas(celulasQueAmenazo)
	  dia += 1
	 } 
	 
	 method agresiva(persona) {return dia > 30 }
	 
	 
	 
	 /*REVISAR SI ASI SE ATENUAN LAS ENFERMEDADES*/
	 method atenuada(dosis){celulasQueAmenazo = celulasQueAmenazo - dosis}
	   
	    
    }
        
/*Las clases enfermedades Infecciosas y AutoInmunes son polimórficas.
Respetan la estructura polimórfica,al llamarse igual los métodos en 
común y también tienen retornos o no aquellos métodos que se llaman igual.

Se dice que hay bajo acoplamiento entre Persona y  las Enfermedades.
La Persona quien le envia mensajes no nota diferencia entre las 
enfermedades y es en este momento donde se aprovecha el polimorfismo.
*/


/*Parte 2: Herencia - Redefinición - Super */

class Medicos inherits Persona{
	
	var property dosis
	var property cant
	
	method atendePaciente(persona) 
	{
	 if ( enfermedades.size()>1 )
	 {persona.recibirMedicina(dosis)}
	 else throw new Exception("No está enfermo")
	}
	
	/*COMO CONTRAE UNA ENFERMEDAD UN MEDICO?*/	
	override method contraerEnfermedad(enfermedad)
	{
     super(enfermedad)
     self.atendePaciente(self)
	}
}

class JefeDepartamento inherits Medicos{
	
	var property persona
	var empleados=[]
	
	method contratarEmpleado(empleado){empleados.add(empleado)}
	
	override method atendePaciente(per){
	
	empleados.anyOne( {empleado => empleado.atendePaciente(per)} )
	
	}
	
}

class LaMuerte
   {
	method causaEfecto(persona){persona.temp(0)}
	
	method curado(){return false}
	
	method agresiva(persona){return true}
	   }

/*Para pensar:
6A)Aparece una nueva enfermedad que cualquier persona puede contraer.
 
¿Qué deberíamos saber de ella para poder representarla? 

RESPUESTA:Tiene que tener los mismos métodos,respetando el nombre,la cantidad de 
parámetros si tienen y respetar si son getters o setters.
 
 
¿Cuáles serían las alternativas de solución?

RESPUESTA:Deben ser polimórficas,de la siguiente manera:

class NuevaEnfermedad
{
 method causaEfecto(enfermedad)
 {el mensaje debe hacer una acción con el objeto enfermedad}

 method agresiva(persona){return un valor}
 }
 
 
 
6B)
¿Qué pasaría si todas las enfermedades matan las células que afectan?
 
RESPUESTA:

 
¿Y si las autoinmunes tuvieran otro efecto adicional?
 
RESPUESTA:Si el segundo efecto respeta la estructura polimórfica lo resolverá.
 
6C)Ahora queremos modelar una enfermedad como el sida, 
que es tanto infecciosa como autoinmune. ¿Cómo lo solucionamos?  
  
RESPUESTA:
  
 

 */
 
 