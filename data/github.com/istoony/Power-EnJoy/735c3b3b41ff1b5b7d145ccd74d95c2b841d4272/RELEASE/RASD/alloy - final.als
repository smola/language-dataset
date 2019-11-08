module PEMS

/* 
Sig Part
*/

//The car have a CarStatus
abstract sig CarStatus{}

//the CarStatus can be Offline, Free or Reserved
one sig Offline	 extends CarStatus{}
one sig Free extends CarStatus{}
abstract sig Reserved extends CarStatus{}

//if a car is Reserved it can be Lock, Unlock or Going
one sig Lock extends Reserved{}
sig Unlock extends Reserved
{
	code: one PersonalCode
}
one sig Going extends Reserved{}

//boolean sig, needed for some checks
enum boolean{T, F}

//a car can be in charging 
enum Charge{isCharging, notCharging}
//BatteryStatus
enum BatteryStatus {Zero, LessThan20, MoreThan50}
//SensorsStatus
enum SensorStatus{NoOne, OnlyOne, TwoOtherMen}


//Car property
sig Car
{
	status: one CarStatus,
	battery: one BatteryStatus,
	plugged: one Charge,
	parkedIn: lone SPA, 		//one al più una
	sensor: one SensorStatus,
	MSOrequest: one boolean,
	engineON: one boolean
}


//All users that access the system
abstract sig User{}

//Code associated to only one User
sig PersonalCode{}

//Authenticated Users (AU) can reserve at most one car
sig AU extends User 
{
	status: one UserStatus,
	currentGA: one GA,
	code: one PersonalCode
}

//Non-Authenticated Users can only registers or log in the system
sig NonAU extends User{}

//The AU is Active or Suspended. Superclass UserStatus and 2 extended
enum UserStatus{Active, Suspended}

//The GA is the most larger area
sig GA {}

//like a park or something, is inside a GA
sig SPA {
	inside: one GA
}

//Is a special case of SPA
sig RA extends SPA 
{
	totSpaces: one Int,
	usedSpaces: one Int,
}{
	usedSpaces<=totSpaces
}

//Maintenance service
sig Onroad{
	operativeIn: one GA,
	assignedCars: set Car
}

//Payment and Reservation Manager
sig Date 
{
	timestamp: one Int
}{
	timestamp>0
}
enum Discount{NoDiscount, TwoOtherManDiscount, CarPluggedDiscount, MoreFiftyRBDiscount, LowBattFarDiscount}

sig Reservation
{
	user: one AU,
	car: one Car,
	discount: one Discount,
	reservationTime: one Date,
	unlockingTime: one Date,
	lockingTime: one Date, 
}

/* 
Pred Part
*/

//dates compare, if a is after b return true
pred DateAfter[a:Date, b:Date]
{
	a.timestamp > b.timestamp
}

//check the reservation property
pred ReservationAU [r: Reservation, u: AU]
{
	r.user = u
}

pred ReservationCar [r: Reservation, c: Car]
{
	r.car = c
}

pred Reserve[c:Car, u:AU]
{
	some r:Reservation | (ReservationAU[r,u] and ReservationCar[r,c])
}

//car unlocked by the right code
pred UnlockCar [c: Car, u: AU]
{
	c.status.code = u.code
}

//check if the car is in a reserved status
pred IsReserved[c: Car]
{
	c.status = Lock or c.status = Unlock or c.status = Going
}

//check if the car c need assistance
pred needAssistance[c:Car, o:Onroad]
{
	c.status =Offline and c.parkedIn.inside = o.operativeIn
}

//CarParked is true when the car C is parked in the SPA spa
pred CarParked [c: Car, spa: SPA]
{
	 spa = c.parkedIn
}

//check if the ride is over
pred EndRide[c:Car]
{
	(c.status = Going) and (c.engineON = F) and (c.sensor = NoOne)
}

/* 
Fact Part
*/

fact ReservationBasic
{
	//for all reservation exist a user and exist a car
	all r:Reservation {
		one u:AU | (ReservationAU[r,u] and u.status = Active)
	}
	all r:Reservation {
		one c:Car| (ReservationCar[r,c])
	}
	//no car has two reservation = No reservations have two cars
	all r1,r2:Reservation | (r1 !=r2) => (r1.car != r2.car)
	//no user has two reservation = No reservations have two AU
	all r1,r2:Reservation | (r1 !=r2) => (r1.user != r2.user)
}

fact BatteryProperty
{
	//All cars with battery = Zero have status = Offline 
	all c:Car | (c.battery = Zero) => (c.status = Offline)
}

//manage the car status of the system.
fact CarStatus
{
	//All reserved cars are in a valid state
	all r:Reservation | IsReserved[r.car]
	//all car not in Reservation Set are free or Offline
	(Car - Reservation.car).status = Free or (Car - Reservation.car).status = Offline
}

fact ReservationProperty{
 	//for all car if car is in Lock, Unlock or going status exist one and only one reservation for this car
	all c:Car | ((IsReserved[c]) <=> (one r:Reservation | ReservationCar[r,c]))
	//if C is offline or free, can not be reserved
	all c:Car | (c.status = Unlock) => (one u:AU | UnlockCar[c,u] and Reserve[c,u])
}

fact ReservationNumber
{
	//there are no two cars with the same unlock
	all c1,c2:Car{
			all ul1,ul2:Unlock | (c1 != c2 and c1.status = ul1 and c2.status = ul2) => (ul1 != ul2)
		}
	//if the car is reserved for one persone this car has the same code of the person
	all c:Car, u:AU | (Reserve[c,u] and c.status = Unlock) => (UnlockCar[c,u])	
	//Per ogni utente u1 per ogni utente u2. u1 != u2 sse i 2 codici sono diversi
	all u1, u2:AU | (u1 !=u2) => (u1.code != u2.code)
}

//for a better visualization
fact NoAloneProperty
{
	//no alone code
	all c:PersonalCode {
		some u:AU | u.code=c
	}
	//no date alone
	all d:Date {
		some r:Reservation | r.reservationTime=d or r.unlockingTime =d or r.lockingTime=d
	} 
}

fact UnlockProperty
{
	//Each Unlock is assigned to a Car
	all ul:Unlock | (one c:Car | ul = c.status)
	//Each different unlock have different Code
	all ul1,ul2:Unlock | ul1!=ul2 => ul1.code !=ul2.code

}

fact AUIsInTheCar
{
	//If car is Going or Unlock Sensor is != NoOne
	all c:Car | (c.status = Going or c.status = Unlock) => c.sensor != NoOne
	all c:Car | (c.status = Lock or c.status = Free or c.status = Offline) => c.sensor = NoOne
}

fact ParkedProperty
{
	//if car is free, Lock or Unlock exist one and only one SPA in ParkedIn
	all c:Car | (c.status = Free or c.status = Lock or c.status = Unlock or c.status = Offline) => (one spa:SPA | CarParked[c,spa])
	//if a car is in Charging it is in RA
	all c:Car | (c.plugged = isCharging) => (c.parkedIn = RA)
	//if a car is Charging it cannot move
	all c:Car | (c.plugged = isCharging) => (c.status != Going or c.status != Offline)
	//# of car in a RA is <= max space (come si fa????)
	//If a car is Going it is no parked
	all c:Car | (c.status = Going) => (no spa:SPA | CarParked[c,spa])
	
}

fact Discounts
{
	//se il motore è spenso non c'è discount
	all r:Reservation | (r.car.engineON = T and r.car.sensor = TwoOtherMen) <=> r.discount = TwoOtherManDiscount
	//se la corsa è finita e la batteria è più di 50 allora c'è lo sconto
	all r:Reservation | (EndRide[r.car] and r.car.battery = MoreThan50) <=> r.discount = MoreFiftyRBDiscount
	//se la corsa è finita e la batteria è meno del 20 allora extrapayment
	all r:Reservation | (EndRide[r.car] and r.car.battery = LessThan20) <=> r.discount =  LowBattFarDiscount
	//se la macchina è plugged allora sconto
	all r:Reservation | (EndRide[r.car] and r.car.plugged = isCharging) <=> r.discount = CarPluggedDiscount
}

fact GoingProperty
{
	//all car not going are really not going (engine off)
	all c:Car| (c.status != Going )=> c.engineON= F
}

fact MSOProperty
{
	//no  one can ask for MSO or have MSO when not going
	all c:Car | (c.status != Going => (c.MSOrequest = F))
	//some who ask for MSO after they verify it
	some c:Car | (c.status = Going => (c.MSOrequest = T))
}

fact RoadService
{
	//all GA have a service
	all g:GA | one o:Onroad | o.operativeIn = g
	//offline cars are taken care by the service in the same GA
	all c:Car {
		all o:Onroad |(needAssistance[c,o]<=> (c in o.assignedCars))
	}
}

/* 
Assertion Part
*/

assert ReservationCar
{
	//For all reserved cars the owner is not suspended
	all u:AU, c:Car | Reserve[c,u] => !(u.status = Suspended)
	//no car has two reservation -> it is a fact, is useless
	all u1:AU, u2:AU, c1:Car, c2:Car | (Reserve[c1,u1] and Reserve[c2,u2] and u1 != u2) => c1 != c2
	//for all free or Offline car there is no reservation
	all c:Car | (c.status = Free or c.status = Offline)=> (all u:AU | !Reserve[c,u])
	//not exist a reserved car without reserver
	all c:Car | (c.status = Reserved) => (one u:AU | Reserve[c,u])
}

assert SPAProperty
{
	//All SPA are in one and only one GA: 
	all spa1:SPA{
			all spa2:SPA | (spa1=spa2) => (spa1.inside = spa2.inside)
	}
	//se una macchina è parked allora ParkedIn = SPA
	all c:Car | (c.status = Free) => (one  spa:SPA | CarParked[c,spa])
	//If car is charging is in RA
	all c:Car | (c.plugged = isCharging) => (c.parkedIn = RA)
}

assert UniqueCode
{
	//there are no two different AUs with the same code
	no u1:AU, u2:AU | u1 != u2 and u1.code = u2.code
}

assert BatteryCheck
{
	all c:Car | (c.battery = Zero => c.status = Offline)
}

//check some constraints
assert Integrity
{
	//reservation integrity
	no r:Reservation|(#r.user>1 and #r.car>1)
	//car integrity
	no c:Car|#c.status>1
	//onroad integrity
	all c:Car|c.status = Offline => one o:Onroad| c in o.assignedCars
}



//show a feasible scenario
pred A
{}

run A for 4

//check assertion 
check ReservationCar for 10
check SPAProperty for 10
check UniqueCode for 10
check BatteryCheck for 10
check Integrity for 10
