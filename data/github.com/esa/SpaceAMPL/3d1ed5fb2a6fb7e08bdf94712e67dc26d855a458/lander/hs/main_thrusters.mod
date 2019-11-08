#####################################################################################
#
# Problem:  Optimal Control Problem (OCP)
# Dynamics: Variable mass point in 2D with one main engine and 2 lateral engines for pitch control
# Transcription: Hermite-Simpson
#
# Author: Dario Izzo (Nov 2012)
#
#####################################################################################


#Parameters---------------------------------------------------
#Generic 
	param n:=20;				#Numbers of nodes
	param g0:=9.81;				#[m/s^2] Earth gravity constant at sea level
	param gmoon:=1.6229;			#[m/s^2] Moon gravity constant
	param pi:= 4*atan(1);			#definition of pi!!!!

#Spacecraft
	param R:=3;				#[m] Radius of the spacecraft (disc)
	param Isp:=311;				#[s] Specific Impulse of the engine, Low Thrust = 2000s, Chemical =300s 
	param maxthrust:=44000;			#[N] max Thrust (main engine)
	param maxthrustL:=880;			#[N] max Thrust (lateral engines)

#Initial Conditions
	param x0:=0;				#[m] Initial x
	param z0:=2300;				#[m] Initial z (height
	param vx0:=150;				#[m/s] initial velocity in x 
	param vz0:=-44;				#[m/s] initial Velocity in z
	param theta0:=-60*3.14159/180;		#[rad] initial pitch
	param omega0:=0;			#[rad/s] initial pitch rate
	param m0:=9472.06;			#[kg] initial total mass	

#Final Conditions
	param xn:=8900;				#[m] final x position (used only for pin-point landing)
	param zn:=10;				#[m] final z position
	param vxn:=0;				#[m/s] final velocity x
	param vzn:=-2.5;			#[m/s] final velocity z
	param thetan:=-16*3.14159/180;		#[rad] final pitch angle
	param omegan:=0.001;			#[rad/s] final pitch rate

#Other
	param tn:=(vz0 + sqrt(vz0^2 + 2*z0*gmoon))/gmoon; #[s] Guess for the final time
#-------------------------------------------------------------	

#Sets---------------------------------------------------------
	set I := 1..n;
	set J := 1..n-1;
#-------------------------------------------------------------

#Variables---------------------------------------------------
	var x{i in I};
	var vx{i in I};
	var z{i in I};
	var vz{i in I};
	var theta{i in I}, <= pi/2, >= -pi/2;   #state 5
	var omega{i in I};
	var m{i in I}, >=0;
	var u1{i in I}, >=0, <=maxthrust;
	var u1m{i in J}, >=0, <= maxthrust;
	var uL{i in I}, >=0, <=maxthrustL;
	var uLm{i in J}, >=0, <=maxthrustL;
	var uR{i in I}, >=0, <=maxthrustL;
	var uRm{i in J}, >=0, <=maxthrustL;
#-------------------------------------------------------------

#Time variables-----------------------------------------------
	var tf, >=0;
	var dt = tf/(n-1);
	var timegrid{i in I} = dt*(i-1);
#-------------------------------------------------------------

#Objective----------------------------------------------------
	#minimize tiempo: tf;
	#maximize massa: m[n];
	minimize fuel: m0-m[n];
#-------------------------------------------------------------

#Dynamic at the grid points-----------------------------------
	var f1{i in I} = vx[i];
	var f2{i in I} = (u1[i]+uL[i]+uR[i]) * sin(theta[i]) / m[i];
	var f3{i in I} = vz[i];
	var f4{i in I} = (u1[i]+uL[i]+uR[i]) * cos(theta[i]) / m[i] - gmoon;
	var f5{i in I} = omega[i];
	var f6{i in I} = (uR[i]-uL[i]) / m[i] / R;
	var f7{i in I} = -(u1[i]+uR[i]+uL[i]) / (Isp*g0);
#-----------------------------------------------------------------------

#State definition at mid-points via Simpson interpolation---------------
	var xm{i in J} 		= 	(x[i] + x[i+1])/2 + tf/(n-1)/8 * (f1[i] - f1[i+1]);
	var vxm{i in J} 	= 	(vx[i] + vx[i+1])/2 + tf/(n-1)/8 * (f2[i] - f2[i+1]);
	var zm{i in J} 		= 	(z[i] + z[i+1])/2 + tf/(n-1)/8 * (f3[i] - f3[i+1]);
	var vzm{i in J} 	= 	(vz[i] + vz[i+1])/2 + tf/(n-1)/8 * (f4[i] - f4[i+1]);
	var thetam{i in J}	= 	(theta[i] + theta[i+1])/2 + tf/(n-1)/8 * (f5[i] - f5[i+1]);
	var omegam{i in J} 	= 	(omega[i] + omega[i+1])/2 + tf/(n-1)/8 * (f6[i] - f6[i+1]);
	var mm{i in J}		= 	(m[i] + m[i+1])/2 + tf/(n-1)/8 * (f7[i] - f7[i+1]);
#-----------------------------------------------------------------------

#Dynamic at the mid-points----------------------------------------------
	var f1m{i in J} = vxm[i];
	var f2m{i in J} = (u1m[i]+uLm[i]+uRm[i])*sin(thetam[i])/mm[i];
	var f3m{i in J} = vzm[i];
	var f4m{i in J} = (u1m[i]+uLm[i]+uRm[i])*cos(thetam[i])/mm[i] - gmoon;
	var f5m{i in J} = omegam[i];
	var f6m{i in J} = (uRm[i]-uLm[i]) / mm[i]/ R;
	var f7m{i in J} = -(u1m[i]+uRm[i]+uLm[i]) / (Isp*g0);
#-----------------------------------------------------------------------

#Hermite Formula---------------------------------------------------------
subject to 
	dynamicx{i in J}:  x[i]  = x[i+1] - tf/(n-1)/6*(f1[i] + f1[i+1] + 4*f1m[i]);
	dynamicvx{i in J}:  vx[i]  = vx[i+1]  - tf/(n-1)/6*(f2[i] + f2[i+1] + 4*f2m[i]);
	dynamicz{i in J}:  z[i]  = z[i+1]  - tf/(n-1)/6*(f3[i] + f3[i+1] + 4*f3m[i]);
	dynamicvz{i in J}: vz[i] = vz[i+1] - tf/(n-1)/6*(f4[i] + f4[i+1] + 4*f4m[i]);
	dynamictheta{i in J}: theta[i] = theta[i+1] - tf/(n-1)/6*(f5[i] + f5[i+1] + 4*f5m[i]);
	dynamicomega{i in J}: omega[i] = omega[i+1] - tf/(n-1)/6*(f6[i] + f6[i+1] + 4*f6m[i]);
	dynamicm{i in J}:  m[i]  = m[i+1]  - tf/(n-1)/6*(f7[i] + f7[i+1] + 4*f7m[i]);
#--------------------------------------------------------------------------	

#Constraints------------------------------------------
	#Boundary Conditions
	subject to InitialPositionx: x[1] = x0;
	subject to InitialPositionz: z[1] = z0;
	subject to InitialVelocityx: vx[1] = vx0;
	subject to InitialVelocityz: vz[1] = vz0;
	subject to InitialPitch: theta[1] = theta0;
	subject to InitialPitchRate: omega[1] = omega0;
	subject to InitialMass: m[1] = m0;

	#Un-Comment the line below for pin-point landing
#	subject to FinalPositionx: x[n] >= xn;
	subject to FinalPositionz: z[n] <= zn;       
	subject to FinalVelocityz: vz[n] >= vzn;
	subject to FinalVelocityx: vx[n] <= vxn;
#	subject to FinalPitch: theta[n] >= thetan;
     
#-------------------------------------------------------------

#Guess-------------------------------------------------------
	let tf := tn;
	let {i in I} m[i] := m0;
#-------------------------------------------------------------

#Solver Options-----------------------------------------------
	option solver ipopt;
	option substout 0;
	option show_stats 1;
	options ipopt_options "outlev=5 max_iter=10000 tol=1e-5";
	options snopt_options "outlev=2 Major_iterations=2000 Superbasics=1500";
#-------------------------------------------------------------

#Solve!!!-----------------------------------------------------
	solve;
#-------------------------------------------------------------

#Print the Solution with midpoints---------------------------
	for {i in J}
	{
	printf "%17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e\n", timegrid[i],  m[i], x[i], vx[i], z[i], vz[i], theta[i], omega[i], u1[i], uR[i], uL[i]> out/sol.out;
	printf "%17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e\n", timegrid[i] + dt/2, mm[i], xm[i], vxm[i], zm[i], vzm[i], thetam[i], omegam[i], u1m[i], uRm[i], uLm[i] > out/sol.out;
	}
	printf "%17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e %17.16e\n", timegrid[n],  m[n], x[n], vx[n], z[n], vz[n], theta[n], omega[n], u1[n], uR[n], uL[n] > out/sol.out;
	close out/sol.out;
#-------------------------------------------------------------
