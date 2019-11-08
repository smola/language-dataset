%--------------------------------------------------------------------------
% Modelo RBC basico
% La funcion de utilidad corresponde a la forma CRRA
% Codigo elaborado por: Carlos Rojas Quiroz
%--------------------------------------------------------------------------

var y c innv g lab kap r w z;
predetermined_variables kap;
varexo e_z e_g;
parameters alpha delta betta sigma kappa eta rho_z rho_g 
z_ss lab_ss r_ss  kap_ss w_ss y_ss c_ss inv_ss g_ss C_Y I_Y G_Y;

alpha  = 0.650;
delta  = 0.025;
betta  = 0.99;
sigma  = 1.00;
rho_z  = 0.919919;
rho_g  = 0.954402;
z_ss   = 1;
eta    = 1;
G_Y = 0.180;
lab_ss = 0.394235908847945;
y_ss   = z_ss^(1/alpha)*((1-alpha)*betta/(1-betta+betta*delta))^((1-alpha)/alpha)*lab_ss;
kappa  = (alpha*1/(1-((1-alpha)*betta*delta/(1-betta+betta*delta))-G_Y)^sigma)*(y_ss^(1-eta*sigma))/(lab_ss^(1+eta));
w_ss   = alpha*y_ss/lab_ss;
kap_ss = (1-alpha)*betta/(1-betta+betta*delta)*y_ss;
inv_ss = delta*kap_ss;
r_ss   = (1-alpha)*y_ss/kap_ss-delta;
c_ss   = ((1-betta+alpha*betta*delta)/(1-betta+betta*delta)-G_Y)*y_ss;
g_ss   = G_Y*y_ss;
C_Y    = c_ss/y_ss;
I_Y    = inv_ss/y_ss;

model;
exp(w)          = kappa*(exp(lab)^eta)*(exp(c)^sigma);
exp(c)^(-sigma) = betta*exp(c(+1))^(-sigma)*(1+exp(r(+1)));
exp(w)          = alpha*exp(y)/exp(lab);
exp(r)+delta    = (1-alpha)*exp(y)/exp(kap);
exp(y)          = exp(c)+exp(innv)+exp(g);
exp(kap(+1))    = (1-delta)*exp(kap)+exp(innv);
exp(y)          = exp(z)*exp(kap)^(1-alpha)*exp(lab)^alpha;
z               = (1-rho_z)*log(z_ss) + rho_z*z(-1) + e_z;
g               = (1-rho_g)*log(g_ss) + rho_g*g(-1) + e_g;
end;

steady_state_model;
lab =log(lab_ss);
c   =log(c_ss); 
w   =log(w_ss); 
r   =log(r_ss); 
y   =log(y_ss); 
kap =log(kap_ss); 
innv=log(inv_ss); 
z   =log(z_ss);
g   =log(g_ss);
end;

shocks;

var e_z; stderr 0.008289*100;
var e_g; stderr 0.003740*100;
end;
 
resid;
steady;
check;

stoch_simul(order = 1, bandpass_filter=[8 32], nograph);
