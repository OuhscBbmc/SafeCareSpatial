PROC IMPORT OUT= WORK.screenDat 
            DATAFILE= "C:\Users\dbard\Documents\GitHub\SafeCareSpatial\N
ational\forSas.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.realScreen 
            DATAFILE= "C:\Users\dbard\Documents\GitHub\SafeCareSpatial\N
ational\realScreen.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data realscreen2; set realscreen;
time = year - 2007;
lpop = log(childpopulation);
overdID = _n_;
lrefc = log(referralcount);
run;

proc nlmixed data=realscreen2;
parms b1=0 b2=0 b3=0 b4=0 mu0=-3 lsig0=-10 loverd=-10;
eta = b0 + b1*(time=1) + b2*(time=2) + b3*(time=3) + b4*(time=4) + lpop + err;
sigma0 = exp(lsig0);
overd = exp(loverd);
model referralcount ~ poisson(exp(eta));
random b0 ~ normal(mu0,sigma0) subject=statename;
random err ~ normal(0,overd) subject=overdID;
estimate 'random int var' sigma0;
estimate 'random over dispersion var' overd;
run;

proc nlmixed data=realscreen2;
*where year = 2011;
*parms b0=-3 loverd=-10;
parms b1=0 b2=0 b3=0 b4=0 mu0=-3 lsig0=-10 loverd=-10;
*eta = b0 + lrefc + err;
eta = b0 + b1*(time=1) + b2*(time=2) + b3*(time=3) + b4*(time=4) + lrefc + err;
sigma0 = exp(lsig0);
overd = exp(loverd);
model ScreenOutCount ~ poisson(exp(eta));
random b0 ~ normal(mu0,sigma0) subject=statename;
random err ~ normal(0,overd) subject=overdID;
estimate 'random int var' sigma0;
estimate 'random over dispersion var' overd;
run;

proc glimmix data=realscreen2;
class statename overdID time;
model referralcount = time / s dist=poisson offset=lpop;
random intercept / subject=statename;
*random overdID ;
run;

proc glimmix data=realscreen2;
class statename overdID time;
model ScreenInCount = time / s dist=poisson offset=lpop;
random intercept / subject=statename;
*random overdID ;
run;



data screenDat2; set screenDat;
lpop = log(childpopulation);
run;

proc genmod data=screenDat2;
model referralcount = / offset=lpop dist=poi scale=D;
run; 
