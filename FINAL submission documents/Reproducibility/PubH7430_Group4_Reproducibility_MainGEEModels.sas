****************************************************************************
* 	PubH 7430 Project - Group 4
* 	main codes for GEE models 
* 
* 	Including: 
* 		1. 6 GEE models with logistic link identity function 
*  		   with AR-1 correlation structure
* 		2. 6 GEE models with logistic link identity function 
*    	   with exchangeable correlation structure for sensitivity analysis
* 		3. Primary outcome of interest: MI/Stroke
*  	  	   Secondary outcome of interest: Diabetes
* 		4. Primary predictor of interest: Age
*  
****************************************************************************;



****************************************************************************
* 	Effects of age, smoking status and gender on the incidence of STROKE/MI
****************************************************************************

*Direct filepath goes here. This should include the actual file name;
*quickest way--> ctrl+shift+right mouse click-->"copy as path"-->paste below.;
proc import datafile="C:\User\EXAMPLE\MISTRK_nona_casecontrol.csv"
dbms=csv out=MISTRK replace;
getnames=yes;
run;


*Model 1				GEE no interaction;
						proc genmod data=MISTRK DESC;
						class RANDID;
						model MISTRK = AGE TOTALCHOL SYSBP BMI / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=ar(1);
						run;

*Model 2				GEE with interaction AGE and CURRENT SMOKER;
						proc genmod data=MISTRK DESC;
						class RANDID;
						model MISTRK = AGE TOTALCHOL SYSBP BMI CURSMOKE AGE*CURSMOKE / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=ar(1);
						run;

*Model 3				GEE with interaction AGE and SEX;
						proc genmod data=MISTRK DESC;
						class RANDID SEX(REF='1');
						model MISTRK = AGE TOTALCHOL SYSBP BMI SEX AGE*SEX / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=ar(1);
						run;

****************************************************************************
* 	Effects of age, smoking status and gender on the incidence of DIABETES
****************************************************************************

*Direct filepath goes here. This should include the actual file name;
*quickest way--> ctrl+shift+right mouse click-->"copy as path"-->paste below.;
proc import datafile="C:\User\EXAMPLE\MISTRK_nona_casecontrol.csv"
dbms=csv out=DBTS replace;
getnames=yes;
run;


*Model 1				GEE no interaction;
						proc genmod data=DBTS DESC;
						class RANDID;
						model DIABETES = AGE TOTALCHOL SYSBP BMI / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=ar(1);
						run;

*Model 2				GEE with interaction AGE and CURRENT SMOKER;
						proc genmod data=DBTS DESC;
						class RANDID;
						model DIABETES = AGE TOTALCHOL SYSBP BMI CURSMOKE AGE*CURSMOKE / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=ar(1);
						run;

*Model 3				GEE with interaction AGE and SEX;
						proc genmod data=DBTS DESC;
						class RANDID SEX(REF='1');
						model DIABETES = AGE TOTALCHOL SYSBP BMI SEX AGE*SEX / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=ar(1);
						run;


****************************************************************************
* 						: 	Use exchangeable working correlation structure
* SENSATIVITY ANALYSIS 	:	We did not see many differences between GEE models 
* 						:	with these two different correlation structures.
****************************************************************************;

*MI/Stroke Models 1b-3b-------------------------------------------;

*Model 1b				GEE no interaction, EXCHANGEABLE structure;
						proc genmod data=MISTRK DESC;
						class RANDID;
						model MISTRK = AGE TOTALCHOL SYSBP BMI / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=exch;
						run;

*Model 2b				GEE with interaction AGE and CURRENT SMOKER, EXCHANGEABLE structure;
						proc genmod data=MISTRK DESC;
						class RANDID;
						model MISTRK = AGE TOTALCHOL SYSBP BMI CURSMOKE AGE*CURSMOKE / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=exch;
						run;

*Model 3b				GEE with interaction AGE and SEX, EXCHANGEABLE structure;
						proc genmod data=MISTRK DESC;
						class RANDID SEX(REF='1');
						model MISTRK = AGE TOTALCHOL SYSBP BMI SEX AGE*SEX / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=exch;
						run;

*Diabetes Models (4b-6b)------------------------------------------;

*Model 4b				GEE no interaction, EXCHANGEABLE structure;
						proc genmod data=DBTS DESC;
						class RANDID;
						model DIABETES = AGE TOTALCHOL SYSBP BMI / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=exch;
						run;

*Model 5b				GEE with interaction AGE and CURRENT SMOKER, EXCHANGEABLE structure;
						proc genmod data=DBTS DESC;
						class RANDID;
						model DIABETES = AGE TOTALCHOL SYSBP BMI CURSMOKE AGE*CURSMOKE / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=exch;
						run;

*Model 6b				GEE with interaction AGE and SEX, EXCHANGEABLE structure;
						proc genmod data=DBTS DESC;
						class RANDID SEX(REF='1');
						model DIABETES = AGE TOTALCHOL SYSBP BMI SEX AGE*SEX / dist=binomial link=logit type3 wald;
						REPEATED SUBJECT = RANDID / corr=exch;
						run;
