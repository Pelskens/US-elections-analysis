********************************************************************************
* Set filepath, import and exploration metadata					   Katrien MEERT
*
********************************************************************************

%let filepath = /folders/myfolders/statcomp2020/data;
/* %let filepath = H:/statmod2020/Project; */

proc import 
	datafile="&filepath/US_County_Level_Presidential_Results_12-16_.csv" DBMS=CSV
	out=presidential_election replace;
	guessingrows=max;	
run;

/* View metadata */
proc contents data=presidential_election;
proc print data=presidential_election (obs=50);
run;

********************************************************************************
* Data preparation						   						Philippe ELSKENS
*
********************************************************************************
/* Selection, renaming, labeling of variables */
/* 29 observations from Alaska removed */
/* In this state residential votes are counted by buroughs, not by county  */
data election;
	set presidential_election;
	where county_name NE "Alaska";
	LogPop = log(POP060210);
	Income = INC110213/1000;
	keep FIPS
		per_gop_2016 
		county_name
		AGE775214 
		RHI125214 
		EDU685213 
		LFE305213 
		PVY020213 
		POP060210 
		LogPop 
		Income; 
	rename per_gop_2016 = GOP 
		county_name = County 
		AGE775214 = Plus65 
		RHI125214 = White 
		EDU685213 = Bachelor 
		LFE305213 = Travel 
		PVY020213 = Poverty 
		POP060210 = PopDensity;
	label per_gop_2016 = GOP votes (percent) 
		Income = Median household income  (1000$)
		AGE775214 = Persons 65 years and over (percent) 
		RHI125214 = White alone (percent) 
		EDU685213 = Bachelors degree or higher (percent) 
		LFE305213 = Mean travel time to work (minutes) 
		PVY020213 = Persons below poverty level (percent) 
		POP060210 = Population per square mile 
		LogPop = Log-population per square mile;
run; 

proc contents data=election;
proc print data=election (obs=10);
run;

/* Define key variables using macro's */
%let independent_vars = Income Plus65 White Bachelor Travel Poverty LogPop;
%let ID = FIPS;

/* Confirm that there are no duplicates or missing data */
proc sort data=election out=election noduprecs dupout=election_dups;
	by FIPS;
proc means data=election nmiss;
	var &independent_vars GOP;
run;

********************************************************************************
* Descriptive statistics					  					   Katrien MEERT 
*
********************************************************************************
/* UNIVARIATE Analysis */
proc means data=election (drop=FIPS) mean median std lclm uclm min max maxdec=4;
	var &independent_vars GOP;
run;

ods trace on;
ods exclude Moments TestsForLocation Quantiles;
proc univariate data=election (drop=FIPS);
	var &independent_vars GOP;
	histogram / kernel;
	inset n mean median std / position=ne;
run;
ods trace off;
ods select all;

/* BIVARIATE Analysis */
proc corr data=election (drop=FIPS) plots=matrix (histogram nvar=all) 
		PLOTS(MAXPOINTS=1000000);
	var &independent_vars GOP;
run;
ods graphics / reset=all imagemap;

proc corr data=election (drop=FIPS) nosimple plots=scatter (nvar=all);
	var &independent_vars GOP;
run;

********************************************************************************
* Statistical analysis 
*
********************************************************************************
********************************************************************************
/* Randomly assign observations to TRAINING or VALIDATION sample set */
proc surveyselect data=election rate=.5 seed=12345 
	outall out=election_selected noprint;
data election_training (drop=Selected);
	set election_selected;
	where Selected=1;
data election_validation (drop=Selected);
	set election_selected;
	where Selected=0;
run;

********************************************************************************
/* SIMPLE LINEAR REGRESSION MODEL with main variable*/	
*											 					  Leander MEURIS
********************************************************************************
proc reg data=election_training;
	model GOP=Income/ partial;
	output out=resid r=rman p=pman;
run;

data residSLR;
	set resid;
	rman2=rman**2;
proc sgplot data=residSLR;
	scatter x=pman y=rman2;
	loess x=pman y=rman2;
	refline 0 / axis=y lineattrs=(color=red);
proc sgplot data=residSLR;
	scatter x=pman y=rman;
	loess x=pman y=rman;
	refline 0 / axis=y lineattrs=(color=red);
run;
												/* adjusted R squared = 0.0545*/
********************************************************************************
/* MULTIPLE REGRESSION 1st ROUND: Forward and backward */		  Leander MEURIS
*
********************************************************************************
proc reg data=election_training;
	model GOP=Income Poverty/ VIF partial;
	test Poverty=0;
	model GOP=Income Plus65/ VIF partial;
	Plus65: test Plus65=0;
	model GOP=Income Bachelor/ VIF partial;
	Bachelor: test Bachelor=0;
	model GOP=Income White/ VIF partial;
	White: test White=0;
	model GOP=Income Travel/ VIF partial;
	Travel: test Travel=0;
	model GOP=Income LogPop/ VIF partial;
	LogPop: test LogPop=0;
run;

/*Add to model: White */
proc reg data=election_training;
	model GOP=Income White/ VIF partial;
	White: test White=0;
	Income: test Income=0;
run;
												/* adjusted R squared = 0.3537*/
********************************************************************************
/* MULTIPLE REGRESSION 2nd ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=Income White Poverty/ VIF partial;
	Poverty: test Poverty=0;
	model GOP=Income White Plus65/ VIF partial;
	Plus65: test Plus65=0;
	model GOP=Income White Bachelor/ VIF partial;
	Bachelor: test Bachelor=0;
	model GOP=Income White Travel/ VIF partial;
	Travel: test Travel=0;
	model GOP=Income White LogPop/ VIF partial;
	LogPop: test LogPop=0;
run;

/*Add variable to model: Bachelor */
proc reg data=election_training;
	model GOP=Income White Bachelor/ VIF partial;
	Income: test Income=0;
	White: test White=0;
	Bachelor: test Bachelor=0;
run;

												/* adjusted R squared = 0.5024*/
********************************************************************************	
/* MULTIPLE REGRESSION 3rd ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=Income White Bachelor Poverty/ VIF;
	Poverty: test Poverty=0;
	model GOP=Income White Bachelor Plus65/ VIF;
	Plus65: test Plus65=0;
	model GOP=Income White Bachelor Travel/ VIF;
	Travel: test Travel=0;
	model GOP=Income White Bachelor LogPop/ VIF;
	LogPop: test LogPop=0;
run;

/*Add variable to model: LogPop */
proc reg data=election_training;
	model GOP=Income White Bachelor LogPop/ VIF partial;
	Income: test Income=0;
	White: test White=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
run;

												/* adjusted R squared = 0.5334*/
********************************************************************************	
/* MULTIPLE REGRESSION 4th ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=Income White Bachelor LogPop Poverty/ VIF;
	Poverty: test Poverty=0;
	model GOP=Income White Bachelor LogPop Plus65/ VIF;
	Plus65: test Plus65=0;
	model GOP=Income White Bachelor LogPop Travel/ VIF;
	Travel: test Travel=0;
run;

/* Income not significant anymore */
proc reg data=election_training;
	model GOP=Income White Bachelor LogPop Poverty/ VIF partial;
	Income: test Income=0;
	White: test White=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0;
run;

/*Add variable to model: Poverty */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty/ VIF partial;
	White: test White=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0;
run;

												/* adjusted R squared = 0.5376*/
********************************************************************************	
/* MULTIPLE REGRESSION 5th ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Plus65/ VIF partial;
	test Plus65=0;
	model GOP=White Bachelor LogPop Poverty Travel/ VIF partial;
	test Travel=0;
	model GOP=White Bachelor LogPop Poverty Income/ VIF partial;
	test Income=0;
run;

/*Add variable to model: Travel */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel/ VIF partial;
	White: test White=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0;
	Travel: test Travel=0;
run;

												/* adjusted R squared = 0.5398*/
********************************************************************************
/* Plus65 and Income not significant */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel Plus65/ VIF partial;
	model GOP=White Bachelor LogPop Poverty Travel Income/ VIF partial;
run;

********************************************************************************
/* Variable TRANSFORMATION */									Philippe ELSKENS
********************************************************************************
data election_training;
	set election_training;
	wortelGOP=sqrt(GOP);
	logGOP=log(GOP);
	GOP_inv=1/GOP;
run;

/* Try model with transformed dependent variable*/
proc reg data=election_training;
	model wortelGOP=White Bachelor LogPop Poverty Travel/ VIF partial;
	model logGOP=White Bachelor LogPop Poverty Travel/ VIF partial;
	model GOP_inv=White Bachelor LogPop Poverty Travel/ VIF partial;
	White: test White=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0;
	Travel: test Travel=0;
run;

********************************************************************************
/* Check ASSUMPTIONS */									 Matthias VANDEKERCKHOVE
********************************************************************************
/* NORMALITY: verify qqplot of (studentised) residuals */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel/ VIF partial;
	output out=residfull r=rman p=pman student=student;
run;

/* LINEARITY: verify plot of (studentised) residuals vs predicted values */
/* HOMOSCADESTICITY: verify (squared) residuals vs predicted values */
data residfull2;
	set residfull;
	rman2=rman**2;
proc sgplot data=residfull2;
	scatter x=pman y=rman;
	loess x=pman y=rman;
	refline 0 / axis=y lineattrs=(color=red);
proc sgplot data=residfull2;
	scatter x=pman y=rman2;
	loess x=pman y=rman2;
	refline 0 / axis=y lineattrs=(color=red);
run;

/* FINAL MAIN effects: White, Bachelor, LogPop, Poverty, Travel */
********************************************************************************
/* INTERACTIONS to test */							   	 Matthias VANDEKERCKHOVE			
********************************************************************************
data election_training;
	set election_training;
	IntWhiteBach=White*Bachelor;
	IntWhiteLogPop=White*LogPop;
	IntWhitePoverty=White*Poverty;
	IntWhiteTravel=White*Travel;
	IntBachLogPop=Bachelor*LogPop;
	IntBachPoverty=Bachelor*Poverty;
	IntBachTravel=Bachelor*Travel;
	IntLogPopPoverty=LogPop*Poverty;
	IntLogPopTravel=LogPop*Travel;
	IntPovertyTravel=Poverty*Travel;
run;

********************************************************************************
/* MULTIPLE REGRESSION 6th ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhiteBach/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntWhiteLogPop/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntWhiteTravel/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntBachLogPop/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntBachPoverty/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntBachTravel/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntLogPopPoverty/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntLogPopTravel/ partial;
	model GOP=White Bachelor LogPop Poverty Travel IntPovertyTravel/ partial;
	model GOP=White Bachelor LogPop Poverty Travel Income/ partial;
	model GOP=White Bachelor LogPop Poverty Travel Plus65/ partial;
run;
/* Although some interactions are significant,  */
/* adjusted R^2 is barely impacted */

/*Add variable to model: IntWhitePoverty */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty/ partial;
	White: test White=0, IntWhitePoverty=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0, IntWhitePoverty=0;
	Travel: test Travel=0;
run;
												/* adjusted R squared = 0.5436*/
********************************************************************************
/* MULTIPLE REGRESSION 7th ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteBach;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntLogPopTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntLogPopPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntPovertyTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty Plus65;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty Income;
run;

/*Add variable to model: IntWhiteTravel */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel/ partial;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0, IntWhitePoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0;
run;
												/* adjusted R squared = 0.5502*/
********************************************************************************
/* MULTIPLE REGRESSION 8th ROUND: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntWhiteBach;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntWhiteLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntPovertyTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		Plus65;
run;

/*Add variable to model: IntLogPopTravel */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel IntLogPopTravel;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0, IntLogPopTravel=0;
	Poverty: test Poverty=0, IntWhitePoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0;
run;
												/* adjusted R squared = 0.5536*/
********************************************************************************
/* MULTIPLE REGRESSION 9th round: Forward and backward */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteBach;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntLogPopPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel Plus65;
run;

/*Add variable to model: IntPovertyTravel */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0, IntLogPopTravel=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntPovertyTravel=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0, IntPovertyTravel=0;
run;
												/* adjusted R squared = 0.5568*/
********************************************************************************
/* MULTIPLE REGRESSION 10th round: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteBach;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntLogPopPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel Plus65;
run;

/*Add variable to model: IntWhiteLogPop */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0, IntWhiteLogPop=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0, IntLogPopTravel=0, IntWhiteLogPop=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntPovertyTravel=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0, IntPovertyTravel=0;
run;
												/* adjusted R squared = 0.5573*/
********************************************************************************
/* MULTIPLE REGRESSION 11th round: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntWhiteBach;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop Plus65;
run;

/*Add variable to model: IntLogPopPoverty */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0, IntWhiteLogPop=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0, IntLogPopTravel=0, IntWhiteLogPop=0, IntLogPopPoverty=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntPovertyTravel=0, 
		IntLogPopPoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0, IntPovertyTravel=0;
run;
												/* adjusted R squared = 0.5601*/
********************************************************************************
/* MULTIPLE REGRESSION 12th round: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntWhiteBach;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		Plus65;
run;

/*Add variable to model: IntWhiteBach */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0, IntWhiteLogPop=0, 
		IntWhiteBach=0;
	Bachelor: test Bachelor=0, IntWhiteBach=0;
	LogPop: test LogPop=0, IntLogPopTravel=0, IntWhiteLogPop=0, IntLogPopPoverty=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntPovertyTravel=0, 
		IntLogPopPoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0, IntPovertyTravel=0;
	run;
												/* adjusted R squared = 0.5607*/
********************************************************************************
/* MULTIPLE REGRESSION 13th round: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach IntBachPoverty;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach Plus65;
run;

/*Add variable to model: IntBachPoverty */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach IntBachPoverty;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0, IntWhiteLogPop=0, 
		IntWhiteBach=0;
	Bachelor: test Bachelor=0, IntWhiteBach=0, IntBachPoverty=0;
	LogPop: test LogPop=0, IntLogPopTravel=0, IntWhiteLogPop=0, IntLogPopPoverty=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntPovertyTravel=0, 
		IntLogPopPoverty=0, IntBachPoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0, IntPovertyTravel=0;
run;
												/* adjusted R squared = 0.5626*/
********************************************************************************
/* MULTIPLE REGRESSION 14th round: Forward and backward  */
proc reg data=election_training;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntWhiteBach IntBachPoverty IntBachLogPop;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntWhiteBach IntBachPoverty IntBachTravel;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntWhiteBach IntBachPoverty Income;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty 
		IntWhiteBach IntBachPoverty Plus65;
run;
/* No more significant terms */

********************************************************************************
/* CENTERING variables to reduce multicollinearity  */
/* between main effects and their related interaction terms */	   Katrien MEERT
********************************************************************************
proc stdize data=election_training method=mean out=centelection;
	var White Bachelor LogPop Poverty Travel;
run;

/* Check again for MULTICOLLINEARITY using VIF's */
data centelection2;
	set centelection;
	IntWhitePoverty=White*Poverty;
	IntWhiteTravel=White*Travel;
	IntLogPopTravel=LogPop*Travel;
	IntPovertyTravel=Poverty*Travel;
	IntWhiteLogPop=White*LogPop;
	IntLogPopPoverty=LogPop*Poverty;
	IntWhiteBach=White*Bachelor;
	IntBachPoverty=Bachelor*Poverty;
run;

/* Model WITHOUT interactions */
proc reg data=centelection2;
	model GOP=White Bachelor LogPop Poverty Travel/ VIF partial;
	White: test White=0;
	Bachelor: test Bachelor=0;
	LogPop: test LogPop=0;
	Poverty: test Poverty=0;
	Travel: test Travel=0;
run;
											/* max VIF on centered data = 1.83*/
********************************************************************************
/* /* Model WITH interactions */ */
proc reg data=centelection2;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntPovertyTravel IntWhiteLogPop 
		IntLogPopPoverty IntWhiteBach IntBachPoverty/ VIF;
run;
											/* max VIF on centered data = 2.93*/
********************************************************************************
/* Check again if all main variables and interaction terms are significant. */
proc reg data=centelection2;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntPovertyTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach 
		IntBachPoverty;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0, IntWhiteLogPop=0, 
		IntWhiteBach=0;
	Bachelor: test Bachelor=0, IntWhiteBach=0, IntBachPoverty=0;
	LogPop: test LogPop=0, IntLogPopTravel=0, IntWhiteLogPop=0, IntLogPopPoverty=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntPovertyTravel=0, 
		IntLogPopPoverty=0, IntBachPoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0, IntPovertyTravel=0;
	White_apart: test White=0;
	Bachelor_apart: test Bachelor=0;
	LogPop_apart: test LogPop=0;
	Poverty_apart: test Poverty=0;
	Travel_apart: test Travel=0;
	IntWhitePoverty: test IntWhitePoverty=0;
	IntWhiteTravel: test IntWhiteTravel=0;
	IntLogPopTravel: test IntLogPopTravel=0;
	IntPovertyTravel: test IntPovertyTravel=0;
	IntWhiteLogPop: test IntWhiteLogPop=0;
	IntLogPopPoverty: test IntLogPopPoverty=0;
	IntWhiteBach: test IntWhiteBach=0;
	IntBachPoverty: test IntBachPoverty=0;
run;
	
/* IntPovertyTravel NOT SIGNIFICANT anymore after centering. */
/* Take out, check again.  */
proc reg data=centelection2;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach 
		IntBachPoverty/VIF;
	White: test White=0, IntWhitePoverty=0, IntWhiteTravel=0, IntWhiteLogPop=0, 
		IntWhiteBach=0;
	Bachelor: test Bachelor=0, IntWhiteBach=0, IntBachPoverty=0;
	LogPop: test LogPop=0, IntLogPopTravel=0, IntWhiteLogPop=0, IntLogPopPoverty=0;
	Poverty: test Poverty=0, IntWhitePoverty=0, IntLogPopPoverty=0, 
		IntBachPoverty=0;
	Travel: test Travel=0, IntWhiteTravel=0, IntLogPopTravel=0;
	White_apart: test White=0;
	Bachelor_apart: test Bachelor=0;
	LogPop_apart: test LogPop=0;
	Poverty_apart: test Poverty=0;
	Travel_apart: test Travel=0;
	IntWhitePoverty: test IntWhitePoverty=0;
	IntWhiteTravel: test IntWhiteTravel=0;
	IntLogPopTravel: test IntLogPopTravel=0;
	IntWhiteLogPop: test IntWhiteLogPop=0;
	IntLogPopPoverty: test IntLogPopPoverty=0;
	IntWhiteBach: test IntWhiteBach=0;
	IntBachPoverty: test IntBachPoverty=0;
run;
												/* adjusted R squared = 0.5613*/
********************************************************************************
********************************************************************************
/* Final model: GOP = White Bachelor LogPop Poverty Travel */				   *
/* IntWhitePoverty IntWhiteTravel IntLogPopTravel IntWhiteLogPop  */		   *
/* IntLogPopPoverty IntWhiteBach IntBachPoverty */							   *
********************************************************************************
********************************************************************************

/* Check ASSUMPTIONS */									 Matthias VANDEKERCKHOVE
********************************************************************************
/* NORMALITY: verify qqplot of (studentised) residuals */
proc reg data=centelection2;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach IntBachPoverty/VIF;
	output out=residfull r=rman p=pman student=student;
proc reg data=centelection2;
	model GOP=White Bachelor LogPop Poverty Travel/VIF;
	output out=residmain r=rman p=pman student=student;
run;

/* LINEARITY: verify plot of (studentised) residuals vs predicted values */
/* HOMOSCADESTICITY: verify (squared) residuals vs predicted values */
/* Model WITHOUT interactions */
data residmain2;
	set residmain;
	rman2=rman**2;
proc sgplot data=residmain2;
	scatter x=pman y=rman;
	loess x=pman y=rman;
	refline 0 / axis=y lineattrs=(color=red);
proc sgplot data=residmain2;
	scatter x=pman y=rman2;
	loess x=pman y=rman2;
	refline 0 / axis=y lineattrs=(color=red);
run;

/* Model WITH interactions */
data residfull2;
	set residfull;
	rman2=rman**2;
proc sgplot data=residfull2;
	scatter x=pman y=rman;
	loess x=pman y=rman;
	refline 0 / axis=y lineattrs=(color=red);
proc sgplot data=residfull2;
	scatter x=pman y=rman2;
	loess x=pman y=rman2;
	refline 0 / axis=y lineattrs=(color=red);
run;

********************************************************************************
/* OUTLIER analysis: Model WITHOUT interactions*/		      	  Leander MEURIS
********************************************************************************
proc reg data=centelection2 plots=(CooksD RStudentByLeverage DFFITS DFBETAS);
	model GOP=White Bachelor LogPop Poverty Travel/ 
		r influence;
	ID FIPS;
	ods output outputstatistics=inflmain;
run;

/* Exclude 84 outliers based on Cook's D: residuals look more homoscedastic */
data obs_filtered_main (keep=FIPS);
	set inflmain;
	where CooksD <=0.002571;
data obs_filtered_main_excluded;
	set inflmain;
	where CooksD >0.002571;
run;

proc sort data=obs_filtered_main;
	by FIPS;
proc sort data=centelection2;
	by FIPS;
data centelection2_filtmain;
	merge obs_filtered_main(in=a) centelection2;
	by FIPS;
	if a;
run;

proc reg data=centelection2_filtmain plots=(CooksD RStudentByLeverage);
	model GOP=White Bachelor LogPop Poverty Travel/ VIF partial influence r;
run;
												/* adjusted R squared = 0.6186*/
********************************************************************************
/* OUTLIER analysis: Model WITH interactions*/						 Hamza Rarou
********************************************************************************
proc reg data=centelection2 plots(label)=(CooksD RStudentByLeverage DFFITS DFBETAS);
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach IntBachPoverty/ 
		r influence;
	ID FIPS;
	ods output outputstatistics=inflfull;
run;

/* Exclude 83 outliers based on Cook's D: residuals look more homoscedastic */
/* DFFITS excludes 150 observation: marginally improves adj Rsquare */
data obs_filtered_full (keep=FIPS);
	set inflfull;
	where CooksD <=0.002571;
data obs_filtered_full_excluded;
	set inflfull;
	where CooksD >0.002571;
run;

proc sort data=obs_filtered_full;
	by FIPS;
proc sort data=centelection2;
	by FIPS;
data centelection2_filtfull;
	merge obs_filtered_full(in=a) centelection2;
	by FIPS;
	if a;
run;

proc reg data=centelection2_filtfull plots=(CooksD RStudentByLeverage);
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach IntBachPoverty/ 
		VIF partial influence r;
run;
												/* adjusted R squared = 0.6349*/
********************************************************************************
/* Check ASSUMPTIONS after removing outliers*/			 Matthias VANDEKERCKHOVE
********************************************************************************
/* NORMALITY: verify qqplot of (studentised) residuals */												
proc reg data=centelection2_filt;
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty 
		IntWhiteTravel IntLogPopTravel IntWhiteLogPop IntLogPopPoverty 
		IntWhiteBach IntBachPoverty/ partial;
	output out=resid_filt r=rman p=pman student=student;
run;

/* LINEARITY: verify plot of (studentised) residuals vs predicted values */
/* HOMOSCADESTICITY: verify (squared) residuals vs predicted values */
data resid_filt;
	set resid_filt;
	rman2=rman**2;
proc sgplot data=resid_filt;
	scatter x=pman y=rman;
	loess x=pman y=rman;
	refline 0 / axis=y lineattrs=(color=red);
proc sgplot data=resid_filt;
	scatter x=pman y=rman2;
	loess x=pman y=rman2;
	refline 0 / axis=y lineattrs=(color=red);
run;

********************************************************************************
/* Model VALIDATION */												 Hamza RAROU
*
********************************************************************************
data election_validation;
	set election_validation;
	wortelGOP=sqrt(GOP);
	logGOP=log(GOP);
	GOP_inv=1/GOP;
run;

/* CENTERING variables to reduce multicollinearity  */
proc stdize data=election_validation method=mean out=centelectionval;
	var White Bachelor LogPop Poverty Travel;
run;

data centelectionval2;
	set centelectionval;
	IntWhitePoverty=White*Poverty;
	IntWhiteTravel=White*Travel;
	IntLogPopTravel=LogPop*Travel;
	IntPovertyTravel=Poverty*Travel;
	IntWhiteLogPop=White*LogPop;
	IntLogPopPoverty=LogPop*Poverty;
	IntWhiteBach=White*Bachelor;
	IntBachPoverty=Bachelor*Poverty;
run;

********************************************************************************
/* FIT model (main variables and interaction terms) to VALIDATION set */
proc reg data=centelectionval2 plots=(CooksD RStudentByLeverage DFFITS DFBETAS);
	model GOP=White Bachelor LogPop Poverty Travel IntWhitePoverty IntWhiteTravel 
		IntLogPopTravel IntWhiteLogPop IntLogPopPoverty IntWhiteBach IntBachPoverty/
		r influence;
	ID FIPS;
	ods output outputstatistics=inflfullval;
run;

/* FIT model (main variables) to VALIDATION set */
proc reg data=centelectionval2 plots=(CooksD RStudentByLeverage DFFITS DFBETAS);
	model GOP=White Bachelor LogPop Poverty Travel/r influence;
	ID FIPS;
	ods output outputstatistics=inflmainval;
run;

********************************************************************************
/* Formal VALIDATION of model with PRess (& Cp) */
proc reg data=centelectionval2;
	model GOP=White Bachelor LogPop Poverty Travel / VIF AIC Cp PRESS r;
	id FIPS;
run;

/* PREDICTIVE CAPABILITY of the model with MSPR */
/* PRess = 16.34 and SSE = 16.18 */
/* This means the model is validated */
/* MSE (0.01) is a good indicator of the predictive capability of the model */

data pred_data;
	set centelectionval2;
	GOPpred=0.64181+White*(0.00405)+Bachelor*(-0.00833)+LogPop*(-0.02137)+Poverty*(-0.00172)+Travel*(0.00144);
data preddiff;
	set pred_data end=last;
	pred_diff=((GOP-GOPpred)**2)/1566;
proc summary data=preddiff;
	var pred_diff;
	output out=totals sum=;
run;

/* MSPR = 0.0106 and should be compared to the MSE = 0.0104 */

proc sort data=obs_filtered_main;
	by FIPS;
proc sort data=centelection2;
	by FIPS;
data centelection2_filt;
	merge obs_filtered_main (in=a) centelection2;
	by FIPS;
	if a;
run;