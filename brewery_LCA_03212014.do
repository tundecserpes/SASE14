//------------------------------------------------------------------------------------------
//	Brewery project
//  Latent class analysis, LCA project
//  03.03.2014
//----------------------------------------------------------------------------------------

set more off
capture drop _all
capture log close
clear all
numlabel _all, add

clear all 
use "C:\Users\Ann Tunde\Documents\My Box Files\Breweries\data\LatentVar_project14\data\spatialbrewery_IRT_03032014.dta"
cd "C:\Users\Ann Tunde\Documents\My Box Files\UIC\archive_classes\3rdyear\SOC509_Latent\Week9-LatentCategories\stata\Week9"

*create new variable for License to self-distribute (1= yes, 0= no)
*url: http://www.brewersassociation.org/pages/government-affairs/self-distribution-laws
*data coded: 03.21.2014
generate self_distrD = .
replace self_distrD =0 if state == "AL"
replace self_distrD =1 if state == "MN"
replace self_distrD =1 if state == "CT"
replace self_distrD =0 if state == "NV"
replace self_distrD =1 if state == "TN"
replace self_distrD =1 if state == "OK"
replace self_distrD =0 if state == "FL"
replace self_distrD =0 if state == "KY"
replace self_distrD =1 if state == "ID"
replace self_distrD =1 if state == "AZ"
replace self_distrD =1 if state == "IL"
replace self_distrD =1 if state == "VA"
replace self_distrD =1 if state == "NH"
replace self_distrD =1 if state == "AR"
replace self_distrD =1 if state == "NC"
replace self_distrD =1 if state == "WY"
replace self_distrD =0 if state == "SD"
replace self_distrD =1 if state == "UT"
replace self_distrD =1 if state == "IN"
replace self_distrD =0 if state == "SC"
replace self_distrD =1 if state == "MD"
replace self_distrD =1 if state == "MA"
replace self_distrD =0 if state == "LA"
replace self_distrD =1 if state == "WA"
replace self_distrD =0 if state == "MI"
replace self_distrD =1 if state == "WV"
replace self_distrD =1 if state == "CA"
replace self_distrD =1 if state == "NM"
replace self_distrD =0 if state == "GA"
replace self_distrD =0 if state == "DE"
replace self_distrD =0 if state == "MO"
replace self_distrD =1 if state == "OR"
replace self_distrD =1 if state == "TX"
replace self_distrD =1 if state == "NY"
replace self_distrD =1 if state == "IA"
replace self_distrD =0 if state == "NE"
replace self_distrD =0 if state == "KS"
replace self_distrD =1 if state == "MT"
replace self_distrD =1 if state == "RI"
replace self_distrD =1 if state == "CO"
replace self_distrD =1 if state == "NJ"
replace self_distrD =1 if state == "ME"
replace self_distrD =0 if state == "MS"
replace self_distrD =1 if state == "PA"
replace self_distrD =1 if state == "WI"
replace self_distrD =1 if state == "OH"
replace self_distrD =1 if state == "VT"
replace self_distrD =1 if state == "ND"
replace self_distrD =0 if state == "DC"

*create dummies (0/1)
rename BAmember BAmemberD
rename SGMember SGmemberD

tab DIFF_DIST 
generate shomoD = .
replace shomoD = 1 if DIFF_DIST > 0
replace shomoD = 0 if DIFF_DIST <= 0
tab shomoD, m
*86%
tab f1
generate f1posD = .
replace f1posD = 0 if f1 <= 0
replace f1posD = 1 if f1 > 0
tab f1posD
*75%
tab f2
generate f2posD = .
replace f2posD = 0 if f2<=0
replace f2posD = 1 if f2 >0
tab f2posD

*dummies: self_distrD BAmemberD SGmemberD shomoD f1posD f2posD


*create dummies (1/2)

foreach var in 	self_distrD BAmemberD SGmemberD shomoD f1posD f2posD {
			
			clonevar `var'2=`var'
			replace `var'2=2 if `var'==0 
			tab `var' `var'2, miss
	}	

	egen NumMiss=rowmiss(self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2)
	drop if NumMiss>0
*2 observations deleted only because I had empty cells for two states from which I don't have any breweries

*create category dummies
generate fast50 = .
replace fast50 = 0 if category == "newbrew" | category == "top50"
replace fast50 = 1 if category == "fast50"
generate newbrew =.
replace newbrew = 0 if category == "fast50" | category == "top50"
replace newbrew = 1 if category == "newbrew"
generate top50 =.
replace top50 = 0 if category == "newbrew" | category == "fast50"
replace top50 = 1 if category == "top50"


//-----------------------------------------------
//  Examine patterns
//-----------------------------------------------
	
	egen Pattern=concat(self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2)
	list Pattern self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2 in 1/30
	tab Pattern
	
	qbys Pattern: gen NumPattern=_N
	
	tab Pattern if NumPattern>=30	

//-----------------------------------------------
//  Run LCA - Examine Fit
//-----------------------------------------------

//	Basic LCA model example

	doLCA self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2, ///
		nclass(1) ///
		seed(100000) ///
		categories(2 2 2 2 2 2)  ///
		criterion(0.000001)  ///
		rhoprior(1.0)
	doLCA self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2, ///
		nclass(2) ///
		seed(100000) ///
		categories(2 2 2 2 2 2)  ///
		criterion(0.000001)  ///
		rhoprior(1.0)
	doLCA self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2, ///
		nclass(3) ///
		seed(100000) ///
		categories(2 2 2 2 2 2)  ///
		criterion(0.000001)  ///
		rhoprior(1.0)
	doLCA self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2, ///
		id(stateid) ///
	  	nclass(4) ///
		seed(100000) ///
		categories(2 2 2 2 2 2)  ///
		criterion(0.000001)  ///
		rhoprior(1.0)  

//final model
		doLCA self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2, ///
		nclass(3) ///
		seed(100000) ///
		categories(2 2 2 2 2 2)  ///
		criterion(0.000001)  ///
		rhoprior(1.0)

		
	//	Gammas

		matrix list r(gamma)

	//	Gamma Standard Errors

		matrix list r(gammaSTD)

	//	Rhos

		matrix list r(rho)

	//	Rho Standard Errors

		matrix list r(rhoSTD)

	//	Posterior Probabilities
	
		matrix list r(post_prob)

	//	Save and check Posterior Probabilities
	
		matrix postprob=r(post_prob)
		svmat postprob
		list stateid postprob* in 1/30, sep(0)

	//	Classify based on highest Posterior Probability
	
		gen Class=1 	if postprob1>0.50
		replace Class=2	if postprob2>0.50
		replace Class=3	if postprob3>0.50
		*replace Class=4	if postprob4>0.50
		
		gen Class90=1 	if postprob1>0.90
		replace Class90=2	if postprob2>0.90
		replace Class90=3	if postprob3>0.90
		
		tab Class
		tab Class90
		table Class, c(mean newbrew mean fast50 mean top50)
		 
		tab Class newbrew, chi2 
		tab Class fast50, chi2  
		tab Class top50, chi2 
		
	// 	Show Entropy Calculation
	
		gen EntropyPiece1=postprob1*ln(postprob1)+postprob2*ln(postprob2)+postprob3*ln(postprob3) ///+postprob4*ln(postprob4)
		
		gen EntropyPiece2=-1*(sum(EntropyPiece1))
		sum EntropyPiece2
		di 1- ((208.8331)/(512*ln(4)))

		doLCA 	self_distrD2 BAmemberD2 SGmemberD2 shomoD2 f1posD2 f2posD2, ///
		nclass(5) ///
		seed(100000) ///
		categories(2 2 2 2 2 2)  ///
		criterion(0.000001)  ///
		rhoprior(1.0)
		
	