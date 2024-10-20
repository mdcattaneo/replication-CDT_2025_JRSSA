/******************************************************************************/
/******************************************************************************/
/** Randomization inference for before-and-after studies with multiple units **/ 
/********** An application to a criminal procedure reform in Uruguay **********/
/******************************************************************************/
/******************************************************************************/

/* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */

/* Replication Code */

clear all
set more off


/******************************************************************************/
/******************* ANALYSIS TO PRODUCE FIGURE 2 ********************/

/******************************************************************************
WARNING: THIS CODE CALCULATES MANY RANDOMIZATION-BASED P-VALUES USING SIMULATIONS AND TAKES LONG TO RUN
TO REDUCE THE AMOUNT OF TIME, REDUCE REPS PARAMETER BELOW FROM 10,000 TO SMALLER VALUE
HOWEVER, FOR REPRODUCIBILITY WE OF VALUES IN THE PAPER, WE RECOMMEND SETTING THIS PARAMETER TO 10,000
******************************************************************************/

global REPS = 10000

/* Event Time: 12 a.m. on November 1st, 2017 */

use Data_panel_before_and_after.dta, clear
	
forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after", replace

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Event Time: 12 a.m. on November 1st, 2015 */
 
use  Data_panel_before_and_after_2015.dta, clear

forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after_2015", replace

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Event Time: 12 a.m. on November 1st, 2016 */
 
use Data_panel_before_and_after_2016.dta, clear
	
forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after_2016", replace

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Event Time: 12 a.m. on November 1st, 2018 */
 
use Data_panel_before_and_after_2018.dta, clear
	
forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after_2018", replace

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Event Time: 12 a.m. on the First Wednesday of November, 2015 */
 
use Data_panel_before_and_after_2015_w.dta, clear
	
forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after_2015_w", replace

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Event Time: 12 a.m. on the First Wednesday of November, 2016 */
 
use Data_panel_before_and_after_2016_w.dta, clear
	
forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after_2016_w", replace

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Event Time: 12 a.m. on the First Wednesday of November, 2018 */
 
use Data_panel_before_and_after_2018_w.dta, clear
	
forvalues i=1(1)20{
	local crimes "crime"
	foreach x of local crimes{
		permute treated diffmean=(r(mu_2)-r(mu_1)), reps($REPS) nowarn strata(barrio): ttest `x'_`i', by(treated)
		matrix delta = r(b) 
		display "delta = " delta[1,1]
		gen delta_`x'_`i' = delta[1,1]
		matrix pval = r(p_twosided) 
		display "p-val = " pval[1,1]
		gen pval_`x'_`i' = pval[1,1]
	}
}

local crimes "crime"
foreach x of local crimes{
	gen pval_`x'=.
	forvalues i=1(1)20{
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	forvalues i=1(1)20{
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

export delimited using "fisherian_before_and_after_2018_w", replace

/******************************************************************************/
/******************************************************************************/
