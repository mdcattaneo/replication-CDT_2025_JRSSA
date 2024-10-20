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
 
global REPS = 10000

/******************************************************************************/
/*********************************** TABLE 1 **********************************/ 
/******************************************************************************/
use Data_daily.dta, clear

local results "diff=(r(mu_2)-r(mu_1)) r(mu_1) r(mu_2) se_1=r(sd_1)/(r(N_1)^(1/2)) se_2=r(sd_2)/(r(N_2)^(1/2)) r(p) r(se) av=(r(mu_1)+r(mu_2))/2 share=[(r(mu_1)+r(mu_2))/2]/330.63*100"

#delimit ;
   qui table 
        (command) (result), 
        command(`results' : ttest theft, by(new_code))
        command(`results' : ttest robbery, by(new_code))
        command(`results' : ttest dv, by(new_code))
        command(`results' : ttest other, by(new_code))
		command(`results' : ttest crime, by(new_code))
    ;

    collect label levels command 
        1 "Theft" 
        2 "Robbery" 
        3 "Domestic Violence"
		4 "Other Crimes"
        5 "Total Police Reports" , modify
        ;
   
    collect addtags myrows[first], fortags(result[av share mu_1 mu_2 diff p]) ;
    collect addtags myrows[second], fortags(result[se_1 se_2 se]) ;
    collect addtags mycols[1], fortags(result[av]) ;
	collect addtags mycols[2], fortags(result[share]) ;
	collect addtags mycols[3], fortags(result[mu_1 se_1]) ;
    collect addtags mycols[4], fortags(result[mu_2 se_2]) ;
    collect addtags mycols[5], fortags(result[diff se]) ;
    collect addtags mycols[6], fortags(result[p]) ;

    collect label levels mycols 
        1 "11/01/16 to 10/31/18"
		2 "Share, %"
		3 "(a) Old CPP" 
        4 "(b) New CPP" 
        5 "Difference (b-a)"
		6 "P-Value"

        , modify
        ;
#delimit cr

qui collect layout (command#myrows) (mycols)
collect style cell myrows, nformat(%6.3f) halign(center)
collect style cell myrows[second], sformat((%s))
collect style header mycols, title(hide) 
collect style header myrows, title(hide) level(hide)
collect title "Table 1: Crime in Montevideo"

log using CDT_Replication_Log, replace
noi collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
 
/******************************************************************************/
/*********************************** TABLE 2 **********************************/ 
/******************************************************************************/

use Data_panel_before_and_after.dta, clear 

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 2: Effects of CCP Reform for Different Windows Around Event Time"

log on
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
 
/******************************************************************************/
/*********************************** TABLE 3 **********************************/ 
/******************************************************************************/

/* See R script CDT_Replication_01-Analysis-Tables1to6.R */

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
 
/******************************************************************************/
/*********************************** TABLE 4 **********************************/ 
/******************************************************************************/

/* Year 2015 */

use Data_panel_before_and_after_2015.dta, clear

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 4, 2015: Effects of CCP Reform for Different Windows Around Placebo Event Times"

log on
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Year 2016 */

use Data_panel_before_and_after_2016.dta, clear

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 4, 2016: Effects of CCP Reform for Different Windows Around Placebo Event Times"

log on 
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Year 2018 */


use Data_panel_before_and_after_2018.dta, clear

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 4, 2018: Effects of CCP Reform for Different Windows Around Placebo Event Times"

log on
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
 
/******************************************************************************/
/*********************************** TABLE 5 **********************************/ 
/******************************************************************************/

/* Year 2015 */

use Data_panel_before_and_after_2015_w.dta, clear

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 5, 2015: Effects of CCP Reform for Different Windows Around Placebo Event Times"

log on 
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Year 2016 */

use Data_panel_before_and_after_2016_w.dta, clear

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 5, 2016: Effects of CCP Reform for Different Windows Around Placebo Event Times"

log on
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Year 2018 */

use Data_panel_before_and_after_2018_w.dta, clear

foreach i in 1 7 14 {
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
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_crime delta_crime 
drop if pval_crime==.
rename barrio window
replace delta_crime=delta_crime/window

/******************************************************************************/

label var window "Tau"
label var delta_crime "Theta_Tau"
label var pval_crime "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_crime pval_crime) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 5, 2018: Effects of CCP Reform for Different Windows Around Placebo Event Times"

log on 
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
 
/******************************************************************************/
/*********************************** TABLE 6 **********************************/ 
/******************************************************************************/

/* Theft */

use Data_panel_before_and_after_theft.dta, clear 

foreach i in 1 7 14 {
	local crimes "theft"
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

local crimes "theft"
foreach x of local crimes{
	gen pval_`x'=.
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_theft delta_theft
drop if pval_theft==.
rename barrio window
replace delta_theft=delta_theft/window

/******************************************************************************/

label var window "Tau"
label var delta_theft "Theta_Tau"
label var pval_theft "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_theft pval_theft) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 6: Effects of CCP Reform for Theft"

log on 
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Domestic Violence */

use Data_panel_before_and_after_dv.dta, clear 

foreach i in 1 7 14 {
	local crimes "dv"
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

local crimes "dv"
foreach x of local crimes{
	gen pval_`x'=.
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_dv delta_dv
drop if pval_dv==.
rename barrio window
replace delta_dv=delta_dv/window

/******************************************************************************/

label var window "Tau"
label var delta_dv "Theta_Tau"
label var pval_dv "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_dv pval_dv) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 6: Effects of CCP Reform for Domestic Violence"

log on 
collect preview
log off 

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* Robbery */


use Data_panel_before_and_after_robbery.dta, clear 
foreach i in 1 7 14 {
	local crimes "robbery"
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

local crimes "robbery"
foreach x of local crimes{
	gen pval_`x'=.
	foreach i in 1 7 14 {
		replace pval_`x'=pval_`x'_`i' if barrio==`i' & treated==1
	}
	gen delta_`x'=.
	foreach i in 1 7 14 {
		replace delta_`x'=delta_`x'_`i' if barrio==`i' & treated==1
	}
}

/******************************************************************************/

keep barrio pval_robbery delta_robbery
drop if pval_robbery==.
rename barrio window
replace delta_robbery=delta_robbery/window


/******************************************************************************/

label var window "Tau"
label var delta_robbery "Theta_Tau"
label var pval_robbery "P-Value"
collect clear
qui table (window) (var), statistic(firstnm delta_robbery pval_robbery) nototals
qui collect layout (window) (var)
collect style cell, nformat(%6.3f) halign(center)
collect title "Table 6: Effects of CCP Reform for Robbery"

log on 
collect preview
log off

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

log close 

/******************************************************************************/
/******************************************************************************/
