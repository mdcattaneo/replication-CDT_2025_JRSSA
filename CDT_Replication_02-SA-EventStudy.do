/******************************************************************************/
/******************************************************************************/
/** Randomization inference for before-and-after studies with multiple units **/ 
/********** An application to a criminal procedure reform in Uruguay **********/
/******************************************************************************/
/******************************************************************************/

/* Matías Cattaneo, Carlos Díaz, and Rocío Titiunik */

** ssc install xtevent

clear all
set more off
xtset,clear

/******************************************************************************/
/*********************************** Event Study analysis **********************************/ 
/******************************************************************************/
use Data_daily_barrio.dta, clear 
xtset,clear 
gen score0 = score
replace score0 = score -1 if score>=0
sum score0
local k = abs(r(min))
display `k'
replace score0 = score0 + `k' + 1

* Using 30 days total, no fixed effects, no time effects
xtevent crime if abs(score) <= 15,  policyvar(new_code) panelvar(barrio) timevar(score0)  window(13)diffavg nofe note 
xteventplot, ytitle("Coefficient") xtitle("Event time") scatterplotopts(yscale(range(-4 4)))
graph export "./tables-and-plots/CDT_plot_ES_w14_abs15_notenofe.pdf", as(pdf) replace
xteventtest, coefs(0 7 14)


* Using 30 days total, with fixed effects and time effects
xtevent crime if abs(score) <= 15,  policyvar(new_code) panelvar(barrio) timevar(score0)  window(13)diffavg
xteventplot, ytitle("Coefficient") xtitle("Event time") scatterplotopts(yscale(range(-4 4)))
graph export "./tables-and-plots/CDT_plot_ES_w14_abs15.pdf", as(pdf) replace
xteventtest, coefs(0 7 14)

* Using 100 days total, with fixed effects and time effects
xtevent crime if abs(score) <= 50,  policyvar(new_code) panelvar(barrio) timevar(score0)  window(13)diffavg
xteventplot, ytitle("Coefficient") xtitle("Event time") scatterplotopts(yscale(range(-4 4)))
graph export "./tables-and-plots/CDT_plot_ES_w14_abs50.pdf", as(pdf) replace
xteventtest, coefs(0 7 14)


* Using 200 days total, with fixed effects and time effects
xtevent crime if abs(score) <= 100,  policyvar(new_code) panelvar(barrio) timevar(score0)  window(13)diffavg
xteventplot, ytitle("Coefficient") xtitle("Event time") scatterplotopts(yscale(range(-4 4)))
graph export "./tables-and-plots/CDT_plot_ES_w14_abs100.pdf", as(pdf) replace
xteventtest, coefs(0 7 14)

