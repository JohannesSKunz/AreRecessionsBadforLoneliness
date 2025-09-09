*************************************************************************
* Project 	: Loneliness
* Table 2	: Unemployment, job loss perception, and social health
***********************************************************************
clear all
set  maxvar 14000

* ------------------------------------------------------------
* Start 
use "main_loneliness.dta", clear

* --------------------

local depvars "vlonely lsupport socialisolation"
local indvars "unemployed job_loss_percep"

local first_loop yes

foreach yvar of varlist vlonely lsupport socialisolation {
  foreach xvar of varlist unemployed job_loss_percep {
	quietly reghdfe `yvar' `xvar', absorb(year age pid) keepsingletons cluster(hhssa4)
	quietly summ `yvar' if e(sample)
		local mean_yvar = r(mean) 
		local sd_yvar = r(sd) 	
	quietly summ `xvar' if e(sample)  
		local mean_xvar = r(mean)  
		local sd_xvar = r(sd)    
	if "`first_loop'"=="yes"{
		outreg2 using "table2.xls", replace ctitle(`yvar') addstat(Mean dep,`mean_yvar', SD dep, `sd_yvar', Mean indep, `mean_xvar',Sd indep,`sd_xvar') label nocons bdec(3)
    local first_loop no
	}
	else {
    outreg2 using "table2.xls", append ctitle(`yvar') addstat(Mean dep ,`mean_yvar', SD dep, `sd_yvar', Mean indep, `mean_xvar',Sd indep,`sd_xvar') label nocons bdec(3)
	}
  }
}
