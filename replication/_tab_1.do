*************************************************************************
* Project 	: Loneliness
* Table 1	: Local unemployment rate and loneliness – Regional analysis
*************************************************************************

clear all
set  maxvar 14000

* ------------------------------------------------------------
* Start 

use "main_loneliness.dta", clear

* Convert unemployment scale
replace unemployment_sa4 = unemployment_sa4/100

local depvars "vlonely lsupport socialisolation club_mem vlonely_chronic"
local indvars "unemployment_sa4 job_loss_percep unemployed"

local first_loop yes

foreach yvar of varlist unemployed job_loss_percep {
  quietly reghdfe `yvar' unemployment_sa4, absorb(year age pid) keepsingletons cluster(hhssa3)
  quietly summ `yvar' if e(sample)
	local mean_yvar = r(mean) 
	local sd_yvar = r(sd) 	
  quietly summ unemployment_sa4 if e(sample)  
	local mean_unemp = r(mean)  
	local sd_unemp = r(sd)    
  if "`first_loop'"=="yes"{
    outreg2 using "tab1.xls", replace ctitle(`yvar') addstat(Mean ,`mean_yvar', SD, `sd_yvar', Unemployment Mean, `mean_unemp',Unemployment Sd,`sd_unemp') label nocons bdec(3)
    local first_loop no
  }
  else {
    outreg2 using "table1.xls", append ctitle(`yvar') addstat(Mean ,`mean_yvar', SD, `sd_yvar', Unemployment Mean, `mean_unemp',Unemployment Sd,`sd_unemp') label nocons bdec(3)
  }
}


