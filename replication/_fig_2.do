***********************************************************************
* Project 	: Loneliness
* Figure 2	: Unemployment and loneliness – Event study analysis
***********************************************************************

clear all
set  maxvar 14000

use "main_loneliness.dta", clear

by pid, sort: egen firsttime = min(cond(unemployed == 1, wave, .))
gen t =  wave - firsttime
replace t = 0 if t == .  
replace t = -3 if t<=-3
replace t = 4 if t>=4
tab t, gen(dt)

drop dt3

reghdfe vlonely i.(dt1-dt8) ib(2002).year  if inrange(t,-3,4) & year>=2002 & unemployment_sa4_l1 !=., absorb(pid) cluster(hhssa4)	
	nlcom 	 ///
			(Tn3: _b[1.dt1]) ///
			(Tn2: _b[1.dt2]) ///
			(Omit: 0) ///
			(T0: _b[1.dt4]) ///
			(T1: _b[1.dt5]) ///
			(T2: _b[1.dt6]) ///
			(T3: _b[1.dt7]) ///
			(T4: _b[1.dt8]) ///
			, post
	est store reg0
	
coefplot reg0, ///
	vertical ciopts(lcol(navy navy)) levels(95 90) ///
	scheme(white_tableau) ///
	xtitle("Years from first observed unemploymemt") ///
	ytitle("Coefficient estimates and 95% (90%) confidence interval" "on time from unemployment on extreme loneliness") ///
	yline(0, lpattern(dash) lcolor(black)) ///
	legend(off) ylabel(-0.06(0.02)0.06) ///
	baselevels omitted xlabel(1 "{it:{&le} -3}" 2 "{it:-2}" 3 "{it:-1}" 4 "{it:0}" 5 "{it:1}" 6 "{it:2}" 7 "{it:3}" 8 "{it:{&ge} 4}" )

cap graph export "figure2.jpg", replace quality(100)
	