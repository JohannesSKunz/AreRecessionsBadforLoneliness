***********************************************************************
* Project 	: Loneliness
* Figure 3	: Heterogeneous relationship of unemployment and loneliness – Job characteristics
***********************************************************************

clear all
set  maxvar 14000

// Unemployed
use "main_loneliness.dta", clear
 
loc i = 1
loc j = 1
  
local indicator unemployed
local variables1 "fulltime_l1 public_sector_l1 permanent_l1 highskill_l1 dual_earner_l1" 

su `indicator'
sca stad = r(sd) 

loc count: word count `variables1'
        matrix A = J(5,`count',.)
        matrix rownames A = coef ll95 ul95 ll90 ul90
        matrix colnames A = `vars'
        matrix B = J(5,`count',.)
        matrix rownames B = coef ll95 ul95 ll90 ul90
        matrix colnames B = `vars'
        matrix C = J(5,`count',.)
		
* For dummies		
foreach var of local variables1  {
	g med = `var' == 1 if !missing(`var')
	g d_`indicator' = `indicator'*med
	g d1_`indicator' = `indicator'*(1-med)
	reghdfe vlonely d_`indicator' d1_`indicator', absorb(year age pid) keepsingletons cluster(hhssa4)
	est sto reg1_`i' 
		qui su `e(depvar)' if e(sample) == 1 & med == 1 
		estadd sca me1 = r(mean) , :reg1_`i'
		estadd sca sd = r(sd)   , :reg1_`i'
		estadd sca mf1 = (_b[d_`indicator']*stad)/r(mean)   , :reg1_`i'	

			sca lb=  _b[d_`indicator']-1.96*_se[d_`indicator']
			sca lb1= _b[d_`indicator']-1.645*_se[d_`indicator']    
			sca ub=  _b[d_`indicator']+1.96*_se[d_`indicator']
			sca ub1= _b[d_`indicator']+1.645*_se[d_`indicator']
            matrix A[1,`i'] = _b[d_`indicator'] \ lb \ ub \ lb1 \ ub1		
		
		qui su `e(depvar)' if e(sample) == 1 & med == 0 
		estadd sca me2 = r(mean) , :reg1_`i'
		estadd sca mf2 = (_b[d1_`indicator']*stad)/r(mean)   , :reg1_`i'	

			sca lb=  _b[d1_`indicator']-1.96*_se[d1_`indicator']
			sca lb1= _b[d1_`indicator']-1.645*_se[d1_`indicator']    
			sca ub=  _b[d1_`indicator']+1.96*_se[d1_`indicator']
			sca ub1= _b[d1_`indicator']+1.645*_se[d1_`indicator']
            matrix B[1,`i'] = _b[d1_`indicator'] \ lb \ ub \ lb1 \ ub1			
		
		test d_`indicator' = d1_`indicator'
		local pF`i' : di %3.2fc r(p) 
		estadd sca pF = r(p)   , :reg1_`i'

		loc i = `i' + 1
		drop med d_`indicator'  d1_`indicator'
		loc title " `title' `"`var'"'"
		}		

* Make figure		
        coefplot (matrix(A), msymbol(O)) (matrix(B), msymbol(D)), ///
			ylab(, labs(small)) xlab(, labs(small))  /// 
			xline(0, lc(black) lp(dash)) ///
			ciopts(lwidth(.35 .35) lcolor(*.4 *1.2)) ci((2 3) (4 5) ) ///
			fxsize(125) fysize(100) ///
			scheme(white_tableau) graphregion(color(white) margin(0 0 0 0)) bgcolor(white) ///
			order(c1 c2 c3 c4 c5 c6 c7 c13 c8 c9 c10 c11 c12) ///
			legend(order(3 "Yes" 6 "No") row(1) pos(6) size(small) region(lcolor(white))) ///
				coeflab( ///
				c1		=       `"Full-time worker (p-val=`pF1')"' ///
				c2		= 	    `"Public sector worker (p-val=`pF2')"' ///
				c3		= 	    `"Permanent contract worker (p-val=`pF3')"' ///
				c4      =		`"High-skill worker (p-val=`pF4')"' ///
				c5      =		`"Dual-earner (p-val=`pF5')"' ///
				) name(gr1, replace) title(Unemployed)	xlabel(-0.04(0.02)0.1, labsize(vsmall))

// Job loss perception
use "main_loneliness.dta", clear
  
loc i = 1
loc j = 1
  
local indicator job_loss_percep
local variables1 "fulltime_l1 public_sector_l1 permanent_l1 highskill_l1 dual_earner_l1" 

su `indicator'
sca stad = r(sd) 

loc count: word count `variables1'
        matrix A = J(5,`count',.)
        matrix rownames A = coef ll95 ul95 ll90 ul90
        matrix colnames A = `vars'
        matrix B = J(5,`count',.)
        matrix rownames B = coef ll95 ul95 ll90 ul90
        matrix colnames B = `vars'
        matrix C = J(5,`count',.)
		
* For dummies		
foreach var of local variables1  {
	g med = `var' == 1 if !missing(`var')
	g d_`indicator' = `indicator'*med
	g d1_`indicator' = `indicator'*(1-med)
	reghdfe vlonely d_`indicator' d1_`indicator', absorb(year age pid) keepsingletons cluster(hhssa4)
	est sto reg1_`i' 
		qui su `e(depvar)' if e(sample) == 1 & med == 1 
		estadd sca me1 = r(mean) , :reg1_`i'
		estadd sca sd = r(sd)   , :reg1_`i'
		estadd sca mf1 = (_b[d_`indicator']*stad)/r(mean)   , :reg1_`i'	

			sca lb=  _b[d_`indicator']-1.96*_se[d_`indicator']
			sca lb1= _b[d_`indicator']-1.645*_se[d_`indicator']    
			sca ub=  _b[d_`indicator']+1.96*_se[d_`indicator']
			sca ub1= _b[d_`indicator']+1.645*_se[d_`indicator']
            matrix A[1,`i'] = _b[d_`indicator'] \ lb \ ub \ lb1 \ ub1		
		
		qui su `e(depvar)' if e(sample) == 1 & med == 0 
		estadd sca me2 = r(mean) , :reg1_`i'
		estadd sca mf2 = (_b[d1_`indicator']*stad)/r(mean)   , :reg1_`i'	

			sca lb=  _b[d1_`indicator']-1.96*_se[d1_`indicator']
			sca lb1= _b[d1_`indicator']-1.645*_se[d1_`indicator']    
			sca ub=  _b[d1_`indicator']+1.96*_se[d1_`indicator']
			sca ub1= _b[d1_`indicator']+1.645*_se[d1_`indicator']
            matrix B[1,`i'] = _b[d1_`indicator'] \ lb \ ub \ lb1 \ ub1			
		
		test d_`indicator' = d1_`indicator'
		local pF`i' : di %3.2fc r(p) 
		estadd sca pF = r(p)   , :reg1_`i'

		loc i = `i' + 1
		drop med d_`indicator'  d1_`indicator'
		loc title " `title' `"`var'"'"
		}		

* Make figure		
        coefplot (matrix(A), msymbol(O)) (matrix(B), msymbol(D)), ///
			ylab(, labs(small)) xlab(, labs(small))  /// 
			xline(0, lc(black) lp(dash)) ///
			ciopts(lwidth(.35 .35) lcolor(*.4 *1.2)) ci((2 3) (4 5) ) ///
			fxsize(125) fysize(100) ///
			scheme(white_tableau) graphregion(color(white) margin(0 0 0 0)) bgcolor(white) ///
			order(c1 c2 c3 c4 c5 c6 c7 c13 c8 c9 c10 c11 c12) ///
			legend(order(3 "Yes" 6 "No") row(1) pos(6) size(small) region(lcolor(white))) ///
				coeflab( ///
				c1		=       `"Full-time worker (p-val=`pF1')"' ///
				c2		= 	    `"Public sector worker (p-val=`pF2')"' ///
				c3		= 	    `"Permanent contract worker (p-val=`pF3')"' ///
				c4      =		`"High-skill worker (p-val=`pF4')"' ///
				c5      =		`"Dual-earner (p-val=`pF5')"' ///
				) name(gr2, replace) title(Job loss perception)	xlabel(-0.04(0.02)0.1, labsize(vsmall))		
				
grc1leg  gr1 gr2 , ysize(15) xsize(18) legendfrom(gr1) scheme(white_tableau) graphregion(color(white))
				
cap graph export "figure3.jpg", replace quality(100)

