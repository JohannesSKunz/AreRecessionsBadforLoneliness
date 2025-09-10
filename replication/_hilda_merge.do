*************************************************************************
* Project: Are recessions bad for loneliness?
*************************************************************************

clear all
set  maxvar 14000

cap cd "HILDA/v22/_raw/2. STATA 220u (Zip file 1 of 4 - Combined Data Files a-k) v3/"


* COMBINE WAVES 1 TO 22
loc i = 1
foreach wave in a b c d e f g h i j k l m n o p q r s t u v {
		use "Combined_`wave'220u.dta", clear
		destring, replace
		drop xhhraid 
		renvars, predrop(1)
		rename waveid xwaveid
		sort xwaveid, stable
		g wave = `i'
		compress
		tempfile wave`i'
		save `wave`i'', replace
	loc i = `i'+ 1
	}

* Append 
cap use `wave1' , clear
forval i=2/22 {
	cap append using `wave`i'', force
	}

compress	
save ../../_processed/allwaves22, replace
