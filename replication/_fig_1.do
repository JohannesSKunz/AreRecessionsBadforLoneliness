***********************************************************************
* Project 	: Loneliness
* Figure 1	: Loneliness and unemployment rate over time
***********************************************************************

clear all

set  maxvar 14000

use "main_loneliness.dta", clear

drop if zzw11103<0

collapse (mean) vlonely lsupport socialisolation unemployment_sa4 , by(year)
	
for var vlonely lsupport socialisolation: replace X = X*100
	
// Loneliness	
tw (connected vlonely year, ytitle("% of people with loneliness")) ///
   (connected unemployment_sa4 year, yaxis(2) lpattern(shortdash) msymbol(d) ytitle("Unemployment rate (%)", axis(2)))	///
	, scheme(white_tableau) graphregion(color(white)) ///
	xtitle("Year") xlabel(2001(2)2019) ///
	legend(label(1 "Loneliness") label(2 "Unemployment rate") pos(6) row(1))
	   
cap graph export "figure1.jpg, replace quality(100)"
  