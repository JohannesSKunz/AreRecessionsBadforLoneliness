*************************************************************************
* Project: Are recessions bad for loneliness?
*************************************************************************

clear all
set  maxvar 14000

use allwaves22.dta

* ---------------------------------------------------------------
* Data preparation

tostring xwaveid, replace 
replace xwaveid="0"+xwaveid if length(xwaveid)==6
g year=wave+2000

order xwaveid year 

compress 

destring xwaveid, gen(pid)
egen id=group(xwaveid)
xtset id year
lab var id "Personal identifier"

cap drop hhid
egen hhid=group(hhrhid)
lab var hhid "Household identifier"

lab var year "Calendar year"

// Month of survey: hhhqivw
gen month = substr(hhhqivw,4,2)		
destring month, replace
tab month
replace month = . if month<0
lab var month "Survey month"

* Generate SCQ non-response variable
gen no_scq = 0
	replace no_scq=1 if ghmh == -8
	replace no_scq=. if ghmh == -10


*--------------------------------------------------
keep xwaveid pid wave year hgyob hhrhid hhpxid hhhqivw hhtup hgsex* edhigh1 lssup* lsclub hifditp ///
hhda10 hhad10 hhec10 hhed10 hhsgcc hhssos hhsra hhsad10 hhsec10 ///
hhsed10 hgage* hgagef lssocal hegpn hehan ghmh ghpf lnwtrp hhwte hhssa1 hhssa2 hhssa3 hhssa4 ///
hhsgcc hhpcode helth heany heart heast hecan hecbe hedi1 hedi2 hedep heomi hehcd hehbp heoc ///
herf2 hedk2 hhura month mrcurr hhsra ancob jbmploj jspsuit jbmplej jbmpgj esempst esbrd ///
jbmhruc jbmcnt wsce wscei jbhruc hgyob lertr hecpmhp slsttm aptmet helstyr slsrate ///
jbmmply jomcsb jomsf jomwf jsactfr lsnwkit lsnwtca mdhagt hifdip hifdin lefrd ///
lsnwexf lsnwcon lspact lsnwcht lsnwvol lsnwtca lsnwser lsnwkit ///
leins leinf ledsc ledrl ledfr levio lepcm ledhm lesep firisk firiska ///
lsselc lssesp lsseci lssefh lssepa lssefd lssecd ///
pnextrv pnagree pnconsc pnemote pnopene no_scq

ds esempst, not
mvdecode `r(varlist)', mv(-10/-1)

***** Outcomes 
* Dimensions of loneliness

g visitmeoften = lssuppv
	label variable visitmeoften "People don't come to visit me as often as I would like (lssuppv)" 
g helpfromothers = lssupnh
	label variable helpfromothers "I often need help from other people but can't get it (lssupnh)" 
g lotsoffriends = lssuplf
	label variable lotsoffriends "I seem to have a lot of friends(lssuplf)" 
g confidein = lssupac
	label variable confidein "I don't have anyone that I can confide in (lssupac)" 
g leanon = lssuplt
	label variable leanon "I have no one to lean on in times of trouble (lssuplt)" 
g cheermeup = lssupcd
	label variable cheermeup "There is someone who can always cheer me up when I'm down (lssupcd)" 
g loneliness = lssupvl
	label variable loneliness "I often feel very lonely (lssupvl)" 
g helpmeout = lssupsh
	label variable helpmeout "When I need someone to help me out, I can usually find someone (lssupsh)" 

* Reverse scale for 'lotsoffriend', 'cheermeup' and 'helpmeout' to be increasing in worse outcomes
g lotsoffriendsreversed = .
	replace lotsoffriendsreversed = 8-lotsoffriends
	tab1 lotsoffriends lotsoffriendsreversed, miss
	label variable lotsoffriendsreversed "I seem to have a lot of friends (Reversed) (lssupsh)" 

g cheermeupreversed = .
	replace cheermeupreversed = 8-cheermeup
	tab1 cheermeup cheermeupreversed, miss
	label variable cheermeupreversed "There is someone who can always cheer me up when I'm down (Reversed) (lssupsh)" 

g helpmeoutreversed = .
	replace helpmeoutreversed = 8-helpmeout
	tab1 helpmeout helpmeoutreversed, miss
	label variable helpmeoutreversed "When I need someone to help me out, I can usually find someone (Reversed) (lssupsh)" 

	
g loneliness_summed= (lssuppv -4) + ///
					 (lssupnh -4) + ///
					 ((8-lssuplf) -4) + ///
					 (lssupac -4) + ///
					 (lssuplt -4) + ///
					 ((8-lssupcd) -4) + ///
					 (lssupvl -4) + ///
					 (lssuppi -4) + ///
					 (lssuptp -4) + ///
					 ((8-lssupsh) -4)
	label variable loneliness_summed "Sum of 10 loneliness questions, (reversed and centered)" 
					 
drop lssu*	
	

*----------- 4 outcomes: loneliness, lack of social support, social isolation, active club/association member
* Loneliness
g vlonely=loneliness>=5
replace vlonely=. if loneliness==.
	lab var vlonely "Loneliness indicator >=5"

gen vlonely_missing = vlonely==.	

* Extreme loneliness
g elonely=loneliness>=6
replace elonely=. if loneliness==.
	lab var elonely "Extreme loneliness indicator >=6"

* Multidimensional Loneliness
egen mlonlely = rowmean(visitmeoften helpfromothers loneliness)
	lab var mlonlely "multi-dimensional loneliness"

* Factor analysis: visitmeoften helpfromothers loneliness
factor visitmeoften helpfromothers loneliness
rotate
predict loneliness_fa
	
* Chronic loneliness
xtset pid wave
gen vlonely_l1 = L1.vlonely
gen vlonely_f1 = F1.vlonely
gen vlonely_f2 = F2.vlonely
gen vlonely_f3 = F3.vlonely

g vlonely_chronic = 0
	replace vlonely_chronic = 1 if vlonely==1 & vlonely_f1==1 & vlonely_f2==1
	replace vlonely_chronic = . if vlonely==. | vlonely_f1==. | vlonely_f2==.
	
g vlonely_chronic2y = 0
	replace vlonely_chronic2y = 1 if vlonely==1 & vlonely_f1==1 
	replace vlonely_chronic2y = . if vlonely==. | vlonely_f1==. 
	
g vlonely_chronic4y = 0
	replace vlonely_chronic4y = 1 if vlonely==1 & vlonely_f1==1 & vlonely_f2==1 & vlonely_f3==1
	replace vlonely_chronic4y = . if vlonely==. | vlonely_f1==. | vlonely_f2==. | vlonely_f3==.
	
* Lack of social support
g lsupport=confidein>=5
replace lsupport=. if confidein==.
	lab var lsupport "Lack of social support"

* Chronic Lack of social support
xtset pid wave
gen lsupport_f1 = F1.lsupport
gen lsupport_f2 = F2.lsupport
gen lsupport_f3 = F3.lsupport

g lsupport_chronic = 0
	replace lsupport_chronic = 1 if lsupport==1 & lsupport_f1==1 & lsupport_f2==1
	replace lsupport_chronic = . if lsupport==. | lsupport_f1==. | lsupport_f2==.

g lsupport_chronic2y = 0
	replace lsupport_chronic2y = 1 if lsupport==1 & lsupport_f1==1 
	replace lsupport_chronic2y = . if lsupport==. | lsupport_f1==. 
		
g lsupport_chronic4y = 0
	replace lsupport_chronic4y = 1 if lsupport==1 & lsupport_f1==1 & lsupport_f2==1 & lsupport_f3==1
	replace lsupport_chronic4y = . if lsupport==. | lsupport_f1==. | lsupport_f2==. | lsupport_f3==.
	
	
* Active Club/Association member
g club_mem = lsclub==1
replace club_mem=0 if lsclub==2 
replace club_mem=. if lsclub==.
	lab var club_mem "Active Club/Association member"
	
*-----------
* Social isolation 
g nohelp=helpmeoutreversed>=5 
replace nohelp=. if helpmeoutreversed==.
	lab var nohelp "No one to help out indicator >=5"
	
g nofrien=lotsoffriendsreversed>=5 
replace nofrien=. if lotsoffriendsreversed==.
	lab var nofrien "Lots of friends indicator >=5"
	
	
g vleanon=leanon>=5
replace vleanon=. if leanon==.
	lab var vleanon "Lean on indicator >=5"

*-----------
* Chronic conditions
g any_chronic = helth == 1
    replace any_chronic=. if helth==.
	lab var vleanon "Any long term health condition"
     
gen any_chronic_l1 = L1.any_chronic

* only defined in wave 9, 13, 17, 21    
describe helth heany heart heast hecan hecbe hedi1 hedi2 hedep heomi hehcd hehbp heoc herf2 hedk2
su helth heany heart heast hecan hecbe hedi1 hedi2 hedep heomi hehcd hehbp heoc herf2 hedk2

*-----------
* Age group
gen agegroup =.

numlist "20(20)100"
tokenize `r(numlist)'

forvalues j = 1(1)4 {
	replace agegroup = `j' if hgage1 >= ``j''
}
la var agegroup "Age Group of Household Head"
la def age_group 1 "20-40" 2 "40-60" 3 "60-80" 4 ">80"
la val agegroup age_group

gen agegroup3 = .
	replace agegroup3 = 1 if hgage<25
	replace agegroup3 = 2 if inrange(hgage,25,50)
	replace agegroup3 = 3 if hgage>=50 & !missing(hgage)
la var agegroup3 "Age Group of Household Head"
la def agegroup3 1 "Below 25" 2 "25-50" 3 "50+"
la val agegroup3 agegroup3
	
gen agegroup40 = .
	replace agegroup40 = 0 if hgage<40
	replace agegroup40 = 1 if hgage>=40 & !missing(hgage)

la var agegroup40 "Age Group of Household Head"
la def agegroup40 0 "Below 40" 2 "40+"
la val agegroup40 agegroup40
	
gen agegroup50 = .
	replace agegroup50 = 0 if hgage<50
	replace agegroup50 = 1 if hgage>=50 & !missing(hgage)
	
la var agegroup50 "Age Group of Household Head"
la def agegroup50 0 "Below 50" 2 "50+"
la val agegroup50 agegroup50
	
gen agegroup55 = .
	replace agegroup55 = 0 if hgage<55
	replace agegroup55 = 1 if hgage>=55 & !missing(hgage)
	
la var agegroup55 "Age Group of Household Head"
la def agegroup55 0 "Below 55" 2 "55+"
la val agegroup55 agegroup55

gen agegroup45 = .
	replace agegroup45 = 0 if hgage<45
	replace agegroup45 = 1 if hgage>=45 & !missing(hgage)
	
la var agegroup45 "Age Group of Household Head"
la def agegroup45 0 "Below 45" 2 "45+"
la val agegroup45 agegroup45
	
xtset pid wave	
bysort pid (wave): gen agegroup3_l1 = L1.agegroup3
bysort pid (wave): gen agegroup40_l1 = L1.agegroup40
bysort pid (wave): gen agegroup50_l1 = L1.agegroup50
bysort pid (wave): gen agegroup55_l1 = L1.agegroup55
bysort pid (wave): gen agegroup45_l1 = L1.agegroup45

* Index of Economic Quintiles

gen seifa =.

numlist "1(2)10"
tokenize `r(numlist)'
forvalues j = 1(1)5 {
	replace seifa = `j' if hhsad10 >= ``j''
}
replace seifa=. if hhsad10==. 

la var seifa "Index of Relative Socio-Economic Advantage/Disadvantage"
la def quintile 1 "1" 2 "2" 3 "3" 4 "4" 5 "5"
la val seifa quintile

* ----------------------------------
* Employment type
gen employment_arrangement = .
	replace employment_arrangement = 9 if esempst == -1 & esbrd==3
	replace employment_arrangement = 8 if esempst == -1 & esbrd==2
	replace employment_arrangement = 7 if inlist(esempst,4)
	replace employment_arrangement = 6 if inlist(esempst,2,3)
	replace employment_arrangement = 5 if esempst == 1 & jbmcnt==8
	replace employment_arrangement = 4 if esempst == 1 & jbmcnt==2
	replace employment_arrangement = 3 if esempst == 1 & jbmcnt==1
	replace employment_arrangement = 2 if esempst == 1 & jbmcnt==3 & inrange(jbmhruc,0,34)
	replace employment_arrangement = 1 if esempst == 1 & jbmcnt==3 & jbmhruc>=35
label def employment_arrangement 1 "Standard" 2 "Permanent PT" 3 "Fixed-term" 4 "Casual" 5 "Other employees"6 "Self-employed" 7 "Unpaid family worker" 8 "Unemployed" 9 "Not in the labour force"
lab val employment_arrangement employment_arrangement
lab var employment_arrangement "Employment type"

gen employed = .
	replace employed = 1 if inrange(employment_arrangement,1,6)
	replace employed = 0 if inrange(employment_arrangement,7,9)
	
* ----------------------------------
* Personal income
* ABS CPI multiplier (source: ABS financial year figures cat no. 640101) 
gen cpi=.
     replace cpi = 74.625 if wave==1 // wave1=2001
     replace cpi = 76.85 if wave==2
     replace cpi = 78.95 if wave==3
     replace cpi = 80.8 if wave==4
     replace cpi = 82.975 if wave==5
     replace cpi = 85.925 if wave==6
     replace cpi = 87.925 if wave==7
     replace cpi = 91.75 if wave==8
     replace cpi = 93.375 if wave==9
     replace cpi = 96.1 if wave==10
     replace cpi = 99.275 if wave==11
     replace cpi = 101.025 if wave==12
     replace cpi = 103.5 if wave==13
     replace cpi = 106.075 if wave==14
     replace cpi = 107.675 if wave==15
     replace cpi = 109.05 if wave==16
     replace cpi = 111.175 if wave==17
     replace cpi = 113.3 if wave==18
     replace cpi = 115.125 if wave==19
     replace cpi = 116.1 if wave==20
     replace cpi = 119.425 if wave==21
     replace cpi = 127.3 if wave==22
	
replace wscei = wscei * cpi / 100

preserve
keep pid wave employed
renvars pid employed \ hhpxid employed_partner

tempfile partner_employed
save `partner_employed'

restore 
	
sort hhpxid wave
	merge m:1 hhpxid wave using `partner_employed'
	drop if _merge==2
	drop _merge
	
gen dual_earner = 0
	replace dual_earner = 1 if inlist(mrcurr,1,2) & employed == 1 & employed_partner==1
	replace dual_earner = . if employed==. | employed_partner==.

recode jbhruc (-10/-1=.), gen(work_hours)
gen parttime = work_hours<35 & !missing(work_hours)
gen fulltime = work_hours>=35 & !missing(work_hours)
	
xtset pid wave	
bysort pid (wave): gen parttime_l1 = L1.parttime
bysort pid (wave): gen fulltime_l1 = L1.fulltime
bysort pid (wave): gen dual_earner_l1 = L1.dual_earner

* ----------------------------------
* Household income
// Household disposable income
gen hdi=hifdip-hifdin if hifdip>=0 & hifdin>=0 & hifdip!=. & hifdin!=.
	label var hdi "Household disposable income"
	
*Real household disposable income (pegged to 2015 aud)
gen rhdi = hdi * cpi / 100
label var rhdi "Real household disp. income ('15 A$)"

// Household eqivalence factor
*ABS definition (1 point for first adult; .5 points for each subsequent 15+; .3 for each <15)
cap drop pnum
bysort hhrhid (xwaveid): gen pnum=_n
gen iheq=1 if pnum==1 & hgage>=15
replace iheq=.5 if pnum!=1 & hgage>=15
replace iheq=.3 if pnum!=1 & hgage<15
label var iheq "Indiv. contribution to household equivalence points"
bysort hhrhid wave: egen fseq=total(iheq)
label var fseq "Family size equivalence multiplier (ABS definition)"
drop iheq pnum

// Get real equivalized household disposable income
gen erhdi = rhdi/fseq
label var erhdi "Equivalized real household disposable income ('15 A$)"

// Above/below poverty line
qui sum erhdi,d
	gen bl_povline = . 
	replace bl_povline = 1 if erhdi<=r(p50)*0.6
	replace bl_povline = 0 if erhdi>r(p50)*0.6 & !missing(erhdi)
label var bl_povline "Below poverty line - 60% of median"

bysort pid (wave): gen bl_povline_l1 = L1.bl_povline

* ----------------------------------
* Education Category
gen educ =.

loc v edhigh1
replace educ = 1 if `v' == 1 | `v' == 2
replace educ = 2 if `v' == 3 | `v' == 4
replace educ = 3 if `v' == 5
replace educ = 4 if `v' == 8 
replace educ = 5 if `v' == 9

la def edu 1 "Postgraduate" 2 "Bachelors/Diploma" 3 "Cert 3 or 4" 4 "High School" 5 "Below High School"
la val educ edu
la var educ "Highest education achieved"

* Death data 
merge m:1 xwaveid using "Master_v220u.dta" , keepusing(yodeath-aadeath) nogen
merge 1:1 xwaveid year using "CNEF_Long_v220c.dta" , nogen

* -------------------------------------------------------------
* Gender 
tab zzd11102ll, miss
g female=zzd11102ll
recode female (1=0) (2=1)
tab female, miss
lab var female "Female (Yes/No)"
drop zzd11102ll

cap drop age marritalstatus
* age 
rename zzd11101 age

drop if age<15 
drop if age>=65 

* married
rename zzd11104 marritalstatus	
recode mrcurr (3/6=1 "Not married/cohabiting") (1=2 "Married") (2=3 "Cohabiting") (else=.), gen(marital)
	lab var marital "Marrital status"
	
* education
recode edhigh1 (9 10 = 1 "Year 11 and below") (8=2 "Year 12") (4/5=3 "Vocational") (1/3 = 4 "Bachelor and higher") (else=.), gen(education)
	lab var education "Education level"
	
* region
recode hhsra (0=1 "Major cities") (1/4=2 "Regional") (else=.), gen(areas)	
	
* country of birth	
recode ancob (-10/-1=.) (.=.) (1101 = 1 "Australian-born") (1201 2100 2201 9225 8104 8102 = 2 "Other English-speaking") (else=3 "Other non-English-speaking"), gen(aus_born)
	lab var aus_born "Country of birth"
	
* Social isolation: not getting together at least on a weekly basis
cap drop socialisolation
g socialisolation = 0 
replace socialisolation = 1 if lssocal>3 & zzd11106==1
replace socialisolation = . if lssocal==.
tab socialisolation


xtset pid wave
gen socialisolation_f1 = F1.socialisolation
gen socialisolation_f2 = F2.socialisolation
gen socialisolation_f3 = F3.socialisolation

g socialisolation_chronic = 0
	replace socialisolation_chronic = 1 if socialisolation==1 & socialisolation_f1==1 & socialisolation_f2==1
	replace socialisolation_chronic = . if socialisolation==. | socialisolation_f1==. | socialisolation_f2==.
	
g socialisolation_chronic2y = 0
	replace socialisolation_chronic2y = 1 if socialisolation==1 & socialisolation_f1==1 
	replace socialisolation_chronic2y = . if socialisolation==. | socialisolation_f1==. 
	
g socialisolation_chronic4y = 0
	replace socialisolation_chronic4y = 1 if socialisolation==1 & socialisolation_f1==1 & socialisolation_f2==1 & socialisolation_f3==1
	replace socialisolation_chronic4y = . if socialisolation==. | socialisolation_f1==. | socialisolation_f2==. | socialisolation_f3==.
	
	
* ----------------------------------
* Household Disposable Quintiles

xtile inc_quintile = hifditp, nq(5)
replace inc_quintile=. if hifditp==.

* Income Per head 

g equivhh = zzh11101*0.3 + (zzd11107-zzh11101-1)*0.5 + 1
g equivhhinc=hifditp/equivhh
su equivhhinc

xtile inc_quintile_equ = equivhhinc, nq(5)
replace inc_quintile_equ=. if equivhhinc==.
tab inc_quintile_equ, m 

*-----------
* Umemplyment rate 
tab hhura

*-----------
* Perception of job loss
clonevar job_loss_percep = jbmploj
replace job_loss_percep = . if job_loss_percep>100
lab var job_loss_percep "% change of losing job during next 12 months"

bysort xwaveid (wave): gen job_loss_percep_l1 = job_loss_percep[_n-1]
bysort xwaveid (wave): gen job_loss_percep_l2 = job_loss_percep[_n-2]
bysort xwaveid (wave): gen job_loss_percep_l3 = job_loss_percep[_n-3]
bysort xwaveid (wave): gen job_loss_percep_l4 = job_loss_percep[_n-4]
bysort xwaveid (wave): gen job_loss_percep_f1 = job_loss_percep[_n+1]
bysort xwaveid (wave): gen job_loss_percep_f2 = job_loss_percep[_n+2]
bysort xwaveid (wave): gen job_loss_percep_f3 = job_loss_percep[_n+3]
bysort xwaveid (wave): gen job_loss_percep_f4 = job_loss_percep[_n+4]

* Unemployed
gen unemployed = employment_arrangement == 8
	replace unemployed = . if employment_arrangement==. | employment_arrangement == 9

* Unemployed & Not in LF
gen unemployed_nlf = employment_arrangement == 8 | employment_arrangement==9
	replace unemployed_nlf = . if employment_arrangement==.
	
xtset pid wave	
bysort pid (wave): gen unemployed_l1 = L1.unemployed
bysort pid (wave): gen unemployed_l2 = L2.unemployed
bysort pid (wave): gen unemployed_l3 = L3.unemployed
bysort pid (wave): gen unemployed_l4 = L4.unemployed
bysort pid (wave): gen unemployed_f1 = F1.unemployed
bysort pid (wave): gen unemployed_f2 = F2.unemployed
bysort pid (wave): gen unemployed_f3 = F3.unemployed
bysort pid (wave): gen unemployed_f4 = F4.unemployed

*-----------
* Early retirement: 
gen early_retiremenet = 0
	replace early_retiremenet = 1 if lertr==2 & age < 67
	replace early_retiremenet = . if lertr==. 

*-----------
* Public vs private sector: 
gen public_sector = .
	replace public_sector = 1 if inlist(jbmmply,2,5)
	replace public_sector = 0 if inlist(jbmmply,1,4)
	
xtset pid wave	
bysort pid (wave): gen public_sector_l1 = L1.public_sector

*-----------
* Permanent vs fixed-term/casual: 
gen permanent = .
	replace permanent = 1 if inlist(employment_arrangement,1,2)
	replace permanent = 0 if inlist(employment_arrangement,3,4)
bysort pid (wave): gen permanent_l1 = L1.permanent
	
	
* Alternative measures of job loss perception
recode jomsf (-10/-1 = .)
	vreverse jomsf, gen(job_secure) 
	lab var job_secure "I have a secure future in my job (reversed)"
recode jomcsb (-10/-1 = .)
	vreverse jomcsb, gen(job_future) 
	lab var job_future "The company I work for will still be in business 5 years from now (reversed)"
recode jomwf (-10/-1 = .), gen(job_worry)
	lab var job_worry "I worry about the future of my job"
	
// Factor analysis of job loss perception measures: job_secure job_future job_worry 
factor job_secure job_future job_worry 
rotate
predict factor1
rename 	factor1 job_insecurity_fa

*----------- High skill-low skill
gen highskill = .
	replace highskill = 1 if education==4
	replace highskill = 0 if inlist(education,1,2,3)
xtset pid wave	
bysort pid (wave): gen highskill_l1 = L1.highskill
	
	
* ----------------------------------

* Health Consumption 

tab hehan

tab hegpn 

* SF36: Mental Health 
tab ghmh

* Mental/physical distress: ghmh ghpf
gen mental_disorder = 0
	replace mental_disorder = 1 if ghmh<50
	replace mental_disorder = . if ghmh==.
bysort pid (wave): gen mental_disorder_l1 = L1.mental_disorder
	
gen physical_disorder = 0
	replace physical_disorder = 1 if ghpf<50
	replace physical_disorder = . if ghpf==.
bysort pid (wave): gen physical_disorder_l1 = L1.physical_disorder

gen mental_disorder_med = 0
	replace mental_disorder_med = 1 if ghmh<76
	replace mental_disorder_med = . if ghmh==.
bysort pid (wave): gen mental_disorder_medl1 = L1.mental_disorder_med
	
gen physical_disorder_med = 0
	replace physical_disorder_med = 1 if ghpf<95
	replace physical_disorder_med = . if ghpf==.
bysort pid (wave): gen physical_disorder_medl1 = L1.physical_disorder_med

* SF36: Physical health 
tab ghpf

drop hgsex*

* Other social health measures: hecpmhp slsttm aptmet helstyr slsrate jsactfr lsnwkit lsnwtca mdhagt
gen mh_visit = hecpmhp==1
	replace mh_visit = . if hecpmhp==.
gen sleep_med = slsttm>1
	replace sleep_med = . if slsttm==.
gen exercise = aptmet>= 840		
	replace exercise = . if aptmet==.

gen gp_visit = helstyr==1
	replace gp_visit = . if helstyr==.	
gen sleep_lowquality = inlist(slsrate,3,4)
	replace sleep_lowquality = . if slsrate==.	
	
gen contact_friend = jsactfr
gen intouch_friend = .
	replace intouch_friend = 1 if inrange(lsnwkit,5,6)
	replace intouch_friend = 0 if inrange(lsnwkit,1,4)
gen talk_affair = .
	replace talk_affair = 1 if inrange(lsnwtca,5,6)
	replace talk_affair = 0 if inrange(lsnwtca,1,4)	
gen material_deprivation = .
	replace material_deprivation = 1 if mdhagt==1
	replace material_deprivation = 0 if mdhagt==2	
		
* ----------------------------------
* Match with SA4 unemployment rate using survey month and year
sort hhssa4 month year
cap	merge m:1 hhssa4 month year using "unemployment_sa4.dta"
	drop if _merge==2	// 469 _merge==1
	drop _merge
	
sort year
cap	merge m:1 year using "unemployment_aus.dta"
	drop if _merge==2	// 469 _merge==1
	drop _merge
	
sum unemployment_sa4
	gen unemployment_sa4_5pc = unemployment_sa4 >= 5
		replace unemployment_sa4_5pc = . if unemployment_sa4==.
	gen unemployment_sa4_3pc = unemployment_sa4 >= 3
		replace unemployment_sa4_3pc = . if unemployment_sa4==.
	gen unemployment_sa4_group = .
		replace unemployment_sa4_group = 1 if unemployment_sa4 <= 3
		replace unemployment_sa4_group = 2 if unemployment_sa4 >3 & unemployment_sa4 <=6
		replace unemployment_sa4_group = 3 if unemployment_sa4 >6 & unemployment_sa4 <=9
		replace unemployment_sa4_group = 4 if unemployment_sa4 >9 & !missing(unemployment_sa4)

xtset pid wave	
gen unemployment_sa4_l1 = L1.unemployment_sa4	
gen unemployment_sa4_l2 = L2.unemployment_sa4	

// Exclude COVID year
drop if year>2019

* Convert job loss perception scale
replace job_loss_percep = job_loss_percep/100

* Life event: job loss - lefrd
recode lefrd (1=0 "No") (2=1 "Yes"), gen(event_jobloss)

// Life event 12 months: leins leinf ledsc ledrl ledfr levio lepcm ledhm
* serious injuries
recode leins (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(serious_injury)
	lab var serious_injury "Life events in past year: Serious personal injury/illness"
	
* Serious personal injury or illness to a close relative / family
recode leinf (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(serious_injury_other)
	lab var serious_injury_other "Life events in past year: Serious injury or illness to a close relative / family"

* Death of spouse or child
recode ledsc (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(death_spouse_child)
	lab var death_spouse_child "Life events in past year: Death of spouse or child"

* Death of close relative/family memb
recode ledrl (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(death_relative)
	lab var death_relative "Life events in past year: Death of close relative/family memb"

* Death of a close friend
recode ledfr (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(death_friend)
	lab var death_friend "Life events in past year: Death of a close friend"
	
* Victim of physical violence
recode levio (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(victim_violence)
	lab var victim_violence "Life events in past year: Victim of physical violence"
	
* Victim of a property crime
recode lepcm (-10/-1=.) (1=0 "No") (2=1 "Yes"), gen(victim_property)
	lab var victim_property "Life events in past year: Victim of a property crime"
	
* weather related disaster
recode  ledhm (1=0) (2=1) (else=.), gen(disaster)
lab var disaster "Life events in past year: A weather related disaster (flood, bushfire, cyclone) damaged"

* Separated from spouse
recode  lesep (1=0) (2=1) (else=.), gen(separate_spouse)
lab var separate_spouse "Life events in past year: separated from spouse or long-term partner"

// Risk & time preferences: firisk fisavep firiska
clonevar risk_preference = firisk
	replace risk_preference = . if firisk==5
	replace risk_preference = 1 if firiska==1
	replace risk_preference = 2 if firiska==2
	replace risk_preference = 3 if firiska==3
	replace risk_preference = 4 if firiska==4
	replace risk_preference=. if risk_preference<0

clonevar risk_preference1 = firisk
	replace risk_preference1 = . if firisk==5
	replace risk_preference1 = . if risk_preference1<0

// LoC
label define LIKERT 1  "[1] Strongly Agree"  2 "[2]" 3 "[3]"  4 "[4]"  5 "[5]"  6 "[6]" 7 "[7] Strongly Disagree"
gen control1 = lsselc  if lsselc >=1
recode control1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
label var control1 "Personal control: Little control"
label values control1 LIKERT
gen control2 = lssesp   if lssesp  >=1
recode control2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
label var control2 "Personal control: No way to solve problems"
label values control2 LIKERT
gen control3 = lsseci  if lsseci >=1
recode control3 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
label var control3 "Personal control: Cannot change important things in life"
label values control3 LIKERT
gen control4 = lssefh  if lssefh >=1
recode control4 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
label var control4 "Personal control: Feel helpless "
label values control4 LIKERT
gen control5 = lssepa  if lssepa >=1
recode control5 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1)
label var control5 "Personal control: Pushed around "
label values control5 LIKERT
gen control6 = lssefd  if lssefd >=1
label var control6 "Personal control: Future depends on me "
label values control6 LIKERT
gen control7 = lssecd  if lssecd  >=1
label var control7 "Personal control: Can do just about anything "
label values control7 LIKERT

gen LOC_overall = (control1 + control2 + control3 + control4 + control5 + control6 + control7) / 7
label var LOC_overall "Locus of control (overall)"	
	
// Big 5 traits
renvars pnextrv pnagree pnconsc pnemote pnopene \ Extroversion Agreeableness Conscientiousness Emotional_stab Openness
for var Extroversion Agreeableness Conscientiousness Emotional_stab Openness: replace X = . if X<0
foreach v in Extroversion Agreeableness Conscientiousness Emotional_stab Openness {
	bysort xwaveid: egen `v'_mean = mean(`v')
}
	
* Generate missing indicator for loneliness variables
gen missing_outcome = 0
	replace missing_outcome =1 if vlonely ==. | lsupport ==. |  socialisolation ==. 

compress
save "main_loneliness.dta", replace

