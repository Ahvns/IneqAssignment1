// Assignment 1 inequalities
// Authors: 428365 & 492810

** Opening Data **


use "ewcs6_2015_ukda_1904.dta", clear


** Data Prep **
** Renaming all the variables **

// Renaming
	rename Q2a			resp_gender
	rename Q2b			resp_age
	rename Q4a			par_domestic
	rename Q4b			domestic
	rename Q14			sector
	rename Q17			employmentlength
	rename Q24			hoursweek
	rename Q25			hoursweek_pref
	rename Q28			otherjob_hours
	rename Q35e			location_home
	rename Q42			time_set_type
	rename Q44			worklife_balance
	rename Q46			work_outside_hrs
	rename Q47			take_time_off
	rename Q104_euro	net_monthly_euro


/* aanpassingen */
replace otherjob_hours		= 0			if otherjob_hours	== .
replace otherjob_hours		= .d		if otherjob_hours	== 888
replace otherjob_hours		= .r		if otherjob_hours	== 999
replace net_monthly_euro	= .d		if net_monthly_euro	== 88888888
replace net_monthly_euro	= .r		if net_monthly_euro	== 99999999
replace hoursweek			= .d		if hoursweek		== 888
replace hoursweek_pref		= hoursweek	if hoursweek_pref	== 777
replace hoursweek_pref		= .d		if hoursweek_pref	== 888
replace hoursweek_pref		= .r		if hoursweek_pref	== 999
replace domestic			= 1			if par_domestic		== 1
replace ISCED				= .d		if ISCED			== 88
replace ISCED				= .r		if ISCED			== 99
replace location_home		= .d		if location_home	== 8
replace location_home		= .r		if location_home	== 9
replace time_set_type		= .d		if time_set_type	== 8
replace time_set_type		= .r		if time_set_type	== 9
replace worklife_balance	= .d		if worklife_balance	== 8
replace worklife_balance	= .r		if worklife_balance	== 9
replace work_outside_hrs	= .n		if work_outside_hrs == 7
replace work_outside_hrs	= .d		if work_outside_hrs == 8
replace work_outside_hrs	= .r		if work_outside_hrs == 9
replace take_time_off		= .d		if take_time_off	== 8
replace take_time_off		= .r		if take_time_off	== 9
replace employmentlength	= 0			if employmentlength	== 999
replace employmentlength	= .n		if employmentlength	== 77
replace employmentlength	= .d		if employmentlength	== 88
replace employmentlength	= .r		if employmentlength	== 99
replace resp_age			= .d		if resp_age			== 888
replace resp_age			= .r		if resp_age			== 999
replace domestic			= .n		if domestic			== 7
replace domestic			= .r		if domestic			== 9




// cleanup
keep if resp_gender == 1 | resp_gender == 2 // keep only male & female

// drop unused vars
keep	net_monthly_euro resp_gender ISCED resp_age employmentlength Country ///
		domestic hoursweek hoursweek_pref otherjob_hours worklife_balance ///
		work_outside_hrs location_home time_set_type take_time_off w4

/* nieuwe vars */
tab ISCED,				generate(educationlevel)
tab Country,			generate(country)
tab time_set_type,		generate(time_set)
tab work_outside_hrs,	generate(outside_hrs)
tab location_home,		generate(workhome)


gen total_hours 	= hoursweek + otherjob_hours
gen net_hourly_euro	= net_monthly_euro / ( total_hours * 52 / 12)
gen log_monthly		= ln(net_monthly_euro)
gen log_hourly		= ln(net_hourly_euro)
gen difhrs			= total_hours - hoursweek_pref
gen difhrs_r		= hoursweek_pref / total_hours
gen difhrsless		= difhrs > 0
gen difhrsmore		= difhrs < 0
gen difhrssame		= difhrs == 0
gen time_off		= take_time_off < 3
gen worklife		= worklife_balance < 3


// regressions & analysis
svyset [weight=w4]	// set as survey data

lab var difhrs				"Tot. - pref. hours"
lab var difhrs_r			"Pref. / tot. hours"
lab var difhrsless			"Prefers to work less"
lab var difhrsmore			"Prefers to work more"
lab var difhrssame			"Happy with working hours"
lab var resp_age 			"Age"
lab var domestic 			"Domestic"
lab var employmentlength	"Years of employment"
lab var total_hours			"Weekly working hours"
lab var log_monthly			"Log monthly earnings"
lab var log_hourly			"Log hourly earnings"
lab var time_off			"Easy to take time off"
lab var worklife			"Good work/life balance"
lab var resp_gender			"Gender [1 = male, 2 = female]"
lab var hoursweek_pref		"Preferred weekly working hours"
lab var time_set1			"- Set by company"
lab var time_set2			"- Choose between fixed options"
lab var time_set3			"- Adapt hours within limits"
lab var time_set4			"- Full autonomy"


foreach var of varlist educationlevel* {
	local varlabel : variable label `var'
	local varlabel : subinstr local varlabel "ISCED==" ""
	local varlabel : subinstr local varlabel "education" ""
	lab var `var' "- `varlabel'"
}
foreach var of varlist country* {
	local varlabel : variable label `var'
	local varlabel : subinstr local varlabel "Country==" ""
	lab var `var' "- `varlabel'"
}
foreach var of varlist outside_hrs* {
	local varlabel : variable label `var'
	local varlabel : subinstr local varlabel "work_outside_hrs==" ""
	lab var `var' "- `varlabel'"
}
foreach var of varlist workhome* {
	local varlabel : variable label `var'
	local varlabel : subinstr local varlabel "location_home==" ""
	lab var `var' "- `varlabel'"
}


/*	summary statistics	*/
eststo clear

bysort resp_gender: eststo: qui estpost summarize ///
	resp_age domestic employmentlength total_hours hoursweek_pref ///
	log_monthly log_hourly time_off worklife educationlevel* time_set1-time_set4 ///
	outside_hrs* workhome*

eststo Difference: qui estpost ttest resp_age domestic employmentlength ///
	total_hours hoursweek_pref ///
	log_monthly log_hourly time_off worklife educationlevel* time_set1-time_set4 ///
	outside_hrs* workhome*, by(resp_gender) unequal
	
eststo Total: qui estpost summarize ///
	resp_age domestic employmentlength total_hours hoursweek_pref ///
	log_monthly log_hourly time_off worklife educationlevel* time_set1-time_set4 ///
	outside_hrs* workhome*

esttab using "Summary.tex", ///
	refcat(educationlevel1 "Education level" time_set1 "Scheduling decision" ///
	outside_hrs1 "Works outside regular hours" workhome1 "Works from home", nolabel) ///
	cells("mean(pattern(1 1 0 1) fmt(2)) sd(pattern(1 1 0 1) fmt(2)) b(star pattern(0 0 1 0) fmt(2)) se(pattern(0 0 1 0) fmt(2))") nodepvar label replace booktabs title("Summary Statistics") notes

	
// monthly earnings

eststo clear
/* specificatie 1 : baseline pay gap */
oaxaca 	log_monthly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo baseline		

/* specificatie 2 : aantal uur */
oaxaca	log_monthly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours difhrs worklife outside_hrs1-outside_hrs4 ///
		country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo number

/* specificatie 3 : timing van uren */
oaxaca	log_monthly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours workhome1-workhome4 time_set2-time_set4 time_off ///
		country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo timing

/* specificatie 4 : alle flexibilitieit */
oaxaca log_monthly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours difhrs worklife outside_hrs1-outside_hrs4 ///
		workhome1-workhome4 ///
		time_set2-time_set4 time_off country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo full

esttab baseline number timing full using "resultsmonth.tex", se(3) booktabs title("Log Monthly Earnings Decomposition") mtitles("Baseline" "Amount" "Timing" "Full") replace

// uurlonen		
eststo clear

/* specificatie 1 : baseline pay gap */
oaxaca 	log_hourly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo baseline

/* specificatie 2 : aantal uur */
oaxaca	log_hourly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours difhrs worklife outside_hrs1-outside_hrs4 ///
		country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo number

/* specificatie 3 : timing van uren */
oaxaca	log_hourly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours workhome1-workhome4 time_set2-time_set4 time_off ///
		country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail // options

eststo timing

/* specificatie 4 : alle flexibilitieit */
oaxaca log_hourly /// dep var
		resp_age employmentlength educationlevel2-educationlevel9 /// indep var
		domestic total_hours difhrs worklife outside_hrs1-outside_hrs4 ///
		workhome1-workhome4 ///
		time_set2-time_set4 time_off country2-country35, ///
		by(resp_gender) /// group indicator
		svy nodetail// options
		
eststo full

esttab baseline number timing full using "resultshour.tex", se(3) booktabs title("Log Hourly Wage Decomposition") mtitles("Baseline" "Amount" "Timing" "Full") replace
