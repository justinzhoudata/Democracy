*log using group7log.smcl, replace
/*----------------------------------------------------------------------------
Group 7 Final Project : GDP, Rainfall, and Democracy
Author: Sophia Pegues
Last updated: 5/25/2022

Datasets needed: 
	mss_data.dta 
	GDP.csv
	independence.csv
------------------------------------------------------------------------------*/



*-*-*-* SETUP *-*-*-*

* setting directory *
if "`c(username)'" == "sophi" {
	global path C:\Users\sophi\Desktop\econ110\Project 
}
else if "`c(username)'" == "cpulido" {
	global path C:\Users\cpulido\OneDrive - The University of Chicago\econometrics
}
else if "`c(username)'" == "justinzhou" {
	global path C:\Users\justinzhou\OneDrive - The University of Chicago\Desktop
}
else if "`c(username)'" == "whuang1" {
	global path C:\Users\whuang1\OneDrive - The University of Chicago\Intro to Econometrics
}
/*else if "`c(username)'" == "<insert username here>" {
	global path <insert datasets file folder directory path here>
}*/
cd "$path" 

* installing packages *
quietly ssc install filelist //not directly used in dofile
quietly ssc install unique //not directly used in dofile
quietly ssc install estout
quietly ssc install labutil


*-*-*-* LOADING DATA *-*-*-*

* importing the datasets and reformating *
use mss_data.dta, clear //precipitation, democracy index, ethnic fractionalization, religious fractionalization, and population data

*********** INCOMPLETE: MORE FORMATING NEEDED ***********
rename country_name country
labmask ccode, values(country_code)
sort country
save mss.dta, replace

import delim GDP.csv, clear // gdp data
rename ïcountryname country
quietly reshape long gdp, i(country) j(year)
label var gdp "GDP per capita (current US$)"
sort country year
keep country countrycode year gdp
save gdp.dta, replace

import delim independence.csv, varnames(1) clear //country independence declaration year data
rename ïcountry country
label var independence "Year Independence Declared"
replace independence = substr(strtrim(independence), 1, 4) //extracting years only from date
quietly destring independence, replace
sort country
save independence.dta, replace

* merging datasets *
use mss.dta, clear

merge m:m country year using gdp.dta
drop if _merge <=2
drop _merge

merge m:1 country using independence.dta
drop if _merge <=2
drop _merge

save project.dta, replace

* cleaning up merged dataset *
use project.dta, clear

keep year country ccode GPCP pop independence ethfrac relfrac democ gdp // keeping only relevant variables

* creating and labeling variables *
gen age = year - independence
label var age "Age of Country"

gen dem_index = democ
replace dem_index = . if democ == -88 | democ == -77 //dropping special cases that mess with continuous variable interpretation 
label var dem_index "Democracy Index"

gen log_gdp = log(gdp)
label var log_gdp "Log of GDP"

gen rain = GPCP/100 //converting to meters
label var rain "Avg Yearly Precipitation (meters)"
gen log_rain = log(rain)
label var log_rain "Log of Avg Yearly Precipitation"

gen log_pop = log(pop)
label var log_pop "Log of Population"

drop if year == 1999
tab year, gen(y_)
rename y_1 y //creating reference year (year == 1981)

tab country, gen(ctry_)
rename ctry_14 ctry //creating reference country (ccode == 404 ["GNB"])
save project.dta, replace

label var pop "Population in 1000s"
label var ethfrac "Ethnic Fractionalization"
label var relfrac "Religious Fractionalization"

drop independence GPCP // dropping more unneeded variables
 
save project.dta, replace

* exporting dataset *
export delimited using project.csv, replace



*-*-*-* ANALYSIS *-*-*-*

use project.dta, clear
xtset ccode year //setting panel dataset

* dropping observations that aren't in regression *
	quietly xtivreg dem_index log_pop ethfrac relfrac y_* (log_gdp = log_rain), fe vce(cluster country)
	gen sample = e(sample) //finding observations that are missing values and thus not included in regression
	drop if sample == 0
	drop sample

* summarizing variables *

estpost tabstat log_gdp log_rain dem_index log_pop age ethfrac relfrac, s(mean sd max min) columns(s) listwise

esttab using summary.txt, replace cells((mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)))) noobs type label wrap modelwidth(9) noabbrev varwidt(38) title("Summary Statistics") // reporting summary in neat table
		*add by year summary table

xtline log_gdp dem_index log_rain , t(year) i(country)

* running regressions *

estimates clear

	** first stage *
	
		*** with population logged and observations dropped ***
			quietly xtreg log_gdp log_rain log_pop ethfrac relfrac y_* , fe vce(cluster country)
			estimate store r1a
			
			esttab r1a using firststage.txt, cells(b(star fmt(%9.2f)) se(par)) stats(r2_a N, fmt(%9.2f %9.0g) labels("Adj. R-squared" "N")) starlevels(* 0.10 ** 0.05 *** 0.01) label replace type varlabels(_cons "Constant") collabels(none) noabbrev keep(log_rain log_pop ethfrac relfrac _cons) varwidt(38) //reporting estimate in neat table
			
		*** without population logged and keeping observations***
			use project.dta, clear
			xtset ccode year 
			
			quietly xtreg log_gdp log_rain pop ethfrac relfrac y_* , fe vce(cluster country)
			estimate store r1b
			
			esttab r1b using firststage.txt, cells(b(star fmt(%9.2f)) se(par)) stats(r2_a N, fmt(%9.2f %9.0g) labels("Adj. R-squared" "N")) starlevels(* 0.10 ** 0.05 *** 0.01) label replace type varlabels(_cons "Constant") collabels(none) noabbrev keep(log_rain pop ethfrac relfrac _cons) varwidt(38) //reporting estimate in neat table
	
	** second stage regression **
	quietly xtivreg dem_index (log_gdp = log_rain), vce(cluster country) // base regression
	estimate store r2a
	quietly xtivreg dem_index (log_gdp = log_rain), fe vce(cluster country) // just country controls
	estimate store r2b
	quietly xtivreg dem_index y_* (log_gdp = log_rain), fe vce(cluster country) // just year and country controls
	estimate store r2c
	quietly xtivreg dem_index log_pop y_* (log_gdp = log_rain), fe vce(cluster country) // adding population
	estimate store r2d
	quietly xtivreg dem_index log_pop ethfrac y_* (log_gdp = log_rain), fe vce(cluster country) // adding ethnic fractionalization
	estimate store r2e
	quietly xtivreg dem_index log_pop ethfrac relfrac y_* (log_gdp = log_rain), fe vce(cluster country) // adding religious fractionalization
	estimate store r2f
	
	esttab r2a r2b r2c r2d r2e r2f using 2ndstage.txt, cells(b(star fmt(%9.2f)) se(par)) stats(N, fmt(%9.2f %9.0g) labels("N")) starlevels(* 0.10 ** 0.05 *** 0.01) label replace type varlabels(_cons "Constant") collabels(none) noabbrev keep(log_gdp log_pop ethfrac relfrac _cons) varwidt(30) modelwidth(7) //reporting estimates in neat table

	
	
*-*-* END OF DOFILE *-*-*
*log close
*translate group7log.smcl group7log.pdf, replace