////////////////////////////////////////////////////////////////////////////////
**# START PREPARE DATA FOR ANALYSES
////////////////////////////////////////////////////////////////////////////////

clear
import spss using "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)\DX SIT.sav"
recode DX_WILLINGNESSTODONATE (2 =1) (1 =0)
drop if missing(DX_ETHNICITY)
drop if missing(DX_REGRESIDENCY)
drop if missing(DX_WILLINGNESSTODONATE)


cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"

drop if missing(DX_COUNTRYOFBIRTH)
tab DX_INCLUDEFORANALYSES
drop if  DX_INCLUDEFORANALYSES == 0
save "datset_oppositions", replace


////////////////////////////////////////////////////////////////////////////////
**# END PREPARE DATA FOR ANALYSES
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# START TABULATE DATA
////////////////////////////////////////////////////////////////////////////////

cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"
use "datset_oppositions", clear

// table of frequency Refusual rate by Region
table (DX_REGRESIDENCY) ( ) ( ), ///
	stat(count DX_WILLINGNESSTODONATE) stat(mean DX_WILLINGNESSTODONATE) ///
	nformat(%9.0f count) ///
	nformat(%4.3f mean)

// table of frequency Refusual Rate by Country
table (DX_COUNTRYOFBIRTH) ( ) ( ), ///
	stat(count DX_WILLINGNESSTODONATE) stat(mean  DX_WILLINGNESSTODONATE) ///
	nformat(%9.0f count) ///
	nformat(%4.3f mean)

// table of frequency Refusual Rate by Ethnicity
table (DX_ETHNICITY) ( ) ( ), ///
	stat(count DX_WILLINGNESSTODONATE) stat(mean DX_WILLINGNESSTODONATE) ///
	nformat(%9.0f count) ///
	nformat(%4.3f mean)	
	
// get figures for non-EU non Italian
count
local tot = r(N)
count if DX_ETHNICITY != 1
local noneu = r(N)
count if DX_COUNTRYOFBIRTH != 60
local noit = r(N)

di in ye "-----> total number: " in wh `tot'
di in ye "-----> non-EU (%): " in wh %3.1f  `noneu'/ `tot'  * 100
di in ye "-----> non-Italian (%): " in wh %3.1f `noit'/ `tot'  * 100
	


////////////////////////////////////////////////////////////////////////////////
**# END TABULATE DATA
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# START PLOTs FOR DATA DISTRIBUTION
////////////////////////////////////////////////////////////////////////////////

cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"
use "datset_oppositions", clear
* net install treecluster, from("https://raw.githubusercontent.com/asjadnaqvi/stata-treecluster/main/installation/") replace
* ssc install schemepack, replace

// Horizontal clustered tree plot Ethnicity - Conutry of Birth
preserve
contract DX_ETHNICITY DX_COUNTRYOFBIRTH, freq(value)
set scheme white_tableau
graph set window fontface "Arial Narrow"
treecluster value, by(DX_ETHNICITY DX_COUNTRYOFBIRTH)  ///
	msize(1.5) labsize(1.8) scalefac(0.10)
graph export distr_ethnicity_contry_of_birth.png, replace
graph export distr_ethnicity_contry_of_birth.pdf, replace
graph export distr_ethnicity_contry_of_birth.tif, replace
restore

// Circular clustered tree plot Region - Ethnicity
cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"
use "datset_oppositions", clear
preserve
contract DX_REGRESIDENCY  DX_ETHNICITY, freq(value)
set scheme white_tableau
graph set window fontface "Arial Narrow"
treecluster value, by(DX_REGRESIDENCY DX_ETHNICITY)  ///
	msize(0.7) labsize(0.8) polar scalefac(0.05)
graph export distr_residency_ethnicity.png, replace
graph export distr_residency_ethnicity.pdf, replace
graph export distr_residency_ethnicity.tif, replace
restore

// Circual Tree Cluster plot Region - Country of Birth
cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"
use "datset_oppositions", clear
preserve
contract DX_REGRESIDENCY  DX_COUNTRYOFBIRTH, freq(value)
set scheme white_tableau
graph set window fontface "Arial Narrow"
treecluster value, by(DX_REGRESIDENCY DX_COUNTRYOFBIRTH)  ///
	msize(0.3) labsize(0.6) polar  scalefac(0.05)
graph export distr_residency_country_of_birth.png, replace
graph export distr_residency_country_of_birth.pdf, replace
graph export distr_residency_country_of_birth.tif, replace
restore


////////////////////////////////////////////////////////////////////////////////
**# END PLOTs FOR DATA DISTRIBUTION
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**# START BAYESAIN MODEL ETHNICITY Two-way crossed random effects logistic reg.
////////////////////////////////////////////////////////////////////////////////

cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"
use "datset_oppositions", clear

// run the two-way crossed random effects logistic reression model
* bayesparallel, nproc(3): ///
bayes, rseed(17) burnin(7500) mcmcsize(15000) thinning(10) nchains(3) ///
	saving(mcmc_ethnicity_a, replace) : ///
	melogit DX_WILLINGNESSTODONATE ///
	|| _all: R.DX_REGRESIDENCY || _all: R.DX_ETHNICITY
	
// check convergence 
bayesstats grubin	

// check Effective sample sizes and related statistics and diag. plots
bayesstats ess
bayesgraph diagnostics _all
bayesgraph diagnostics {DX_WILLINGNESSTODONATE: _cons}

// visualize random effects
bayesstats summary {U0:sigma2 V0:sigma2 DX_WILLINGNESSTODONATE: _cons}
bayesstats summary {U0[DX_REGRESIDENCY]: }
bayesstats summary {V0[DX_ETHNICITY]: }

// save estimates for future use
estimate save mcmc_bayes_ethnicity_120123, replace
* estimate use mcmc_bayes_ethnicity_120123

// create dataset for plotting from estimates in current memory 
bayesstats summary {U0}
matrix list r(summary)

cap drop ub* 
cap drop mean 
cap drop sd 
cap drop mcse 
cap drop median 
cap drop lb95 
cap drop hb95
cap drop pr ll ul etn

svmat r(summary), names(ub)

rename ub1 mean
rename ub2 sd
rename ub3 mcse
rename ub4 median
rename ub5 lb95
rename ub6 ub95

cap drop label
levelsof DX_ETHNICITY, local(levels)
display "`r(levels)'"
cap drop ethnicity
 gen ethnicity = .
local i = 0
foreach l of local levels {
	local i = `i' + 1
    qui replace ethnicity = `l' in `i'
    }
tab DX_ETHNICITY
tab DX_ETHNICITY, nolab

label define ethnicity ///
          1 "EU-BORN" ///
          2 "EASTERN EUROPEAN" ///
          3 "ASIAN" ///
          4 "HISPANIC"  ///
          5 "AFRICAN"  ///
          6 "NORTH AFRICA & MIDDLE EAST", modify
		
label values ethnicity ethnicity


sort mean
list ethnicity mean in 1/6, noobs sep(0)
list ethnicity in 1/24, noobs sep(0)


cap drop rankb
egen rankb = rank(mean), unique
serrbar mean sd rankb, scale(1.96) ///
xlab(1(1)6) ///
title() xtitle(Rank) ytitle(Ethnicity effects) legend(off) yline(0)

cap drop pr ll ul
gen pr = exp(mean) / (1 + exp(mean)) * 100
gen ll = exp(lb95) / (1 + exp(lb95)) * 100
gen ul = exp(ub95) / (1 + exp(ub95)) * 100

cap drop ctn
gen ctn = _n
drop if ctn > 6


// make the plot
local eu = pr[1]
twoway ///
 scatter ctn pr, msymbol(circle) msize(*0.8) mcolor(navy) mfcolor(%30) || ///
 rcap ll ul ctn, horizontal color(navy) || ///
 ,  ///
  xline(`eu', lcolor(maroon) lwidth(mthick) lpattern(dash)) ///
  xtitle("(%)", size(*1.2)) xscale(range(20 100) titlegap(2)) ///
  xlabel(20(10)100, labsize(*0.8) grid) ///
  ylabel(1  "EU-born" 2 "Hispanic" 3 "Eastern European" 4 "Africa" 5 "Asian" ///
  6 "North Africa & Middle East"  /// 
  , angle(horizontal) labsize(*.8) notick) ///
  yscale( range(0 7) reverse noline) ytitle(" ") ///
  legend(off)  scheme(s1mono)  /// 
  graphregion(color(white)) bgcolor(white)  ///
  plotregion(style(none)) ///
  title("Refusals to donation  by Ethnicity", box bexpand size(*0.8))

graph export prop_opp_by_ethnicicty.png, replace
graph export prop_opp_by_ethnicicty.pdf, replace
graph export prop_opp_by_ethnicicty.tif, replace

graph export prop_opp_by_ethnicicty.png, replace
graph export prop_opp_by_ethnicicty.pdf, replace
graph export prop_opp_by_ethnicicty.tif, replace

// save data used for the plot
save data_for_plot_ethnicicty, replace

////////////////////////////////////////////////////////////////////////////////
**# END BAYESAIN MODEL ETHNICITY Two-way crossed random effects logistic reg.
////////////////////////////////////////////////////////////////////////////////	

////////////////////////////////////////////////////////////////////////////////
**# START BAYESAIN MODEL COUNTRY  Two-way crossed random effects logistic reg.
////////////////////////////////////////////////////////////////////////////////


cd "C:\Users\Pc\Dropbox\DATI PROGETTO ME TOO\DATI ICU - SIT (OK)"
use "datset_oppositions", clear

// run the two-way crossed random effects logistic reression model
* bayesparallel, nproc(3): ///
bayes, rseed(17) burnin(7500) mcmcsize(15000) thinning(10) nchains(3) ///
saving(mcmc_countyrofbirth_a, replace) : ///
melogit DX_WILLINGNESSTODONATE ///
	|| _all: R.DX_REGRESIDENCY || _all: R.DX_COUNTRYOFBIRTH

	
// check convergence
bayesstats grubin

// check Effective sample sizes and related statistics and diag. plots
bayesstats ess
bayesgraph diagnostics _all
bayesgraph diagnostics {DX_WILLINGNESSTODONATE: _cons}

// visualize random effects
bayesstats summary {U0:sigma2 V0:sigma2 DX_WILLINGNESSTODONATE: _cons}
bayesstats summary {U0[DX_REGRESIDENCY]: }
bayesstats summary {V0[DX_COUNTRYOFBIRTH]: }


// save estimates for future use
estimate save mcmc_bayes_countyrofbirth_120123, replace
estimate use mcmc_bayes_countyrofbirth_120123
	

// create dataset for plotting from estimates in current memory 
bayesstats summary {V0}
matrix list r(summary)

cap drop ub* 
cap drop mean 
cap drop sd 
cap drop mcse 
cap drop median 
cap drop lb95 
cap drop hb95
cap drop pr ll ul etn

svmat r(summary), names(ub)

rename ub1 mean
rename ub2 sd
rename ub3 mcse
rename ub4 median
rename ub5 lb95
rename ub6 ub95

cap drop label
levelsof DX_COUNTRYOFBIRTH, local(levels)


display "`r(levels)'"
cap drop country
 gen country = .
local i = 0
foreach l of local levels {
	local i = `i' + 1
    qui replace country = `l' in `i'
    }
	
label define country ///
          3 "ALBANIA" ///
         10 "BANGLADESH" ///
         16 "BRAZIL" ///
         23 "CHINA"  ///
         35 "EGYPT"  ///
         40 "PHILIPPINES" ///
         42 "FRANCE" ///
         45 "GERMANY" ///
         46 "GHANA" ///
         53 "INDIA" ///
         60 "ITALY" ///
         74 "MOROCCO" ///
         76 "MOLDAVIA" ///
         81 "NIGERIA" ///
         84 "PAKISTAN" ///
         85 "PERU'" ///
         86 "POLAND" ///
         87 "UNITED KINGDOM" ///
         91 "ROMANIA" ///
         96 "SENEGAL" ///
        106 "SRI LANKA" ///
        111 "SWITZERLAND" ///
        118 "TUNISIA" ///
        120 "UKRAINE", modify
		
label values country country


sort mean
list country mean in 1/24, noobs sep(0)
list country in 1/24, noobs sep(0)



cap drop rankb
egen rankb = rank(mean), unique
serrbar mean sd rankb, scale(1.96) ///
xlab(1(1)24) ///
title() xtitle(Rank) ytitle(Country of Birth effects) legend(off) yline(0)

cap drop pr ll ul
gen pr = exp(mean) / (1 + exp(mean)) * 100
gen ll = exp(lb95) / (1 + exp(lb95)) * 100
gen ul = exp(ub95) / (1 + exp(ub95)) * 100

cap drop ctn
gen ctn = _n
drop if ctn > 24


// make the plot
local eu = pr[4]

twoway ///
 scatter ctn pr, msymbol(circle) msize(*0.8) mcolor(navy) mfcolor(%30) || ///
 rcap ll ul ctn, horizontal color(navy) || ///
 ,  ///
  xline(`eu', lcolor(maroon) lwidth(mthick) lpattern(dash)) ///
  xtitle("(%)", size(*1.2)) xscale(range(20 100) titlegap(2)) ///
  xlabel(20(10)100, labsize(*0.8) grid) ///
  ylabel(1  "Poland" 2 "Switzerland" 3 "Sri Lanka" 4 "Italy" 5 "France" ///
  6 "Romania" 7 "Uinited Kingdom" 8 "Brazil" 9 "Moldavia" 10 "Germany" /// 
  11 "Ghana" 12 "Ukraina" 13 "Peru'" 14 "Nigeria" 15 "India" 16 "Albania" ///
  17 "Tunisia" 18 "Philippines" 19 "Pakistan" 20 "Senegal" 21 "Egypt" ///
  22 "Morocco" 23 "Bangladesh" 24 "China" /// 
  , angle(horizontal) labsize(*.8) notick) ///
  yscale( range(0 25) reverse noline) ytitle(" ") ///
  legend(off)  scheme(s1mono)  /// 
  graphregion(color(white)) bgcolor(white)  ///
  plotregion(style(none)) ///
  title("Refusals to donation by Country of Birth", box bexpand size(*0.8))

graph export prop_opp_by_country_of_birth.png, replace
graph export prop_opp_by_country_of_birth.pdf, replace
graph export prop_opp_by_country_of_birth.tif, replace

// save data used for the plot
save data_for_plot_country_of_birth, replace
////////////////////////////////////////////////////////////////////////////////
**# START BAYESAIN MODEL COUNTRY  Two-way crossed random effects logistic reg.
////////////////////////////////////////////////////////////////////////////////

