/*
*****************************************************************************
*                  Political Cycle and GDP Manipulation                     *
*                                  Master                                   *
*****************************************************************************
 File Name: Master.do
 Description:  This program records all of the programs that need to be run of this paper.
 Created By:  Jiayuan Li; Zhibin Chen
 Created on:  August 2024
 Last modified:  September 2025
*/

clear all
set more off, permanent

/*Before running code, set the path first */
cd "Set your path here"             /*like D:\Working\*/
*use data
	use "main",clear
    
*****************************************************************************
*                            Descriptive Statistics                         *
*****************************************************************************
sum cheating_gdp
hist cheating_gdp
// Run mixed-effects model
mixed cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank mayor_lastyear secretary_lastyear Industry3 urbanization population_density aqi Temperature Precipitation Sunshine Target_Attainment_Rate || provinceid: || citycode:, reml 
// Predict random effects
predict re_provinceid re_citycode, reffects
// Kernel density plots with normal overlay
kdensity re_provinceid, normal title("Provincial Random Effects Distribution")
kdensity re_citycode, normal title("City Random Effects Distribution")
// Q-Q plots for normality assessment
qnorm re_provinceid, title("Q-Q Plot: Provincial Random Effects")
qnorm re_citycode, title("Q-Q Plot: City Random Effects")
// Shapiro-Wilk normality tests
swilk re_provinceid
swilk re_citycode
// Detailed descriptive statistics
summarize re_provinceid, detail
summarize re_citycode, detail
// Correlation between random effects
correlate re_provinceid re_citycode
// Scatter plot of random effects
scatter re_citycode re_provinceid, title("City vs Provincial Random Effects")
// Identify extreme values - highest provincial random effects
gsort -re_provinceid
list provinceid re_provinceid in 1/5
// Identify extreme values - highest city random effects  
gsort -re_citycode
list citycode re_citycode in 1/5


**Likelihood Ratio (LR) Test, Wald chi2 and ICC for Both Models
*（1）null model
mixed cheating_gdp || provinceid: || citycode:, reml
estat icc
estimates store null_model
*（2）baseline model: with controls
mixed cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank  mayor_lastyear secretary_lastyear Industry3 urbanization  population_density  aqi Temperature Precipitation Sunshine Target_Attainment_Rate || provinceid: || citycode:, reml 
gen byte insample = e(sample)
estat icc
estimates store full_model
estat ic //Information Criteria
mixed cheating_gdp || provinceid: || citycode: if insample == 1, reml 
estat ic
**Deviance
mixed cheating_gdp quarter4 || provinceid: || citycode: if insample == 1, reml
mixed cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank  mayor_lastyear secretary_lastyear Industry3 urbanization  population_density  aqi Temperature Precipitation Sunshine Target_Attainment_Rate || provinceid: || citycode:, reml 

**Descriptive Statistics
tabstat ln_gdp lntrue_gdp cheating_gdp quarter4 market fiscal_autonomy Provincial_Secretary target_city rank_gdpr_3q Industry3 mayor_age secretary_age mayor_lastyear secretary_lastyear Rank  population_density  urbanization  aqi Temperature Precipitation Sunshine Target_Attainment_Rate,s( mean sd min max) c(s) f(%10.2f)	

*****************************************************************************
*                              Baseline Regression                          *
*****************************************************************************
**Controls
* Performance pressure variables
global controls_performance target_city rank_gdpr_3q
* Political incentive variables
global controls_political mayor_age secretary_age mayor_lastyear secretary_lastyear Rank
* Structural variables affecting GDP-luminosity relationship
global controls_structure Industry3 urbanization population_density
* Environmental variables affecting light readings
global controls_enviro aqi Temperature Precipitation Sunshine
* Main global for all controls
global all_controls $controls_performance $controls_political $controls_structure $controls_enviro	
	
** Table 1: The Impact of Political Cycle on GDP Manipulation

* Model (1) ML（no controls）
mixed cheating_gdp quarter4 || provinceid: || citycode:, reml
r2_nakagawa
eststo maincheating

* Model (2) ML (with controls)
mixed cheating_gdp quarter4 $all_controls || provinceid: || citycode:, reml
r2_nakagawa
eststo withcontrols

* Model 3 employs multiple imputation to address missing data. Because the associated data file is large (~1.02 GB), it is available from the corresponding author upon reasonable request.
* Model (4) Fixed Effects with Robust SE, including diagnostic tests
xtset citycode timequarter  // Make sure you have set the panel structure properly
xtreg cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank mayor_lastyear secretary_lastyear Industry3 urbanization population_density aqi Temperature Precipitation Sunshine, fe
eststo FE
xtreg cheating_gdp quarter4 $all_controls
eststo RE
hausman FE RE  // Hausman test to compare FE and RE models
xtreg cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank  mayor_lastyear secretary_lastyear Industry3 urbanization  population_density  aqi Temperature Precipitation Sunshine i.year
testparm i.year // Test for year fixed effects
xtreg cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank mayor_lastyear secretary_lastyear Industry3 urbanization population_density aqi Temperature Precipitation Sunshine, fe
xttest3          // Test for heteroskedasticity
xtserial cheating_gdp quarter4 $all_controls // Test for serial autocorrelation
// Final FE model for the table
xtreg cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank  mayor_lastyear secretary_lastyear Industry3 urbanization  population_density  aqi Temperature Precipitation Sunshine, fe r
eststo fer

* Model (5) Two-Way Fixed Effects (TWFE)
xtreg cheating_gdp quarter4 target_city rank_gdpr_3q mayor_age secretary_age Rank  mayor_lastyear secretary_lastyear Industry3 urbanization  population_density  aqi Temperature Precipitation Sunshine i.year, fe
eststo towwfe

* Model (6) GMM with post-estimation for Pseudo R-squared
xtabond cheating_gdp L.cheating_gdp quarter4 $all_controls, lags(1) robust
predict double yhat, xb
corr yhat cheating_gdp if e(sample)
display "Pseudo R2 = " r(rho)^2
eststo gmm
esttab  maincheating withcontrols fer towwfe gmm using results.rtf, ///
    append star(* 0.10 ** 0.05 *** 0.01) staraux r2 nogaps ///
     mtitles("ML" "ML" "FE(r)" "TWFE" "GMM") ///
    title("Table 1 Baseline Regression") ///
    b(%9.3f) se(%9.3f) ///
    cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par([ ])))	

/*****************************************************************************
*                              Mechanism Analysis                          *
*****************************************************************************/
** Table 2: Moderating Influences of Fiscal Autonomy

* Model (1) Interaction with Municipal_fiscal (City-level)
* Corrected: now includes the 'urbanization' control via the macro
mixed cheating_gdp i.quarter4##c.Municipal_fiscal $all_controls || provinceid: || citycode:, reml
r2_nakagawa
eststo mfiscal

* Model (2) Interaction with fiscal_autonomy (Provincial-level)
* Corrected: now includes the 'urbanization' control via the macro
mixed cheating_gdp i.quarter4##c.fiscal_autonomy $all_controls || provinceid: || citycode:, reml
r2_nakagawa
eststo fiscal	
esttab  mfiscal fiscal  using moderator.rtf, ///
    append star(* 0.10 ** 0.05 *** 0.01) staraux r2 nogaps ///
    title("Table 2 Moderating Influences of  Fiscal Autonomy") ///
    b(%9.3f) se(%9.3f) ///
    cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par([ ])))

** Table 3: Moderating Influences of Marketization (using renamed variables)

* Model (1) Interaction with overall marketization index
* Corrected: now includes the 'urbanization' control via the macro
mixed cheating_gdp i.quarter4##c.market $all_controls || provinceid: || citycode:, reml
r2_nakagawa
eststo market1	

* Models (2)-(6) Interactions with marketization sub-indices
* Corrected: all models now include the 'urbanization' control via the macro
mixed cheating_gdp i.quarter4##c.market_gov_relation $all_controls || provinceid: || citycode:, reml
eststo market2
mixed cheating_gdp i.quarter4##c.nsoe_dev $all_controls || provinceid: || citycode:, reml
eststo market3
mixed cheating_gdp i.quarter4##c.product_market_dev $all_controls || provinceid: || citycode:, reml
eststo market4
mixed cheating_gdp i.quarter4##c.factor_market_dev $all_controls || provinceid: || citycode:, reml
eststo market5
mixed cheating_gdp i.quarter4##c.legal_env_dev $all_controls || provinceid: || citycode:, reml
eststo market6
esttab market1 market2 market3 market4 market5 market6 using moderator.rtf, ///
    append star(* 0.10 ** 0.05 *** 0.01) staraux r2 nogaps ///
     title("Table 3 Moderating Influences of Marketization") ///
    b(%9.3f) se(%9.3f) ///
    cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par([ ])))	


	
/*****************************************************************************
*                           Strategic Manipulation                          *
*****************************************************************************/   
** Table 4: Evidence of Strategic Manipulation

* Model (1) Interaction with Provincial_Secretary's final term
* This model was correct, no variables were missing
mixed cheating_gdp i.quarter4##i.Provincial_Secretary $all_controls || provinceid: || citycode:, reml
r2_nakagawa
eststo PSecretary

* Model (2) Interaction with Target Attainment Rate
* Corrected: now includes the 'urbanization' control via the macro
mixed cheating_gdp c.Target_Attainment_Rate##i.quarter4 $all_controls || provinceid: || citycode:, reml
r2_nakagawa
eststo targetattainment

* Model (3) Variation across all quarters
* This model was correct, no variables were missing
mixed cheating_gdp i.quarter $all_controls || provinceid: || citycode:, reml	
r2_nakagawa
eststo allquarters	
esttab PSecretary targetattainment allquarters using Strategic_Manipulation.rtf, ///
    append star(* 0.10 ** 0.05 *** 0.01) staraux r2 nogaps ///
     mtitles("FinalTerm" "Target Attainment Rate" "Quarter") ///
    title("Table 5 Evidence of Strategic Manipulation") ///
    b(%9.3f) se(%9.3f) ///
    cells(b(star fmt(%9.3f)) se(fmt(%9.3f) par([ ])))

	
/*****************************************************************************
*                                 Figures                                    *
*****************************************************************************/ 
***Figure 1: See the R file "figure1_adjusted_quarterly_gdp.R" for details.

***Figure 2 Marginal Effects plot for interaction term
**Figure 1.1 Interaction term: fiscal_autonomy
* 1. Run mixed-effects model
mixed cheating_gdp i.quarter4##c.fiscal_autonomy $all_controls || provinceid: || citycode:, reml
* 2. Calculate marginal effects
margins, dydx(quarter4) at(fiscal_autonomy=(0(0.1)1)) 
* 3. Generate marginal effects plot
marginsplot, ///
    title("Conditional on Fiscal Autonomy", size(medium)) ///
    ytitle("Marginal Effect of Q4") ///
    xtitle("Fiscal Autonomy") ///
    xlabel(0(0.1)1) ///
    ylabel(, angle(horizontal)) ///
    plot1opts(lcolor(navy) lwidth(medthick)) ///
    ci1opts(lpattern(dash) lcolor(navy%50)) ///
    yline(0, lpattern(dash) lcolor(gray)) 
* 4. Optional: save the graph
graph save q4_marginal_effects.gph, replace

****Figure 1.1 Interaction term: market 
* 1. Run mixed-effects model
mixed cheating_gdp i.quarter4##c.market $all_controls || provinceid: || citycode:, reml
r2_nakagawa
* 2. Calculate marginal effects
margins, dydx(quarter4) at(market=(0(1)10))
* 3. Generate marginal effects plot
marginsplot, ///
    title("Conditional on Marketization Index", size(medium)) ///
    ytitle("Marginal Effect of Q4") ///
    xtitle("Marketization Index") ///
    xlabel(0(1)10) ///
    ylabel(, angle(horizontal)) ///
    plot1opts(lcolor(navy) lwidth(medthick)) ///
    ci1opts(lpattern(dash) lcolor(navy%50)) ///
    yline(0, lpattern(dash) lcolor(gray))    
* 4. Optional: save the graph
graph save q4_market_effects.gph, replace

** Graph Combine
graph use q4_marginal_effects.gph
graph use q4_market_effects.gph
graph combine q4_marginal_effects.gph q4_market_effects.gph, ///
    title("Marginal Effects of Fourth Quarter on GDP Manipulation") ///
    b1("Marginal Effects")  xsize(12) ysize(8)
graph export "combined_graphs.png", replace width(3000) as(png)

***Figure 3 Marginal Effect of Q4 on GDP Manipulation across Levels of Target Attainment
summarize Target_Attainment_Rate
mixed cheating_gdp c.Target_Attainment_Rate##i.quarter4 $all_controls || provinceid: || citycode:, reml
margins, dydx(quarter4) ///
    at(Target_Attainment_Rate=(0.55(0.005)0.76)) ///
    
marginsplot, xdimension(Target_Attainment_Rate) ///
    title("Marginal Effect of Q4 on GDP Manipulation", size(medium)) ///
    ytitle("Marginal Effect of Q4") ///
    xtitle("Target Attainment Rate") ///
	ylabel(0(0.02)0.08, angle(h) format(%3.2f)) /// 
    xlabel(0.55(0.05)0.75, format(%3.2f)) ///
	legend(off) ///
    recast(line) recastci(rarea) ///
    ciopts(color(gs12%50)) ///
    plotopts(lcolor(navy) lwidth(medthick)) ///            
    addplot(rug Target_Attainment_Rate, yaxis(1) lowerc(0) upperc(0.005) lcolor(gs8))


***Figure 4: See the R file "figure 4_Adjusted_Effect_of_Quarter_on_GDP_Manipulation.R" for details.

