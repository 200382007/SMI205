/*
STATA Replication Code for

Pride amid Prejudice: The Impact of LGBT+ Rights Activism in a Socially Conservative Society
*/


*the dataset: “Sarajevo Pride_replication data.dta”

************************************************************************************
*                             Main Body of the Paper                               *
************************************************************************************

*Table 1
sum Men if treatment==0
sum Men if treatment==1
ksmirnov Men, by(treatment)
sum age if treatment==0
sum age if treatment==1
ksmirnov age, by(treatment)
sum Education if treatment==0
sum Education if treatment==1
ksmirnov Education, by(treatment)
sum rural if treatment==0
sum rural if treatment==1
ksmirnov rural, by(treatment)
sum unemployed if treatment==0
sum unemployed if treatment==1
ksmirnov unemployed, by(treatment)
sum Serb if treatment==0
sum Serb if treatment==1
ksmirnov Serb, by(treatment)
sum Bosniak if treatment==0
sum Bosniak if treatment==1
ksmirnov Bosniak, by(treatment)
sum Croat if treatment==0
sum Croat if treatment==1
ksmirnov Croat, by(treatment)

*For Figures 1 and 2, see the "Sarajevo Pride Replication Instructions" file

*Table 2 
reg supportpride treatment##sarajevocity, robust
reg supportpride treatment##sarajevocity, cluster(municipality)
xtset sms_id
xtreg supportpride treatment if panel==1 & wave3==0, fe robust
xtreg supportpride treatment if panel==1, fe robust

*Figure 3 
cem age rural, treatment(treatment)
reg supportpride treatment##sarajevocity alphaclosecontact heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)
coefplot, xline(0) nolabel drop(_cons) scheme(s1mono) title("Figure 3. Pride Effects on LGBT+ Activism, Extended Controls", size(medlarge)) note("N=2190, R2=0.48, OLS regression, robust standard errors clustered by municipality")

*Figure 4
reg AttendPride GayActivistTreatment treatment##sarajevocity, cluster(municipality)
estimates store a1
reg ProtestPride HomophobeTreatment treatment##sarajevocity, cluster(municipality)
estimates store a2
coefplot a1 || a2, xline(0) nolabel drop(_cons) scheme(s1mono) 
*title("Figure 4. Mobilization for and Counter-Mobilization against LGBT+ Activism", size(medlarge)) 
*note("Model 1 N=2560, Model 2 N=2518, OLS regression, robust standard errors clustered by municipality")

*Figure 5 
cibar dgprogay, over2(sarajevocity) over1(treatment) graphopts(scheme(s1mono) name(G1, replace) ytitle("Bosnian Marks Allocated"))

twoway  (histogram dgprogay if sarajevocity==1 & treatment==0, percent ylabel(0(20)80) title("Inside Sarajevo, Histogram of Allocated Marks")) (histogram dgprogay if sarajevocity==1 & treatment==1, percent ylabel(0(20)80) fcolor(none) lcolor(black)), scheme(s1mono) name(G2, replace)

twoway  (histogram dgprogay if sarajevocity==0 & treatment==0, percent ylabel(0(20)80) title("Outside Sarajevo, Histogram of Allocated Marks")) (histogram dgprogay if sarajevocity==0 & treatment==1, percent ylabel(0(20)80) fcolor(none) lcolor(black)), scheme(s1mono) name(G3, replace)

#delimit ;
graph combine G1 G2 G3, rows(3) scheme(s1mono) ysize(9) xsize(5)
title("Figure 5. Dictator Giving to Pro-LGBT+ Advocacy Group", size(medsmall))
;

*Figure 6
reg dgprogay treatment##sarajevocity, cluster(municipality)
coefplot, xline(0) nolabel scheme(s1mono) title("Figure 6. Mobilization of Resources for LGBT+ Activism," "Bosnian Marks Allocated during the Dictator Game", size(medlarge)) note("N = 2685, Adj. R2 = 0.22, OLS Regression, robust standard errors clustered by municipality")


************************************************************************************
*                                 Online Appendix                                  *
************************************************************************************

*Sampling Locations in Sarajevo by Municipality
tab municipality if sarajevocity==1 & panel==1
tab municipality if sarajevocity==1 & panel==0

*Summary Statistics for the full dataset, nationwide and panel
sum supportpride gayrights metgay closetogay knewvictim heardofpride prideknowledge AttendPride ProtestPride dgprogay age Education ethnocentric bosniakfavorable croatfavorable serbfavorable voteeu religious Men unemployed rural Croat Serb Bosniak
*Summary Statistics for the nationwide dataset
sum supportpride gayrights metgay closetogay knewvictim heardofpride prideknowledge AttendPride ProtestPride dgprogay age Education ethnocentric bosniakfavorable croatfavorable serbfavorable voteeu religious Men unemployed rural Croat Serb Bosniak if panel==0
*Summary Statistics for the online panel dataset
sum supportpride gayrights metgay closetogay knewvictim heardofpride prideknowledge AttendPride ProtestPride dgprogay age Education ethnocentric bosniakfavorable croatfavorable serbfavorable voteeu religious Men unemployed rural Croat Serb Bosniak if panel==1

*Factor Analysis of LGBT+ contact, support Index
alpha gayrights metgay closetogay knewvictim, gen(alphaclosecontact)
factor gayrights metgay closetogay knewvictim

*Manuscript Figure 1. Pre-Treatment and Post-Treatment
twoway  (histogram supportpride if sarajevocity==0 & treatment==0, discrete percent) (histogram supportpride if sarajevocity==1 & treatment==0,discrete percent fcolor(none) lcolor(black))
twoway  (histogram supportpride if sarajevocity==0 & treatment==1, discrete percent) (histogram supportpride if sarajevocity==1 & treatment==1,discrete percent fcolor(none) lcolor(black))

*Appendix Table 1. Effect of the Pride March on Support for LGBT+ Activism (Panel + Sarajevo nationwide)
reg supportpride treatment##sarajevocity
reg supportpride treatment##sarajevocity, robust
reg supportpride treatment##sarajevocity, cluster(municipality)

*Appendix Table 2. Effect of the Pride March on Support for LGBT+ Activism (Panel - Sarajevo nationwide)
reg supportpride treatment##panel if sarajevosurvey~=1
reg supportpride treatment##panel if sarajevosurvey~=1, robust
reg supportpride treatment##panel if sarajevosurvey~=1, cluster(municipality)

*Appendix Table 3. Effect of the Pride March on Support for LGBT+ Activism (Ordered Probit)
oprobit supportpride treatment##panel
oprobit supportpride treatment##panel, robust
oprobit supportpride treatment##panel, cluster(municipality)

*Appendix Table 4. Effect of the Pride March on Support for LGBT+ Activism (Panel + Sarajevo nationwide, Ordered Probit) 
oprobit supportpride treatment##sarajevocity
oprobit supportpride treatment##sarajevocity, robust
oprobit supportpride treatment##sarajevocity, cluster(municipality)

*Appendix Figure 1 (run in a separate dofile due to the <collapse> command)
/*
gen interaction = treatment*sarajevocity

oprobit supportpride treatment sarajevocity interaction, robust

gen placeholder = 0

keep if _n==1
keep placeholder

		matrix def b = e(b)
		matrix def V = e(V)
		mat list b
		mat list V

set more off
	save PrideModelIVs.dta, replace
		
	*program drop PrideRegression
program define PrideRegression
	use PrideModelIVs.dta, clear
	
	drawnorm b1-b6, means(b) cov(V)
	
	gen pSupportSarajevoPost = b1*1 + b2*1 + b3*1*1 
	gen pSupportSarajevoPre = b1*0 + b2*1 + b3*0*1 
	gen pSupportBosniaPost = b1*1 + b2*0 + b3*1*0 
	gen pSupportBosniaPre = b1*0 + b2*0 + b3*0*0 
	
	rename b4 a1
	rename b5 a2
	rename b6 a3 
	
	gen SupportSarajevoPost =  1 - normal(a3 - pSupportSarajevoPost) 
	gen SupportSarajevoPre = 1 - normal(a3 - pSupportSarajevoPre)  
	gen SupportBosniaPost = 1 - normal(a3 - pSupportBosniaPost) 
	gen SupportBosniaPre = 1 - normal(a3 - pSupportBosniaPre)  
	
append using PrideModel.dta
save PrideModel.dta, replace
			
			end
							
			clear
			save PrideModel.dta, replace emptyok
	
			set more off
			//simulating
			simulate, reps(1000):  PrideRegression

			use PrideModel.dta, clear
			
			browse
set more off

#delimit ; 
collapse (mean)
SupportSarajevoPost SupportSarajevoPre SupportBosniaPost SupportBosniaPre 
(sd)
SupportSarajevoPost_sd=SupportSarajevoPost SupportSarajevoPre_sd=SupportSarajevoPre 
SupportBosniaPost_sd=SupportBosniaPost SupportBosniaPre_sd=SupportBosniaPre 
 ;
#delimit cr 

browse

gen SupportSarajevoPost_hi=SupportSarajevoPost + 1.96*SupportSarajevoPost_sd
gen SupportSarajevoPost_lo=SupportSarajevoPost - 1.96*SupportSarajevoPost_sd

gen SupportSarajevoPre_hi=SupportSarajevoPre + 1.96*SupportSarajevoPre_sd
gen SupportSarajevoPre_lo=SupportSarajevoPre - 1.96*SupportSarajevoPre_sd

gen SupportBosniaPost_hi=SupportBosniaPost + 1.96*SupportBosniaPost_sd
gen SupportBosniaPost_lo=SupportBosniaPost - 1.96*SupportBosniaPost_sd

gen SupportBosniaPre_hi=SupportBosniaPre + 1.96*SupportBosniaPre_sd
gen SupportBosniaPre_lo=SupportBosniaPre - 1.96*SupportBosniaPre_sd

gen axis_1 = 1
gen axis_2 = 2
gen axis_3 = 3
gen axis_4 = 4

lab def Prideplot 1 "Bosnia Pre-Pride" 2 "Bosnia Post-Pride" 3 "Sarajevo Pre-Pride" 4 "Sarajevo Post-Pride" 
label val axis_1 Prideplot 

#delimit ;
twoway  (bar SupportBosniaPre axis_1, 
			color(black) barwidth(.5))
		(rcap SupportBosniaPre_lo SupportBosniaPre_hi axis_1,
			lcolor(gs10))
		(bar SupportBosniaPost axis_2, 
			color(black) barwidth(.5))
		(rcap SupportBosniaPost_lo SupportBosniaPost_hi axis_2,
			lcolor(gs10))
		(bar SupportSarajevoPre axis_3, 
			color(black) barwidth(.5))
		(rcap SupportSarajevoPre_lo SupportSarajevoPre_hi axis_3,
			lcolor(gs10))
		(bar SupportSarajevoPost axis_4, 
			color(black) barwidth(.5))
		(rcap SupportSarajevoPost_lo SupportSarajevoPost_hi axis_4,
			lcolor(gs10))
		,
legend(off)
ylabel(0(.1).4)
title("Appendix Figure 1: Probability of Strongly Supporting Sarajevo Pride" "Representing the Appendix Table 4 Model with Robust Standard Errors", size(med))
ytitle("Pr(Strongly Support Sarajevo Pride)", size(medsmall))
note("Data source: Bosnia surveys (Panel + Sarajevo nationwide)" "Estimates calculated using an ordered probit model", size(med))
xlabel(#3,valuelabel angle(horizontal))
name(Pridegraph, replace)
scheme(s1mono);
#delimit cr
*/

*Appendix Table 5. Effect of the Pride March on Support for LGBT+ Activism (Panel - Sarajevo nationwide)
oprobit supportpride treatment##panel if sarajevosurvey~=1
oprobit supportpride treatment##panel if sarajevosurvey~=1, robust
oprobit supportpride treatment##panel if sarajevosurvey~=1, cluster(municipality)

*Appendix Table 6. Effect of the Pride March on Support for LGBT+ Activism (T-tests, Panel and Sarajevo Only)
xtreg supportpride treatment if panel==1 & wave3==0, fe robust
xtreg supportpride treatment if panel==1, fe robust
ttest supportpride if sarajevocity==1, by(treatment) unequal

*Appendix Table 7. Effect of the Pride March on Support for LGBT+ Activism (Panel fixed effects)
xtreg supportpride treatment if panel==1, fe
xtreg supportpride treatment if panel==1, fe robust
xtreg supportpride treatment if panel==1, fe cluster(municipality)

*Appendix Table 8. Effect of the Pride March on Support for LGBT+ Activism (Panel + Nationwide)
reg supportpride treatment if sarajevocity==1
reg supportpride treatment if sarajevocity==1, robust
reg supportpride treatment if sarajevocity==1, cluster(municipality)
reg supportpride treatment if sarajevocity==1 & wave3==0, cluster(municipality)

*Appendix Table 9. Pride Effects on LGBT+ Activism, Extended Controls (OLS)
cem age rural, treatment(treatment)
reg supportpride treatment##sarajevocity alphaclosecontact heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)

*Appendix Table 10. Treatment Effects Estimation
diff supportpride, period(treatment) treated(sarajevocity) cluster(municipality)
diff supportpride [pweight = cem_weight], period(treatment) treated(sarajevocity) cov(alphaclosecontact heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed)  cluster(municipality)
teffects ipwra (supportpride  age rural) (treatment  age rural) if sarajevocity==1
teffects psmatch (supportpride)(treatment  age rural) if sarajevocity==1, vce(iid)

*Appendix Table 11. Pride Effects on LGBT+ Activism, Extended Controls (Ordered Probit)
oprobit supportpride treatment##sarajevocity alphaclosecontact heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)

*Appendix Table 12. Pride Effects on LGBT+ Activism, Extended Controls (OLS inclusion of additional municipal level controls for war-time conflict and ethnic vote share from Hadzic 2017)
reg supportpride treatment##sarajevocity  ethnic_vote_share log_casualty refugees [pweight = cem_weight], cluster(municipality)
reg supportpride treatment##sarajevocity  ethnic_vote_share log_casualty refugees  alphaclosecontact heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)

*Appendix Table 13. Effect of the Pride March on Support for LGBT+ Activism (Panel Only, Panel Fixed Effects)
xtlogit treatment gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious, fe
xtreg supportpride treatment gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious, fe
xtreg supportpride treatment gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious if wave3==0, fe

*Appendix Table 14. Balance Tests for Randomization to Treatment and Control (Logit Regression)
logit GayActivistTreatment Serb Bosniak Men age Education rural unemployed, robust
logit HomophobeTreatment Serb Bosniak Men age Education rural unemployed, robust

*Appendix Table 15. Mobilization for and Counter-Mobilization against LGBT+ rights (OLS, Full sample)
reg AttendPride GayActivistTreatment treatment##sarajevocity, cluster(municipality)
xtset sms_id
xtreg AttendPride GayActivistTreatment treatment##sarajevocity, fe robust
reg ProtestPride HomophobeTreatment treatment##sarajevocity, cluster(municipality)
xtreg ProtestPride HomophobeTreatment treatment##sarajevocity, fe robust

*Appendix Table 16. Mobilization for and Counter-Mobilization against LGBT+ rights (OLS, full sample)
reg AttendPride GayActivistTreatment treatment##sarajevocity, cluster(municipality)
reg AttendPride GayActivistTreatment treatment##sarajevocity gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, cluster(municipality)
reg ProtestPride HomophobeTreatment treatment##sarajevocity, cluster(municipality)
reg ProtestPride HomophobeTreatment treatment##sarajevocity gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, cluster(municipality)

*Appendix Table 17. Mobilization and Counter Mobilization (tobit regression)
tobit AttendPride GayActivistTreatment treatment##sarajevocity, ll ul cluster(municipality)
tobit AttendPride GayActivistTreatment treatment##sarajevocity gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, ll ul cluster(municipality)
tobit ProtestPride HomophobeTreatment treatment##sarajevocity, ll ul cluster(municipality)
tobit ProtestPride HomophobeTreatment treatment##sarajevocity gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, ll ul cluster(municipality)

*Appendix Table 18. Mobilization and Counter Mobilization (Tobit regression, online panel)
tobit AttendPride GayActivistTreatment treatment##panel, ll ul cluster(municipality)
tobit AttendPride GayActivistTreatment treatment##panel gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, ll ul cluster(municipality)
tobit ProtestPride HomophobeTreatment treatment##panel, ll ul cluster(municipality)
tobit ProtestPride HomophobeTreatment treatment##panel gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, ll ul cluster(municipality)

*Appendix Table 19. Mobilization and Counter Mobilization (OLS, Sarajevo Online panel, panel fixed effects)
xtset sms_id
xtreg AttendPride GayActivistTreatment treatment, fe
xtreg AttendPride GayActivistTreatment treatment gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious, fe
xtreg ProtestPride HomophobeTreatment treatment, fe
xtreg ProtestPride HomophobeTreatment treatment gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious, fe

*Robustness Checks for Manuscript Table 5.
cibar dgprogay, over2(sarajevocity) over1(treatment)
twoway  (histogram dgprogay if sarajevocity==1 & treatment==0) (histogram dgprogay if sarajevocity==1 & treatment==1, fcolor(none) lcolor(black))
twoway  (histogram dgprogay if sarajevocity==0 & treatment==0) (histogram dgprogay if sarajevocity==0 & treatment==1, fcolor(none) lcolor(black))
twoway  (kdensity dgprogay if sarajevocity==1 & treatment==0) (kdensity dgprogay if sarajevocity==0 & treatment==0)  (kdensity dgprogay if sarajevocity==1 & treatment==1)  (kdensity dgprogay if sarajevocity==0 & treatment==1, fcolor(none) lcolor(black))
twoway  (kdensity dgprogay if sarajevocity==1 & treatment==0) (kdensity dgprogay if sarajevocity==1 & treatment==1, fcolor(none) lcolor(black))
twoway  (kdensity dgprogay if sarajevocity==0 & treatment==0) (kdensity dgprogay if sarajevocity==0 & treatment==1, fcolor(none) lcolor(black))

*Appendix Table 20. Mobilization of Resources for LGBT+ Activism (OLS)
reg dgprogay treatment##sarajevocity, cluster(municipality)
reg dgprogay treatment##sarajevocity gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu Men age Education ethnocentric religious Bosniak Croat Serb rural unemployed, cluster(municipality)

*Appendix Table 21. Anti-LGBT Bias in Dictator Giving (logit, Sarajevo nationwide + online panel)
logit dgbias treatment##sarajevocity, cluster(municipality)
logit dgbias treatment##sarajevocity gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education  rural unemployed, cluster(municipality)

*Appendix Table 22. Dictator Giving for Sarajevo Panel Only
reg dgprogay treatment##panel, cluster(municipality)
reg dgprogay treatment##panel gayrights closetogay metgay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, cluster(municipality)
reg dgprogay treatment if panel==1, cluster(municipality)

*Appendix Table 23. Pride Support vs. LGBT+ Support (Logit Regression)
/*gen rightspride = 1 if supportpride>2 & gayrights>5
replace rightspride = 0 if rightspride==.
replace rightspride = . if supportpride==.
replace rightspride = . if gayrights==.*/
logit rightspride treatment##sarajevocity metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)
/*gen rightsnopride = 1 if gayrights>5 & supportpride<3
replace rightsnopride =0 if rightsnopride==.
replace rightsnopride =. if gayrights==.
replace rightsnopride =. if supportpride==.*/
logit rightsnopride treatment##sarajevocity metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)
/*gen rightsyesnopride = 1 if rightspride==1
replace rightsyesnopride = 0 if rightsnopride==1*/
logit rightsyesnopride treatment##sarajevocity metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)

*Ethnicity and Pride Support 
cibar supportpride, over1(ethnicity)
histogram ethnicity, by (sarajevocity) discrete percent addlabels

*Appendix Table 24. Impact of Ethnicity on Pride Support (OLS Regression)
reg supportpride treatment##sarajevocity, robust
reg supportpride treatment##sarajevocity i.ethnicity, robust

*Ethnocentrism and Pride Support/Opposition
cibar ethnocentric, over2(sarajevocity) over1(treatment)

*Appendix Table 25. Impact of Ethnicity on Pride Support (OLS Regression)
reg supportpride treatment##sarajevocity, robust
reg supportpride treatment##sarajevocity i.ethnicity, robust
reg supportpride treatment##sarajevocity i.ethnicity ethnocentric, robust

*Appendix Table 26 : Unpacking attitudes regarding LGBT+ rights by ethnicity with 95 percent CIs in parentheses
mean supportpride, over(ethnicity)
mean gayrights, over(ethnicity)
mean closetogay, over(ethnicity)
mean AttendPride, over(ethnicity)
mean ProtestPride, over(ethnicity)
recode metgay 4=1 3=1 2=0 1=0, gen(knowinggayperson)
proportion knowinggayperson, over(ethnicity)

*Municipal-level Ethnic voting and Ethnic fractionalization on Pride Support
cibar ethnic_vote_share , over1(sarajevocity)
cibar hhi_sb, over1(sarajevocity)
cibar hhi_cb, over1(sarajevocity)

*Appendix Table 27. Municipal Ethnic Vote Share, Ethnic Fractionalization, and Pride Support (OLS Regression)
reg supportpride treatment##sarajevocity Bosniak Croat Serb ethnocentric ethnic_vote_share, robust
reg supportpride treatment##sarajevocity Bosniak Croat Serb ethnocentric ethnic_vote_share hhi_sb hhi_cb, robust

*Appendix Table 28. Municipal Ethnic Vote Share, Ethnic Fractionalization, and Pride Support (Extended Controls, OLS)
reg supportpride treatment##panel alphaclosecontact heardofpride prideknowledge voteeu ethnocentric ethnic_vote_share hhi_sb hhi_cb religious Bosniak Croat Serb Men age Education rural unemployed, cluster(municipality)
xtset municipality
xtreg supportpride treatment##panel alphaclosecontact heardofpride prideknowledge voteeu ethnocentric ethnic_vote_share hhi_sb hhi_cb religious Bosniak Croat Serb Men age Education rural unemployed, fe cluster(municipality)
xtset sms_id
xtreg supportpride treatment##panel alphaclosecontact heardofpride prideknowledge voteeu ethnocentric ethnic_vote_share hhi_sb hhi_cb religious Bosniak Croat Serb Men age Education rural unemployed, fe cluster(municipality)

*Ethno-federalism and Pride Support
cibar supportpride if entity~=3, over1(treatment) over2(entity)

*Appendix Table 29 : Unpacking attitudes regarding LGBT+ rights by Bosnia’s ethnic entities with 95 percent CIs in parentheses
mean supportpride, over(entity)
mean gayrights, over(entity)
mean closetogay, over(entity)
mean AttendPride, over(entity)
mean ProtestPride, over(entity)
recode metgay 4=1 3=1 2=0 1=0, gen(knowinggayperson2)
proportion knowinggayperson2, over(entity)

*Appendix Table 30. Ethno-federalism and Pride Support (OLS)
reg supportpride treatment##federation if entity~=3, robust
reg supportpride treatment##federation if entity~=3 & sarajevocity==0, robust

*Religiosity and Pride Support 
cibar religious, over1(ethnicity)
cibar supportpride, over1(religious)
cibar religious, over2(sarajevocity) over1(treatment)

*Appendix Table 31. Ethnocentrism, Religiosity and Pride Support
reg supportpride treatment##sarajevocity i.ethnicity ethnocentric religious, robust
reg supportpride treatment##sarajevocity i.ethnicity c.ethnocentric##c.religious, robust

*Urban-Rural Divisions and Pride Support
cibar supportpride, over2(rural) over1(treatment)

*Appendix Table 32. Urban-Rural Population Density and Pride Support (OLS Regression)
reg supportpride treatment##sarajevocity rural, robust
reg supportpride treatment##sarajevocity log_pop_density, robust

*Appendix Table 33. Urban-Rural Population Density and Pride Support (OLS Regression, Extended Controls)
reg supportpride treatment##sarajevocity gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education unemployed rural [pweight = cem_weight], cluster(municipality)
reg supportpride treatment##sarajevocity gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education unemployed log_pop_density [pweight = cem_weight], cluster(municipality)

*Appendix Table 34. Pride Support in Other Urban Locations Outside Sarajevo (OLS)
reg supportpride treatment##urban, robust
reg supportpride treatment##urban if sarajevocity==0, robust
reg supportpride treatment##c.log_pop_density if sarajevocity==0, robust
reg supportpride treatment##tuzla if sarajevocity==0, robust
reg supportpride treatment##banjaluka if sarajevocity==0, robust
graph box supportpride, by(municipality2)

*Appendix Table 35 : Unpacking attitudes regarding LGBT+ rights by rural/urban with 95 percent CIs in parentheses
mean supportpride, over(type)
mean gayrights, over(type)
mean closetogay, over(type)
mean AttendPride, over(type)
mean ProtestPride, over(type)
recode metgay 4=1 3=1 2=0 1=0, gen(knowinggayperson3)
proportion knowinggayperson3, over(type)

*Appendix Table 36 : Unpacking attitudes regarding LGBT+ rights by major city with 95 percent CIs in parentheses
mean supportpride, over(municipality)
mean gayrights, over(municipality)
mean closetogay, over(municipality)
mean AttendPride, over(municipality)
mean ProtestPride, over(municipality)
recode metgay 4=1 3=1 2=0 1=0, gen(knowinggayperson3)
proportion knowinggayperson3, over(municipality)
graph box supportpride if sarajevocity==1, by(municipality)
cibar ethnocentric if municipality3~=0, over1(municipality3)
cibar religious if municipality3~=0, over1(municipality3)
cibar supportpride, over1(municipality3)

*Conflict-related violence and Pride Support
cibar casualty, over1(sarajevocity)

*Appendix Table 37. Conflict-related Violence and Pride Support (OLS Regression)
reg supportpride treatment##sarajevocity log_casualty , robust
reg supportpride treatment##sarajevocity log_casualty, cluster(municipality)
reg supportpride treatment##sarajevocity log_casualty gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)

*Appendix Table 38. Mediation of Pride Awareness, Contact, and Religiosity on Pride Effects (OLS, Logit)
reg supportpride treatment##sarajevocity, cluster(municipality)
reg supportpride treatment##sarajevocity gayrights, cluster(municipality)
reg supportpride treatment##sarajevocity metgay, cluster(municipality)
reg supportpride treatment##sarajevocity closetogay, cluster(municipality)
reg supportpride treatment##sarajevocity knewvictim, cluster(municipality)
reg supportpride treatment##sarajevocity HeardOfPride, cluster(municipality)
reg supportpride treatment##sarajevocity prideknowledge, cluster(municipality)
reg supportpride treatment##sarajevocity voteeu, cluster(municipality)
reg supportpride treatment##sarajevocity ethnocentric, cluster(municipality)
reg supportpride treatment##sarajevocity religious, cluster(municipality)

*Appendix Table 39. Mediation Analysis on Pride Effects (OLS, Logit)
/*To install medeff, medsens commands in Stata, see the following:
st0243_1 from http://www.stata-journal.com/software/sj12-2 */

medeff (regress gayrights treatment) (regress supportpride treatment gayrights) if sarajevocity==1, mediate(gayrights) treat(treatment) sims(500)
medsens (regress gayrights treatment) (regress supportpride treatment gayrights) if sarajevocity==1, mediate(gayrights) treat(treatment) sims(500)

medeff (regress metgay treatment) (regress supportpride treatment metgay) if sarajevocity==1, mediate(metgay) treat(treatment) sims(500)
medsens (regress metgay treatment) (regress supportpride treatment metgay) if sarajevocity==1, mediate(metgay) treat(treatment) sims(500)

medeff (regress closetogay treatment) (regress supportpride treatment closetogay) if sarajevocity==1, mediate(closetogay) treat(treatment) sims(500)
medsens (regress closetogay treatment) (regress supportpride treatment closetogay) if sarajevocity==1, mediate(closetogay) treat(treatment) sims(500)

medeff (regress knewvictim treatment) (regress supportpride treatment knewvictim) if sarajevocity==1, mediate(knewvictim) treat(treatment) sims(500)
medsens (regress knewvictim treatment) (regress supportpride treatment knewvictim) if sarajevocity==1, mediate(knewvictim) treat(treatment) sims(500)

medeff (regress heardofpride treatment) (regress supportpride treatment heardofpride) if sarajevocity==1, mediate(heardofpride) treat(treatment) sims(500)
medsens (regress heardofpride treatment) (regress supportpride treatment heardofpride) if sarajevocity==1, mediate(heardofpride) treat(treatment) sims(500)

medeff (regress prideknowledge treatment) (regress supportpride prideknowledge treatment) if sarajevocity==1, mediate(prideknowledge) treat(treatment) sims(500)
medsens (regress prideknowledge treatment) (regress supportpride prideknowledge treatment) if sarajevocity==1, mediate(prideknowledge) treat(treatment) sims(500)

medeff (regress voteeu treatment) (regress supportpride treatment voteeu) if sarajevocity==1, mediate(voteeu) treat(treatment) sims(500)
medsens (regress voteeu treatment) (regress supportpride treatment voteeu) if sarajevocity==1, mediate(voteeu) treat(treatment) sims(500)

medeff (regress ethnocentric treatment) (regress supportpride treatment ethnocentric) if sarajevocity==1, mediate(ethnocentric) treat(treatment) sims(500)
medsens (regress ethnocentric treatment) (regress supportpride treatment ethnocentric) if sarajevocity==1, mediate(ethnocentric) treat(treatment) sims(500)

medeff (regress religious treatment) (regress supportpride treatment religious) if sarajevocity==1, mediate(religious) treat(treatment) sims(500)
medsens (regress religious treatment) (regress supportpride treatment religious) if sarajevocity==1, mediate(religious) treat(treatment) sims(500)

*Appendix Table 40. Exploring Mediators, Moderators, and Confounders of Pride Effects (OLS, Logit)
reg supportpride gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed if treatment==0, cluster(municipality)
logit panel gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed if treatment==0, cluster(municipality)
logit treatment gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed, cluster(municipality)

*Appendix Table 41. Pride Effects with Extended Controls (OLS)
reg supportpride treatment##panel, cluster(municipality)

cem age rural, treatment(treatment)
reg supportpride treatment##panel gayrights metgay closetogay knewvictim heardofpride prideknowledge voteeu ethnocentric religious Bosniak Croat Serb Men age Education rural unemployed [pweight = cem_weight], cluster(municipality)
