<img width="468" height="644" alt="image" src="https://github.com/user-attachments/assets/17570174-6c40-41bd-b736-ad0b8575a2b2" /># outsourcing_paper_datatset
Dataset of the paper: When Collaboration Reshapes Local Choices: Policy Feedback from Inter-Municipal Cooperation 
[Readme File.docx](https://github.com/user-attachments/files/24173181/Readme.File.docx)
[PAPER REPLICATION FILES.docx](https://github.com/user-attachments/files/24173186/PAPER.REPLICATION.FILES.docx)
[Uploading stata file for replication.do…


*******************************************************
* PAPER REPLICATION FILES
* Read Readme file for detailed instruction comments
*******************************************************


*******************************************************
* HEALTH SECTOR
*******************************************************

*-----------------------------------------------------*
* 0) Preparation
*-----------------------------------------------------*

cd "/XXX"
use "/XXX/paper.dta", clear
set more off
xtset codibge year
outreg, clear


*-----------------------------------------------------*
* 1) - Table 03
*     JOINT RECURSIVE MODEL (BIVARIATE) – biprobit
*   - Equation 1: outsourcing = f(Cons_saude, Years_Cons_Saude, X, Year FE)
*   - Equation 2: Cons_saude  = f(Z, Year FE)
*   - SE clustered by municipality
*   - ATTENTION: include one or more EXCLUDED instruments in Z.
*                E.g.: distcapestd, distcapfed, Capital,
*                density of neighboring consortia, etc.
*-----------------------------------------------------*

biprobit ///
    (outsourcing = Cons_saude Anos_Cons_Saude ///
        age_mayor edu_HighSchool edu_UndergraduateAbove female_mayor second_term ///
        Buroc_estatut Buroc_esc Buroc_capita ///
        ltotal_rec indicador_2 lGasto_saude_capita direita dummy_del_saude quant_reuniao_saude health_salient ///
        lpop lpib i.year) ///
    (Cons_saude = ///
        lpop lpib Capital distcapestd distcapfed ///
        age_mayor female_mayor ///
        i.year), vce(cluster codibge) nolog

* Export biprobit results
outreg2 using resultadosA1.doc, dec(3) se replace ctitle("Recursive biprobit") ///
    addstat("LR chi2", e(chi2), "Prob > chi2", e(p)) label ///
    addnote("Recursive: outsourcing ← Cons_saude. SE clustered at codibge. Includes i.year.")
	

* =========================
* 1B) Recursive model - Table 4
* =========================

outreg, clear
* Ensure panel structure + ordering by year within municipality
xtset codibge year
bysort codibge (year): gen _wave = _n   // 1=2014, 2=2018, 3=2021 (panel waves)

* If cooperation variables are strings, convert to numeric 0/1
capture confirm numeric variable Cons_saude
if _rc destring Cons_saude, replace ignore(" ,.")
capture confirm numeric variable Anos_Cons_Saude
if _rc destring Anos_Cons_Saude, replace ignore(" ,.")

* Lag by previous observed wave (does not use L.)
bysort codibge (year): gen Lw_Cons_saude      = Cons_saude[_n-1]
bysort codibge (year): gen Lw_Anos_Cons_Saude = Anos_Cons_Saude[_n-1]
label var Lw_Cons_saude      "Health consortium (previous wave)"
label var Lw_Anos_Cons_Saude "Years in health consortium (previous wave)"

* Check whether there is now a valid sample
count if !missing(Lw_Cons_saude, Lw_Anos_Cons_Saude)


eststo clear

* Model 01 (Recursive GEE – wave)
quietly xtgee outsourcing Lw_Cons_saude Lw_Anos_Cons_Saude i.year ///
    if !missing(Lw_Cons_saude, Lw_Anos_Cons_Saude), ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m1

* Model 02 (Fiscal GEE)
quietly xtgee outsourcing ltotal_rec indicador_2 lGasto_saude_capita i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m2

* Model 03 (Mayor characteristics GEE)
quietly xtgee outsourcing age_mayor edu_HighSchool edu_UndergraduateAbove ///
    female_mayor second_term i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m3

* Model 04 (Capacity / bureaucracy GEE)
quietly xtgee outsourcing Buroc_estatut Buroc_esc Buroc_capita i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m4

* Model 05 (Ideology GEE)
quietly xtgee outsourcing direita i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m5

* Model 06 (Councils GEE)
quietly xtgee outsourcing dummy_del_saude quant_reuniao_saude i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m6

* Model 07 (Policy salience GEE)
quietly xtgee outsourcing health_salient i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m7

* Model 08 (Full model – recursive wave)
quietly xtgee outsourcing ///
    Lw_Cons_saude Lw_Anos_Cons_Saude ///
    age_mayor edu_HighSchool edu_UndergraduateAbove female_mayor second_term ///
    Buroc_estatut Buroc_esc Buroc_capita ///
    ltotal_rec indicador_2 lGasto_saude_capita direita dummy_del_saude quant_reuniao_saude health_salient ///
    lpop lpib Capital distcapestd distcapfed ///
    i.year if !missing(Lw_Cons_saude, Lw_Anos_Cons_Saude), ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo m8

* ================================
* Export single table (columns 1–8)
* ================================

esttab m1 m2 m3 m4 m5 m6 m7 m8 using "resultadosA2.rtf", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    label compress nogaps ///
    mtitles("Model 01 (recursive-wave)" "Model 02 (fiscal)" "Model 03 (mayor)" ///
        "Model 04 (capacity)" "Model 05 (ideology)" "Model 06 (councils)" ///
        "Model 07 (policy salience)" "Model 08 (complete-wave)") ///		
    stats(N chi2 p, labels("N" "Wald chi2" "Prob > chi2")) ///
    addnotes("GEE binomial-probit; corr(exchangeable); vce(robust); Year FE (i.year)." ///
             "In Models 01 and 06, cooperation is lagged by wave (Lw_*).") ///
    drop(_cons) indicate("Year FE = *.year")




*******************************************************
* CULTURE SECTOR
*******************************************************

*-----------------------------------------------------*
* 0) Preparation
*-----------------------------------------------------*


xtset codibge year
outreg, clear

*-----------------------------------------------------*
*   Table 5
* 1) JOINT RECURSIVE MODEL (BIVARIATE) – biprobit
*   Eq.1 (outcome): outsourcing_cultura = f(imc_cultura, year_cultura, X, Year FE)
*   Eq.2 (selection): imc_cultura = f(Z, Year FE)
*   vce(robust). Include at least one excluded instrument in Z.
*-----------------------------------------------------*
biprobit ///
    (outsourcing_cultura = imc_cultura year_cultura ///
        age_mayor edu_HighSchool edu_UndergraduateAbove female_mayor second_term ///
        Buroc_estatut Buroc_esc Buroc_capita ///
        ltotal_rec indicador_2 lGasto_cultura_capita direita dummy_del_cultura quant_reuniao_cultura culture_salient ///
        lpop lpib i.year) ///
    (imc_cultura = ///
        lpop lpib Capital distcapestd distcapfed ///
        age_mayor female_mayor ///
        i.year), vce(robust) nolog

outreg2 using resultados_cultura1.doc, dec(3) se replace ctitle("Recursive biprobit – Culture") ///
    addstat("LR chi2", e(chi2), "Prob > chi2", e(p)) label ///
    addnote("Recursive system: outsourcing_cultura ← imc_cultura. vce(robust). Includes i.year.")



*-----------------------------------------------------*
* 2) PA MODELS (GEE) WITH WAVE LAGS - Table 06
*-----------------------------------------------------*
bysort codibge (year): gen _wave_cult = _n
capture drop Lw_imc_cultura Lw_year_cultura

* Ensure cooperation variables are numeric
capture confirm numeric variable imc_cultura
if _rc destring imc_cultura, replace ignore(" ,.")
capture confirm numeric variable year_cultura
if _rc destring year_cultura, replace ignore(" ,.")

bysort codibge (year): gen Lw_imc_cultura  = imc_cultura[_n-1]
bysort codibge (year): gen Lw_year_cultura = year_cultura[_n-1]
label var Lw_imc_cultura  "Culture IMC (previous wave)"
label var Lw_year_cultura "Years in culture consortium (previous wave)"

* ================================
* CULTURE — eststo / esttab
* ================================

eststo clear

quietly xtgee outsourcing_cultura Lw_imc_cultura Lw_year_cultura i.year ///
    if !missing(Lw_imc_cultura, Lw_year_cultura), ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c1

quietly xtgee outsourcing_cultura ltotal_rec indicador_2 lGasto_cultura_capita i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c2

quietly xtgee outsourcing_cultura age_mayor edu_HighSchool edu_UndergraduateAbove ///
    female_mayor second_term i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c3

quietly xtgee outsourcing_cultura Buroc_estatut Buroc_esc Buroc_capita i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c4

quietly xtgee outsourcing_cultura direita i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c5

quietly xtgee outsourcing_cultura dummy_del_cultura quant_reuniao_cultura i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c6

quietly xtgee outsourcing_cultura culture_salient i.year, ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c7

quietly xtgee outsourcing_cultura ///
    Lw_imc_cultura Lw_year_cultura ///
    age_mayor edu_HighSchool edu_UndergraduateAbove female_mayor second_term ///
    Buroc_estatut Buroc_esc Buroc_capita ///
    ltotal_rec indicador_2 lGasto_cultura_capita direita culture_salient ///
    lpop lpib Capital distcapestd distcapfed ///
    i.year if !missing(Lw_imc_cultura, Lw_year_cultura), ///
    family(binomial) link(probit) corr(exchangeable) vce(robust) iterate(200)
eststo c8

esttab c1 c2 c3 c4 c5 c6 c7 c8 using "resultados_cultura2.rtf", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    label compress nogaps ///
    mtitles("Model 01 (recursive-wave)" "Model 02 (fiscal)" "Model 03 (mayor)" ///
        "Model 04 (capacity)" "Model 05 (ideology)" "Model 06 (councils)" ///
        "Model 07 (policy salience)" "Model 08 (complete-wave)") ///
    stats(N chi2 p, labels("N" "Wald chi2" "Prob > chi2")) ///
    addnotes("GEE binomial-probit; corr(exchangeable); vce(robust); Year FE (i.year)." ///
             "In Models 01 and 08, cooperation is lagged by wave (Lw_*).") ///
    drop(_cons) indicate("Year FE = *.year")



display "=== END (CULTURE): results saved in resultados_cultura.doc ==="
```


]()

