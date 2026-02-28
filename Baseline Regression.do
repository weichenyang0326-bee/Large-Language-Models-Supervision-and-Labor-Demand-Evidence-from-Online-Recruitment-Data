/*
The English translations of the Chinese content in the data and code can be found in Translation.md
*/

clear all
set more off

* Please check the path.
use "Final Data.dta", clear
keep if year >= 2018

gen ln_total_employee = ln(员工总数)
gen ln_per_profit     = ln(人均创利万元)
gen ln_hires          = ln(1 + recruit_total)

rename llm_exposure_alpha_core_weight    llm_exp_alpha_core
rename llm_exposure_beta_core_weight     llm_exp_beta_core
rename llm_exposure_gamma_core_weight    llm_exp_gamma_core
rename llm_exposure_alpha_equal_weight   llm_exp_alpha_eq
rename llm_exposure_beta_equal_weight    llm_exp_beta_eq
rename llm_exposure_gamma_equal_weight   llm_exp_gamma_eq

* Divide the workforce into two major categories
* if type_1 = 1, labor belongs to Cognitive, else belongs to Manual
gen byte type_1 = inlist(strtrim(labor_type), "非常规认知", "常规认知")
* Code the industry and province
foreach var in industryname province {
    local newvar = cond("`var'" == "industryname", "industry", "Province")
    
    capture confirm numeric variable `var'
    if _rc {
        encode `var', gen(`newvar')
    }
    else {
        cap drop `newvar' 
        gen `newvar' = `var'
    }
}


/* ----------------------------------------------------------------------
baseline regression
------------------------------------------------------------------------*/ 

* z-score
foreach v in llm_exp_gamma_core llm_exp_gamma_eq supervision_exposure {
    cap drop z_`v'
    bys type_1: egen z_`v' = std(`v')
    label var z_`v' "Std. `v' within type_1"
}

* firm-level control variables
local X "ln_total_employee ln_per_profit 本科学历占比"

* Table 1 Column (1)
levelsof type_1, local(L)
foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regression for Group: `g_name' "
    reg ln_hires ///
        c.z_llm_exp_gamma_core ///         
        c.z_llm_exp_gamma_core#c.z_supervision_exposure /// 
        if type_1 == `g' , ///
        vce(cluster stkcd)
}

* Table 1 Column (2)
levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regression for Group: `g_name' "
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///          
        c.z_llm_exp_gamma_core#c.z_supervision_exposure /// 
        if type_1 == `g' , ///
        absorb(year) ///                 
        vce(cluster stkcd) //            
}

* Table 1 Column (3)
levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")  
    di "Running Regression for Group: `g_name' "   
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///          
        c.z_llm_exp_gamma_core#c.z_supervision_exposure /// 
		`X' ///
        if type_1 == `g' , ///
        absorb(year industry Province) ///                 
        vce(cluster stkcd) //            
}

* Table 1 Column (4)
levelsof type_1, local(L)
foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regression for Group: `g_name' (type_1=`g')"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///           
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///  
        `X' ///
        if type_1 == `g', ///
        absorb(year industry Province i.year#industry i.year#Province) ///
        vce(cluster stkcd)
}
