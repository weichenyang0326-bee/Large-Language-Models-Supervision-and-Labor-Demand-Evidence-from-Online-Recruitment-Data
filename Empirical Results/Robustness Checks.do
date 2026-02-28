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
encode industryname, gen(ind_id)

* z-score
foreach v in llm_exp_gamma_core llm_exp_gamma_eq supervision_exposure {
    cap drop z_`v'
    bys type_1: egen z_`v' = std(`v')
    label var z_`v' "Std. `v' within type_1"
}

* 1. Lagged 
egen long panelid = group(stkcd labor_type)
duplicates drop panelid year, force
xtset panelid year

gen L_llm = L.llm_exp_gamma_core
gen L_sup = L.supervision_exposure

gen L_ln_emp    = L.ln_total_employee
gen L_ln_profit = L.ln_per_profit
gen L_edu       = L.本科学历占比

bys type_1: egen std_L_llm = std(L_llm)
bys type_1: egen std_L_sup = std(L_sup)

gen L_interact = std_L_llm * std_L_sup

label var std_L_llm  "Lagged LLM (Std.)"
label var std_L_sup  "Lagged Sup (Std.)"
label var L_interact "Interaction: Lagged LLM * Lagged Sup"

local L_X "L_ln_emp L_ln_profit L_edu"


reghdfe ln_hires std_L_llm L_interact  `L_X' ///
        if type_1 == 1, ///
        absorb(year province industry) /// 
        vce(cluster stkcd)
		
reghdfe ln_hires std_L_llm L_interact  `L_X' ///
        if type_1 == 0, ///
        absorb(year province industry) /// 
        vce(cluster stkcd)

* 2. IV

local RAW_X "llm_exp_gamma_core"      
local RAW_Z "supervision_exposure"  

bys industry year type_1: egen double sum_x = total(`RAW_X')
bys industry year type_1: egen long   n_x   = count(`RAW_X')

gen double iv_ind_raw = .
replace iv_ind_raw = (sum_x - `RAW_X') / (n_x - 1) if n_x > 1
drop sum_x n_x

local vars_to_std "`RAW_X' `RAW_Z' iv_ind_raw"

foreach v of local vars_to_std {
    cap drop z_`v'
    bys type_1: egen z_`v' = std(`v')
    label var z_`v' "Std. `v'"
}

gen double z_iv_inter = z_iv_ind_raw * z_supervision_exposure
label var z_iv_inter "IV Interaction (z_IV * z_Sup)"

local CTRLS "ln_total_employee ln_per_profit 本科学历占比"

winsor2 z_iv_inter z_iv_ind_raw ln_hires ln_total_employee ln_per_profit 本科学历占比, cuts(1 98) replace
ivreghdfe ln_hires `CTRLS' ///
        ( c.z_llm_exp_gamma_core c.z_llm_exp_gamma_core#c.z_supervision_exposure = ///
          c.z_iv_ind_raw       c.z_iv_inter ) ///
        if type_1 == 1, ///
        absorb(year Province) ///
        vce(cluster stkcd) ///
        first savefirst 

		    
ivreghdfe ln_hires `CTRLS' ///
        ( c.z_llm_exp_gamma_core c.z_llm_exp_gamma_core#c.z_supervision_exposure = ///
          c.z_iv_ind_raw       c.z_iv_inter ) ///
        if type_1 == 0 , ///
        absorb(year Province) ///
        vce(cluster stkcd) ///
        first savefirst 

* Excluding the COVID-19 year
levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive", "Manual")  
    di "Running Regression for Group: `g_name' "   
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///          
        c.z_llm_exp_gamma_core#c.z_supervision_exposure /// 
		`X' ///
        if type_1 == `g' & year != 2020, ///
        absorb(year industry Province) ///                 
        vce(cluster stkcd) //            
}

* Excluding LLM-producing industries.
/*
互联网和相关服务 corresponds to "Internet and related services"
软件和信息技术服务业 corresponds to "Software and Information Technology Services"
*/
levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive", "Manual")  
    di "Running Regression for Group: `g_name' "   
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///          
        c.z_llm_exp_gamma_core#c.z_supervision_exposure /// 
		`X' ///
        if type_1 == `g' & industryname != "互联网和相关服务" & industryname != "软件和信息技术服务业", ///
        absorb(year industry Province) ///                 
        vce(cluster stkcd) //            
}

* Alternative LLM exposure construction
levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive", "Manual")  
    di "Running Regression for Group: `g_name' "   
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_eq ///          
        c.z_llm_exp_gamma_eq#c.z_supervision_exposure /// 
		`X' ///
        if type_1 == `g', ///
        absorb(year industry Province) ///                 
        vce(cluster stkcd) //            
}

