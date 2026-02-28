/*
The English translations of the Chinese content in the data and code can be found in Translation.md
*/

clear all
set more off

* Please check the path.
use "Final Data.dta", clear
keep if year >= 2018

* merge with marketization index data
* Please check the path
merge m:1 stkcd year using "Marketization Index and Its Sub-indices(1997-2024).dta"
drop _merge


gen ln_total_employee = ln(员工总数)
gen ln_per_profit     = ln(人均创利万元)
gen ln_hires          = ln(1 + recruit_total)

rename llm_exposure_alpha_core_weight    llm_exp_alpha_core
rename llm_exposure_beta_core_weight     llm_exp_beta_core
rename llm_exposure_gamma_core_weight    llm_exp_gamma_core
rename llm_exposure_alpha_equal_weight   llm_exp_alpha_eq
rename llm_exposure_beta_equal_weight    llm_exp_beta_eq
rename llm_exposure_gamma_equal_weight   llm_exp_gamma_eq

gen byte type_1 = inlist(strtrim(labor_type), "非常规认知", "常规认知")
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

* z-score
foreach v in llm_exp_gamma_core llm_exp_gamma_eq supervision_exposure {
    cap drop z_`v'
    bys type_1: egen z_`v' = std(`v')
    label var z_`v' "Std. `v' within type_1"
}

local X "ln_total_employee ln_per_profit 本科学历占比"

* 1.Firm heterogeneity
* generate total income
gen total_revenue = 员工总数 * 人均创收万元
label var total_revenue "企业总收入"

local SIZE_BASE 2020

gen double tmp_rev0 = total_revenue if year==`SIZE_BASE'
bys stkcd: egen double rev0 = max(tmp_rev0)
drop tmp_rev0

* medium
bys industry: egen double med_rev0 = median(rev0)

gen byte size_large = (rev0 >= med_rev0) if !missing(rev0, med_rev0)
label define size_lab 1 "Large Scale" 0 "Small Scale", replace
label values size_large size_lab
label var size_large "Large firm (rev0 >= industry median, base=`SIZE_BASE')"

levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regressions for Group: `g_name'"
    * ---- Small firms ----
	di "Small firms"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & size_large==0, ///
        absorb(year industry Province) ///
		vce(cluster stkcd)
				
    * ---- Large firms ----
	di "Large firms"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & size_large==1, ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
	
	* Wald Test
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core##c.z_supervision_exposure##i.size_large ///
        `X' ///
        if type_1==`g' & !missing(size_large), ///
        absorb(year industry Province) ///
        vce(cluster stkcd)

    quietly test 1.size_large#c.z_llm_exp_gamma_core = 0
    di as result "Wald p (LLM slope diff: Large vs Small) = " %6.4f r(p)

    quietly test 1.size_large#c.z_llm_exp_gamma_core#c.z_supervision_exposure = 0
    di as result "Wald p (LLM×Sup diff: Large vs Small)   = " %6.4f r(p)
}

* 2. Industry heterogeneity(Sector)
gen byte sector = .

* ---- Manufacturing ----
replace sector = 1 if inlist(industryname, ///
"专用设备制造业","仪器仪表制造业","其他制造业","农副食品加工业", ///
"化学原料及化学制品制造业")

replace sector = 1 if inlist(industryname, "化学纤维制造业","医药制造业","印刷和记录媒介复制业", ///
"家具制造业","文教、工美、体育和娱乐用品制造业")

replace sector = 1 if inlist(industryname, "有色金属冶炼及压延加工业", ///
"木材加工及木、竹、藤、棕、草制品业","橡胶和塑料制品业","汽车制造业", ///
"电气机械及器材制造业","皮革、毛皮、羽毛及其制品和制鞋业")

replace sector = 1 if inlist(industryname, "石油加工、炼焦及核燃料加工业","纺织业","纺织服装、服饰业", ///
"计算机、通信和其他电子设备制造业")

replace sector = 1 if inlist(industryname, "通用设备制造业","造纸及纸制品业", ///
"酒、饮料和精制茶制造业","金属制品业", ///
"铁路、船舶、航空航天和其它运输设备制..","非金属矿物制品业","食品制造业", ///
"黑色金属冶炼及压延加工业","废弃资源综合利用业")

* ---- Other  ----
replace sector = 2 if inlist(industryname, ///
"农业","林业","畜牧业","渔业","农、林、牧、渔服务业")

replace sector = 2 if inlist(industryname, ///
"煤炭开采和洗选业","石油和天然气开采业","有色金属矿采选业","黑色金属矿采选业","非金属矿采选业")
replace sector = 2 if inlist(industryname, "开采辅助活动", ///
"土木工程建筑业","房屋建筑业","建筑安装业","建筑装饰和其他建筑业")

replace sector = 2 if inlist(industryname , ///
"电力、热力生产和供应业","燃气生产和供应业","水的生产和供应业", ///
"综合")

* ---- Remaining is Services ----
replace sector = 0 if missing(sector)

label define sector 0 "Services" 1 "Manufacturing" 2 "Other", replace

levelsof type_1, local(L)
foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regressions for Group: `g_name'"
    * Services 
	di "Services"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & sector == 0, ///
        absorb(year industry Province) ///
		vce(cluster stkcd)
    * Manufacturing 
	di "Manufacturing"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & sector == 1, ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
	* Others 
	di "Others"
	reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & sector == 2, ///
        absorb(year industry Province) ///
		vce(cluster stkcd)
		
		
	* Wald Test
	reghdfe ln_hires ///
        c.z_llm_exp_gamma_core##c.z_supervision_exposure##ib0.sector ///
        `X' ///
        if type_1==`g' & !missing(sector), ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
 
    * H0: LLM_Sup slope is equal across all sectors.
    quietly testparm i.sector#c.z_llm_exp_gamma_core
    di as result "Wald p (Any sector diff in LLM slope)     = " %6.4f r(p)
    
    * H0: LLM_Exp * Sup_Exp slope is equal across all sectors.
    quietly testparm i.sector#c.z_llm_exp_gamma_core#c.z_supervision_exposure
    di as result "Wald p (Any sector diff in LLM×Sup slope) = " %6.4f r(p)
}


* Industry heterogeneity(Knowledge intensity)
bys industry: egen double ind_edu_share = mean(本科学历占比)
egen double edu_median = median(ind_edu_share)
gen byte high_knowledge = (ind_edu_share >= edu_median) if !missing(ind_edu_share)

levelsof type_1, local(L)
foreach g of local L {
    di "Running Regressions for Group: `g'"
    * --- high_knowledge ----
	di "high knowledge"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & high_knowledge == 1, ///
        absorb(year industry Province) ///
		vce(cluster stkcd)
    * ---- low_knowledge ----
	di "low knowledge"
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & high_knowledge == 0, ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
		
		reghdfe ln_hires ///
        c.z_llm_exp_gamma_core##c.z_supervision_exposure##i.high_knowledge ///
        `X' ///
        if type_1==`g' & !missing(high_knowledge), ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
    
    quietly test 1.high_knowledge#c.z_llm_exp_gamma_core#c.z_supervision_exposure = 0
    di as result "Wald p (LLM×Sup diff: High vs Low knowledge)   = " %6.4f r(p)
    
    quietly test 1.high_knowledge#c.z_llm_exp_gamma_core = 0
    di as result "Wald p (LLM slope diff: High vs Low knowledge) = " %6.4f r(p)

}

* Region heterogeneity 
local BASE_YEAR 2020

* Market Intermediaries and Legal Environment

local INSTVAR "市场中介组织的发育和法律制度环境"

gen double tmp_inst = `INSTVAR' if year == `BASE_YEAR'
bys stkcd: egen double inst0 = mean(tmp_inst)
drop tmp_inst

preserve
    keep if year == `BASE_YEAR' & !missing(inst0)
    bys stkcd: keep if _n == 1         
    quietly summarize inst0, meanonly
    scalar inst_cut = r(mean)           
restore

gen byte high_inst = (inst0 >= inst_cut) if !missing(inst0)
label define highinst 0 "Low (below 2020 mean)" 1 "High (>= 2020 mean)", replace
label values high_inst highinst
label var high_inst "High institution/legal env (firm avg in 2020 >= sample mean)"

levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regressions for Group: `g_name'"
    * High
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & high_inst==1, ///
        absorb(year industry Province) ///
		vce(cluster stkcd)
		
    * Low
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & high_inst==0, ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
		
	reghdfe ln_hires ///
        c.z_llm_exp_gamma_core##c.z_supervision_exposure##i.high_inst ///
        `X' ///
        if type_1==`g' & !missing(high_inst), ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
    
    quietly test 1.high_inst#c.z_llm_exp_gamma_core#c.z_supervision_exposure = 0
    di as result "Wald p (LLM×Sup diff: High vs Low institution) = " %6.4f r(p)
    
    quietly test 1.high_inst#c.z_llm_exp_gamma_core = 0
    di as result "Wald p (LLM slope diff: High vs Low institution) = " %6.4f r(p)
    
}


* Product Market Development
local BASE_YEAR 2020
local INSTVAR1 "产品市场的发育程度"

gen double tmp_inst1 = `INSTVAR1' if year == `BASE_YEAR'
bys stkcd: egen double inst01 = mean(tmp_inst1)
drop tmp_inst1

preserve
    keep if year == `BASE_YEAR' & !missing(inst01)
    bys stkcd: keep if _n == 1
    quietly summarize inst01, meanonly
    scalar inst_cut1 = r(mean)
restore

gen byte high_inst1 = (inst01 >= inst_cut1) if !missing(inst01)

label define highinst1 0 "Low (below 2020 mean)" 1 "High (>= 2020 mean)", replace
label values high_inst1 highinst1
label var high_inst1 "High product market dev. (firm avg in 2020 >= sample mean)"

levelsof type_1, local(L)

foreach g of local L {
    local g_name = cond(`g'==1, "Cognitive (认知类)", "Manual (非认知类)")
    di "Running Regressions for Group: `g_name'"
    * High
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & high_inst1==1, ///
        absorb(year industry Province) ///
		vce(cluster stkcd)
		
    * Low
    reghdfe ln_hires ///
        c.z_llm_exp_gamma_core ///
        c.z_llm_exp_gamma_core#c.z_supervision_exposure ///
		`X' ///
        if type_1==`g' & high_inst1==0, ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
		
	* Wald Test
	reghdfe ln_hires ///
        c.z_llm_exp_gamma_core##c.z_supervision_exposure##i.high_inst1 ///
        `X' ///
        if type_1==`g' & !missing(high_inst1), ///
        absorb(year industry Province) ///
        vce(cluster stkcd)
    
    quietly test 1.high_inst1#c.z_llm_exp_gamma_core#c.z_supervision_exposure = 0
    di as result "Wald p (LLM×Sup diff: High vs Low product market) = " %6.4f r(p)
    
    quietly test 1.high_inst1#c.z_llm_exp_gamma_core = 0
    di as result "Wald p (LLM slope diff: High vs Low product market) = " %6.4f r(p)
}








