
********************************************************************************
**************************  TIME ADJUSTED IMD ( 03b ) **************************
********************************************************************************

* Author: Matthew Tibbles

version 17
macro drop _all 
capture log close 
clear all 
drop _all 

set linesize 255 


* Description
********************************************************************************
* This do-file generates coefficient estimates and delta-based marginal predictions
* for REWB cross-level interaction regression models of IMD/time-adjusted IMD 
* on Small Area Mental Health Index score.

* The first model includes a cross-level interaction term between the within and
* between effect of IMD/time-adjusted IMD.
* The second model includes an additional three-way interaction between the within
* component of this interaction and the time-invariant effect of LSOA residential
* area type.

* Estimates are written to CSV, and and imported into R, which is then used to
* generate Table 4 and Figures 4 and 5 (see IMtadj-03-script.R)

********************************************************************************

// Required packages
local packages center xsvmat
foreach p in `packages' {
    capture `p'
    if _rc {
        ssc install `p'
    }
}


* Load/clean data
********************************************************************************

// Load
import delimited "Time Adjusted IMD\Data\Derived\IMD_samhi_TS_agg.csv", clear 

// Destring LSOA code
replace lsoa_code_agg = subinstr(lsoa_code_agg, "E", "", .) 
destring lsoa_code_agg, gen(lsoa_coden)

// Declare panel format
xtset lsoa_coden year


* REWB with cross-level interaction estimation
********************************************************************************

* Model and coefficient table
*--------------------------------

frame copy default rewbi
frame change rewbi

// Macros for DV and controls
local dv samhi
local w_controls w_medage w_male_share w_popdens w_morz
local b_controls b_medage b_male_share b_popdens b_morz
local lv2 i.resarea_code


mat tabmat_rewbi = J(21, 4,.)
//mat pmat_rewbi = J(21, 4,.)

// Loop over IMD / time-adjusted IMD
local mno 0
local mtype imd imd_tadj
foreach m in `mtype' {
    local ++ mno
   
	*| Generate cross-level interaction
    gen wxb_`m'_dec_ipol = w_`m'_dec_ipol* b_`m'_dec_ipol

    bysort lsoa_coden: center wxb_`m'_dec_ipol, /*
	               */  gen(w_wxb_`m'_dec_ipol) mean(b_wxb_`m'_dec_ipol)
    
	*| Estimate model
    xtreg `dv' w_`m'_dec_ipol b_`m'_dec_ipo  /*
	       */  w_wxb_`m'_dec_ipol b_wxb_`m'_dec_ipol  /*
		   */  `w_controls' `b_controls' `lv2',  /*
		   */  vce(cluster lsoa_coden) re

	*| Store coefficient matrix		   
	mat b`mno' = e(b)
	mat V`mno' = e(V)
	local paramno = rowsof(V`mno')
	mat se`mno' = J(1, `paramno',.)
	forvalues i = 1/`paramno' {
		mat se`mno'[1,`i'] = sqrt(V`mno'[`i',`i'])
	}
	local rnames: rownames e(V)
    mat rownames tabmat_rewbi = `rnames'
	forvalues i = 1/`paramno' {
		if `mno' == 1 {
		    mat tabmat_rewbi[`i',1] = b`mno'[1, `i']
		    mat tabmat_rewbi[`i',2] = se`mno'[1, `i']
	    }
		else {
			mat tabmat_rewbi[`i',3] = b`mno'[1, `i']
		    mat tabmat_rewbi[`i',4] = se`mno'[1, `i']
	    }
	}
	
	mat rtab = r(table) 
	mat matp_rewbi_`mno' = rtab[4,1...]
	
	est store rewbi_`mno'
}
	
mat matp_rewbi = matp_rewbi_1 \ matp_rewbi_2
local rnamesswap: rownames tabmat_rewbi

* Marginal predictions
*---------------------------

// Program to post results matrixes to e
capture program drop epost_bv
program define epost_bv, eclass
   args b V
   ereturn post `b' `V'
end

// Tag 1st observation 
sort lsoa_coden year
tempvar obno
gen `obno' = _n
local ob1 `obno' == 1
su lsoa_coden if `ob1'
tempvar gr1
gen `gr1' = 1 if lsoa_coden == r(mean)
local gr1 `gr1' == 1
tab lsoa_coden if `gr1'

// Macro for controls
local controls medage male_share popdens morz

// Set controls at mean/median
foreach c in `controls' {
		
	su `c', meanonly
	replace `c' = r(mean)
		
	tempvar w_`c'_T b_`c'_T  
	bysort lsoa_coden: center `c', gen(`w_`c'_T') mean(`b_`c'_T')
	
	replace w_`c' = `w_`c'_T'
	replace b_`c' = `b_`c'_T'
}
su resarea_code, detail
replace resarea_code = r(p50)

// Loop over IMD / time-adjusted IMD
local mtype imd imd_tadj
local mno 0
foreach m in `mtype' {
    local ++ mno
	est restore rewbi_`mno'
	
	*| Loop over IMD deciles (between effect)
    forvalues b = 1/9 {
	
	    replace `m'_dec_ipol = `b' if `gr1'
        
		*| Discrete effect of IMD across IMD decile ranks (within effect)
	    forvalues w = 0/1 {
		
		    replace `m'_dec_ipol = `b' + `w' if `ob1'
				
			 tempvar                                           /*
			 */      w_`m'_dec_ipolT b_`m'_dec_ipolT          /*
			 */      wxb_`m'_dec_ipolT                           /*
		     */      w_wxb_`m'_dec_ipolT b_wxb_`m'_dec_ipolT
			 
			 *|  Within/between compmponents
			 bysort lsoa_coden: center `m'_dec_ipol, /*
			 */     gen(`w_`m'_dec_ipolT') mean(`b_`m'_dec_ipolT')
			 	 
			 *| Multiplicative cross-level interaction term
			 gen `wxb_`m'_dec_ipolT' =                   /*
		     */  `w_`m'_dec_ipolT'*`b_`m'_dec_ipolT'
			 
			 *| Within-between components of cross-level interaction
			 bysort lsoa_coden: center `wxb_`m'_dec_ipolT', /*
			 */     gen(`w_wxb_`m'_dec_ipolT') mean(`b_wxb_`m'_dec_ipolT')
			 
			 *| Update model paramters
			 replace w_`m'_dec_ipol = `w_`m'_dec_ipolT' if `ob1'
			 replace b_`m'_dec_ipol = `b_`m'_dec_ipolT' if `gr1'
			 replace w_wxb_`m'_dec_ipol = `w_wxb_`m'_dec_ipolT' if `ob1'
			 replace b_wxb_`m'_dec_ipol = `b_wxb_`m'_dec_ipolT' if `gr1'
			
			 *| Generate predictions with partial derivatives
			 predictnl pr_`mno'_`b'_`w' = xb(), g(d_`mno'_`b'_`w'___) 	
			 
			 *| Set derivative to 0 if missing
			 forvalues p = 1/`paramno' {
			     capture confirm var d_`mno'_`b'_`w'___`p'
				 if c(rc) == 111 { 
				     gen d_`mno'_`b'_`w'___`p' = 0
				 }
				 else {
				 	continue
				 }
			 }
		}
	}
}


/// Drop redundant observations
drop if `obno' != 1

// Post-estimation loop
local mno 0
local mtype imd imd_tadj
foreach m in `mtype' {
    local ++ mno
	
	preserve
	
	est restore rewbi_`mno'
	
	/// Estimates matrix
	local colno_b = 2*9
    mat b = J(1,`colno_b',.)
    
	/// Jacobian matrix										
	local rowno_j = 2*9
    mat J = J(`rowno_j',`paramno',.)
    local cnames_j: colnames e(V)
    mat colnames J = `cnames_j'
	
	/// Estimates matrix values
	forvalues b = 1/9 {
		
		forvalues w = 0/1 {
					
			if `w' == 0 {
						
				mat b[1,`b'] = pr_`mno'_`b'_`w'[1]
			}
					
			else if `w' == 1 {
						
				local b2 = `b'+9
						
				mat b[1,`b2'] = pr_`mno'_`b'_`w'[1]
			}
			
			
			/// Jacobian matrix values
			forvalues p = 1/`paramno' {
				
		        if `w' == 0 {
				 	
				    mat J[`b',`p'] = d_`mno'_`b'_`w'___`p'[1]
				    
				}
				   
				else if `w' == 1 {
				    local b2 = `b'+9
					mat J[`b2',`p'] = d_`mno'_`b'_`w'___`p'[1]
				}
			}
		}
	}


	/// Generate variance-covariance matrix
	mat V = J*e(V)*J'
	
	/// Macro for matrix row/column names
	local names ""
	
	forvalues w = 0/1 {
        forvalues b = 1/9 {
            local names "`names' _atw`w'#bdec`b'"
        }
	}   
	
	di "`names'"
	
	mat colnames b = `names'
    mat	colnames V = `names'
	mat rownames V = `names'
	

    /// Post results matrixes to e (for feeding to nlcom)
    epost_bv b V
    
    /// Matrix to store est
	mat rewbi_`mno' = J(9,3,.)
	
	/// Calculate discrete effect (of within effect across (between) IMD deciles)
	forvalues b = 1/9 {
		
	    nlcom (e:(_b[_atw1#bdec`b']-_b[_atw0#bdec`b']))
	
		mat rewbi_`mno'[`b', 1] = r(b)[1,1]
		mat rewbi_`mno'[`b', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
		mat rewbi_`mno'[`b', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
	}
	
	/// Convert matrix to data file
	xsvmat rewbi_`mno', saving("Time Adjusted IMD\Data\Derived\pred_rewbi_`m'", replace)
	use "Time Adjusted IMD\Data\Derived\pred_rewbi_`m'", clear
	gen m = `mno'
	gen bdec = _n
	rename rewbi_`mno'1 _e
    rename rewbi_`mno'2 _lb
    rename rewbi_`mno'3 _ub
	save "Time Adjusted IMD\Data\Derived\pred_rewbi_`m'", replace
	
	restore
}

/// Append results and save
use "Time Adjusted IMD\Data\Derived\pred_rewbi_imd", clear
append using "Time Adjusted IMD\Data\Derived\pred_rewbi_imd_tadj"
export delimited "Time Adjusted IMD\Data\Derived\pred_rewbi.csv", replace


* REWB with 3-way cross-level interaction estimation
********************************************************************************
frame change default
frame copy default rewbi3
frame change rewbi3

// Macros for DV and controls
local dv samhi
local w_controls w_medage w_male_share w_popdens w_morz
local b_controls b_medage b_male_share b_popdens b_morz
local lv2 i.resarea_code

mat tabmat_rewbi3 = J(27, 4,.)

// Loop over IMD / time-adjusted IMD
local mno 0
local mtype imd imd_tadj
foreach m in `mtype' {
    local ++ mno
   
	*| Generate cross-level interaction
    gen wxb_`m'_dec_ipol = w_`m'_dec_ipol* b_`m'_dec_ipol

    bysort lsoa_coden: center wxb_`m'_dec_ipol, /*
	               */  gen(w_wxb_`m'_dec_ipol) mean(b_wxb_`m'_dec_ipol)
				       
	*| Estimate model with 3-way interaction
    xi: xtreg `dv' w_`m'_dec_ipol b_`m'_dec_ipo  /*
	       */  `lv2'*w_wxb_`m'_dec_ipol b_wxb_`m'_dec_ipol  /*
		   */  `w_controls' `b_controls',  /*
		   */  vce(cluster lsoa_coden) re
 

			   
	mat b`mno' = e(b)
	mat V`mno' = e(V)
	local paramno = rowsof(V`mno')
	mat se`mno' = J(1, `paramno',.)
	forvalues i = 1/`paramno' {
		mat se`mno'[1,`i'] = sqrt(V`mno'[`i',`i'])
	}
	local rnames: rownames e(V)
    mat rownames tabmat_rewbi3 = `rnames'
	forvalues i = 1/`paramno' {
		if `mno' == 1 {
		    mat tabmat_rewbi3[`i',1] = b`mno'[1, `i']
		    mat tabmat_rewbi3[`i',2] = se`mno'[1, `i']
	    }
		else {
			mat tabmat_rewbi3[`i',3] = b`mno'[1, `i']
		    mat tabmat_rewbi3[`i',4] = se`mno'[1, `i']
	    }
	}
	
	mat rtab = r(table) 
	mat matp_rewbi3_`mno' = rtab[4,1...]
		
	est store rewbi3_`mno'
	
}

mat matp_rewbi3 = matp_rewbi3_1 \ matp_rewbi3_2

* Export coefficient table
*----------------------------

// Combine model matrixes
mat_capp mattab : tabmat_rewbi tabmat_rewbi3, miss(.) cons

// Recode matrix (to account for different area codes name due i. versus xi prefix)
forvalues col = 5/8 {
	local j 7
    forvalues row = 21/28 {
	    local rowmj = `row' - `j'
        mat mattab[`rowmj',`col'] = mattab[`row', `col']
	}
}
mat mattabadj1 = mattab[1..20,1...]
mat mattabadj2 = mattab[28..35,1...]
mat mattabadj = mattabadj1\mattabadj2

// Matrix to dataset
xsvmat mattabadj, rownames(variable) frame(mattab)
frame change mattab

// Export estimates
export delimited "Time Adjusted IMD\Data\Derived\table_rewbi.csv", replace

// Combine model p value matrixes
//mat_rapp mattabp : matp_rewbi matp_rewbi3, miss(.) cons
//forvalues row = 3/4 {
	//local j 7 
	//forvalues col = 21/28 {
		//local colmj = `col' - `j'
		//mat mattabp[`row', `colmj'] = mattabp[`row', `col']
	//}
//}
//xsvmat mattabp,  frame(mattabp) names(col)



* Marginal predictions
*---------------------------

frame change rewbi3

// Program to post results matrixes to e
capture program drop epost_bv
program define epost_bv, eclass
args b V
ereturn post `b' `V'
end

// Tag 1st observation 
sort lsoa_coden year
tempvar obno
gen `obno' = _n
local ob1 `obno' == 1
su lsoa_coden if `ob1'
tempvar gr1
gen `gr1' = 1 if lsoa_coden == r(mean)
local gr1 `gr1' == 1
tab lsoa_coden if `gr1'



// Set controls at mean
local controls medage male_share popdens morz
foreach c in `controls' {
		
    su `c', meanonly
	replace `c' = r(mean)
		
	tempvar w_`c'_T b_`c'_T  
	bysort lsoa_coden: center `c', gen(`w_`c'_T') mean(`b_`c'_T')
	
	replace w_`c' = `w_`c'_T'
	replace b_`c' = `b_`c'_T'
}

// Set area classification and 3-way interaction term at 0 (reference code)
forvalues r = 2/8 {
	 replace _Iresarea_c_`r' = 0
	 replace _IresXw_wxb_`r' = 0
}


// Loop over IMD / time-adjusted IMD
local mtype imd imd_tadj
local mno 0
foreach m in `mtype' {
    local ++ mno
	est restore rewbi3_`mno'
	
	*| Loop over areas (I use separate frames bc Stata BE var limit)
	forvalues r = 1/8 {
		
		if `r' == 1 {
			frame copy rewbi3 rewbi3_`m'_1
	        frame change rewbi3_`m'_1
		}
		
		else if `r' == 4 {
			frame copy rewbi3 rewbi3_`m'_2
	        frame change rewbi3_`m'_2
			replace  _Iresarea_c_`r' = 1
		}
		
		else if `r' == 7 {
			frame copy rewbi3 rewbi3_`m'_3
	        frame change rewbi3_`m'_3
			replace  _Iresarea_c_`r' = 1
		}
        
		*| Set area code (2-8) to 1
	    else {
			replace  _Iresarea_c_`r' = 1
		}
			
		*| Loop over IMD deciles (between effect)
        forvalues b = 1/9 {
	
	        replace `m'_dec_ipol = `b' if `gr1'
            
			*| Discrete effect across IMD deciles (within effect)
	        forvalues w = 0/1 {
		
			    replace `m'_dec_ipol = `b' + `w' if `ob1'
				
			    tempvar                                           /*
			    */      w_`m'_dec_ipolT b_`m'_dec_ipolT          /*
			    */      wxb_`m'_dec_ipolT                           /*
		        */      w_wxb_`m'_dec_ipolT b_wxb_`m'_dec_ipolT
			 
			    *|  Within/between compmponents
			    bysort lsoa_coden: center `m'_dec_ipol, /*
			    */     gen(`w_`m'_dec_ipolT') mean(`b_`m'_dec_ipolT')
			 	 
			    *| Multiplicative cross-level interaction term
			    gen `wxb_`m'_dec_ipolT' =                   /*
		        */  `w_`m'_dec_ipolT'*`b_`m'_dec_ipolT'
			 
			    *| Within-between components of cross-level interaction
			    bysort lsoa_coden: center `wxb_`m'_dec_ipolT', /*
			    */     gen(`w_wxb_`m'_dec_ipolT') mean(`b_wxb_`m'_dec_ipolT')
			 
			    *| Update model paramters
			    replace w_`m'_dec_ipol = `w_`m'_dec_ipolT' if `ob1'
			    replace b_`m'_dec_ipol = `b_`m'_dec_ipolT' if `gr1'	
			    
				replace w_wxb_`m'_dec_ipol = `w_wxb_`m'_dec_ipolT' if `ob1'
			    replace b_wxb_`m'_dec_ipol = `b_wxb_`m'_dec_ipolT' if `gr1'
				
				if `r' != 1{
					replace  _IresXw_wxb_`r' = w_wxb_`m'_dec_ipol * _Iresarea_c_`r'
				}
			   
			    *| Generate predictions with partial derivatives
			    predictnl pr_`mno'_`b'_`w'_`r' = xb(), g(d_`mno'_`b'_`w'_`r'___) 	
			 
			    *| Set partial derivative to 0 if missing
			    forvalues p = 1/`paramno' {
			        capture confirm var d_`mno'_`b'_`w'_`r'___`p'
				    if c(rc) == 111 { 
				        gen d_`mno'_`b'_`w'_`r'___`p' = 0
				    }
				    else {
				        continue
				    }
			    }
		    }
	    }
		*| Set area code and 3way interaction back to 0
		if `r' != 1 {
		    replace  _Iresarea_c_`r' = 0
			replace  _IresXw_wxb_`r' = 0
		}
    }
	frame change rewbi3
}

frame change rewbi3

// Post-estimation loop
local mtype imd imd_tadj
local mno 0
foreach m in `mtype' {
	
	preserve
	
    local ++ mno
		
	
	*| Loop by area
	forvalues r = 1/8 {
		
	    est restore rewbi3_`mno'
		
		local colno = 2*9
		mat b = J(1, `colno',.)
		
	    local paramno = colsof(e(V))
        mat J = J(`colno',`paramno',.)
		
		if `r' == 1 {
	        frame change rewbi3_`m'_1
			drop if _n != 1
		}
			
		else if `r' == 4 {
			frame change rewbi3_`m'_2
			drop if _n != 1
		}
		
		else if `r' == 7 {
			frame change rewbi3_`m'_3
			drop if _n != 1
		}
			
		*| Loop over between deciles
		forvalues b = 1/9 {
				
		    local bp1 = `b' + 9
		    
			*| Loop within
		    forvalues w = 0/1 {
					
			    if `w' == 0 {
				    mat b[1,`b'] = pr_`mno'_`b'_`w'_`r'[1]
			    }
					
			    else if `w' == 1 {
				    mat b[1,`bp1'] = pr_`mno'_`b'_`w'_`r'[1]
			    }
			
		        forvalues p = 1/`paramno' {
				
		            if `w' == 0 {
				        mat J[`b',`p'] = d_`mno'_`b'_`w'_`r'___`p'[1]
				    }
				   
			        else if `w' == 1 {
					    mat J[`bp1',`p'] = d_`mno'_`b'_`w'_`r'___`p'[1]
				    }
			    }
		    }
		}
	
	    /// Generate variance-covariance matrix
	    mat V = J*e(V)*J'
	
	    /// Macro for matrix row/column names
	    local names ""
	    forvalues w = 0/1 {
            forvalues b = 1/9 {
                local names "`names' _atw`w'#bdec`b'"
            }
	    } 

	    di "`names'"
	
	    mat colnames b = `names'
        mat	colnames V = `names'
	    mat rownames V = `names'
	

        /// Post results matrixes to e (for feeding to nlcom)
        epost_bv b V
    
        /// Matrix to store est
	    mat rewbi3_`m'_`r' = J(9,4,.)
		forvalues c = 1/9 {
		    mat rewbi3_`m'_`r'[`c', 4] = `r'
		}
	
	    /// Calculate discrete effect (of within effect across (between) IMD deciles) by area
	    forvalues b = 1/9 {
		
	        nlcom (e:(_b[_atw1#bdec`b']-_b[_atw0#bdec`b']))
	
		    mat rewbi3_`m'_`r'[`b', 1] = r(b)[1,1]
		    mat rewbi3_`m'_`r'[`b', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
		    mat rewbi3_`m'_`r'[`b', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
	    }
				
	}
	
	frame create rewbi3_`m'
	frame change rewbi3_`m'
	
	
	/// Combine area estimates
	mat rewbi3_`m' = rewbi3_`m'_1 \ rewbi3_`m'_2 \ rewbi3_`m'_3 \ rewbi3_`m'_4 /*
	             */  \ rewbi3_`m'_5 \ rewbi3_`m'_6 \ rewbi3_`m'_7 \ rewbi3_`m'_8
	
	/// Convert matrix to data file
	xsvmat rewbi3_`m', saving("Time Adjusted IMD\Data\Derived\pred_rewbi3_`m'", replace)
	use "Time Adjusted IMD\Data\Derived\pred_rewbi3_`m'", clear
	rename rewbi3_`m'1 _e
    rename rewbi3_`m'2 _lb
    rename rewbi3_`m'3 _ub
	rename rewbi3_`m'4 area
	sort area, stable
	by area: gen bdec = _n
	gen m = `mno'
	save "Time Adjusted IMD\Data\Derived\pred_rewbi3_`m'", replace
	
	frame change rewbi3
	frame drop rewbi3_`m'
	
	restore
}

/// Append results and save
use "Time Adjusted IMD\Data\Derived\pred_rewbi3_imd", clear
append using "Time Adjusted IMD\Data\Derived\pred_rewbi3_imd_tadj"
export delimited "Time Adjusted IMD\Data\Derived\pred_rewbi3.csv", replace

	
	
