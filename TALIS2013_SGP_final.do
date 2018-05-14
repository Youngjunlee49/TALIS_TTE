* ======================================================================
* TALIS 2013 SINGAPORE FINAL PROJECT
* CREATED BY: 	YOUNGJUN LEE, APRIL 01, 2017
* UPDATED BY:   YOUNGJUN LEE, APRIL 15, 2017
* ======================================================================


************************************************************************
***** STANDARD HEADERS    	                                       *****
************************************************************************

	clear all
	set more off, permanently
	set matsize 5000
	
************************************************************************
***** START A LOG FILE    	                                       *****
************************************************************************
	
	cap log close
	set linesize 80		
	log using "final project_0413.log", replace
		
************************************************************************
***** OPEN THE DATA FILE		                                   *****
************************************************************************

	use "TALIS2013SGP_MERGED.dta", clear	
	d, short //n=3,109
	codebook IDSCHOOL //number of school=159

************************************************************************
***** DATA CLEANING      		                                   *****
************************************************************************

* Dependent variables *

	* All types of professional development activities
	tab1 TT2G21A1 TT2G21B1 TT2G21C1 TT2G21G TT2G21H TT2G21I
	
	* Missing data indicators *
	
foreach p of var TT2G21A1 TT2G21B1 TT2G21C1 TT2G21G TT2G21H TT2G21I {
	recode `p' (2=0) (7 9 =.)
	}

	cap drop pd1
	gen pd1=TT2G21A1
	cap drop pd2
	gen pd2=TT2G21B1 
	cap drop pd3
	gen pd3=TT2G21C1 
	cap drop pd4
	gen pd4=TT2G21D1
	cap drop pd5
	gen pd5=TT2G21E1
	cap drop pd6
	gen pd6=TT2G21G
	cap drop pd7
	gen pd7=TT2G21H
	cap drop pd8
	gen pd8=TT2G21I

	global 	pdlist "pd1 pd2 pd3 pd6 pd7 pd8"
	
	* TEACHER FACTORS
	cap drop FEMALE
	gen FEMALE=TT2G01
	recode FEMALE (2=0) (7 8 9=.) 

	cap drop PERMANENT
	gen PERMANENT=TT2G06    //employment status
	recode PERMANENT (2/3=0) (7 8 9=.)
	
	cap drop TWORKYEAR
	gen TWORKYEAR=TT2G05B //how many years of work experience as a teacher
	recode TWORKYEAR (97 98 99=.)
		
	cap drop TEACHTIME
	gen TEACHTIME=TT2G17 //teaching time per a week
	recode TEACHTIME (997 998 999=.)
	
	cap drop NEEDDIVERSE
	gen NEEDDIVERSE=TPDDIVS
	recode NEEDDIVERSE (9997 9998 9999=.)

	cap drop NEEDSUBEDU
	gen NEEDSUBEDU=TPDPEDS
	recode NEEDSUBEDU (9997 9998 9999=.)

	cap drop STEM
	recode TT2G15B (7 8 9=.)
	recode TT2G15C (7 8 9=.)
	gen STEM=0
	replace STEM=1 if TT2G15B==1 | TT2G15C==1
	replace STEM=. if TT2G15B==. | TT2G15C==.
		
	global TFAC     "FEMALE PERMANENT TWORKYEAR TEACHTIME NEEDDIVERSE NEEDSUBEDU STEM"
	
	* SCHOOL CONTEXT FACTORS
	/*cap drop PUBLIC
	gen PUBLIC=TC2G10
	recode PUBLIC (2=0) (7 8 9=.) //1=public school

	cap drop METRO
	gen METRO=TC2G09
	recode METRO (6=1) (1 2 3 4 5=0) (7 8 9=.)  //1=school in a large city */
	
	cap drop SIZE
	gen SIZE=TC2G14 //enrollment students
	recode SIZE (9997 9998 9999=.)
	
	recode STRATIO (997 998 999=.) //student-teacher ratio
	
	global SCFAC 	"SIZE STRATIO"  // 2 school context factors
	
	* SCHOOL ORGANIZATIONAL FACTORS
	cap drop PWORKYEAR
	gen PWORKYEAR=TC2G04B
	recode PWORKYEAR (97 98 99=.)
	
	recode PINSLEADS (9997 9998 9999=.)
	recode PSCMUTRS (9997 9998 9999=.)
	recode PLACKPER (7 8 9=.) (2/3=1) (1=0)
	recode PLACKMAT (7 8 9=.) (2/3=1) (1=0)
	
	global SOFAC	"PWORKYEAR PINSLEADS PSCMUTRS PLACKPER PLACKMAT" // 5 school organizational factors

	
	*DESCRIPTIVE STATISTICS

	order $pdlist $TFAC $SCFAC $SOFAC
	mean $pdlist $TFAC $SCFAC $SOFAC [pw=TCHWGT]
	sum $pdlist $TFAC $SCFAC $SOFAC

	keep if!missing(FEMALE, PERMANENT, TWORKYEAR, TEACHTIME, NEEDDIVERSE, ///
					NEEDSUBEDU, STEM, SIZE, STRATIO, ///
					PWORKYEAR, PINSLEADS, PSCMUTRS, PLACKPER, PLACKMAT)
	
	save "TALIS2013SGP_CLEAN.dat", replace 
	
	
************************************************************************
***** Multilevel Logistic Regression					           *****
************************************************************************

	cap log close
	set linesize 80		
	log using "final project_0413.log", replace

	use "TALIS2013SGP_CLEAN.dat", clear

	global 	pdlist "pd1 pd2 pd3 pd4 pd5 pd6 pd7 pd8"
	global TFAC     "FEMALE PERMANENT TWORKYEAR TEACHTIME NEEDDIVERSE NEEDSUBEDU STEM" 
	global SCFAC 	"SIZE STRATIO"  // 2 school context factors
	global SOFAC	"PWORKYEAR PINSLEADS PSCMUTRS PLACKPER PLACKMAT" // 5 school organizational factors

*******With rescaled level-1 weights; conditional weight for teachers*******	
	* #0 Logistic model *
foreach i of var pd1 pd2 pd3 pd4 pd5 pd6 pd7 pd8 {
	logit `i' $TFAC $SCFAC $SOFAC [pweight=TCHWGT], or robust cl(IDSCHOOL) // unconditional level-1 weight
	estimates store `i'm0
	* #1 Multilevel Logistic model *
	meglm `i' [pw=TCHWGT/SCHWGT], family(bernoulli) link(logit) ||IDSCHOOL:, pweight(SCHWGT) or 
	estimates store `i'm1
	meglm `i' $TFAC [pw=TCHWGT/SCHWGT], family(bernoulli) link(logit) ||IDSCHOOL:, pweight(SCHWGT) or
	estimates store `i'm2
	meglm `i' $TFAC $SCFAC [pw=TCHWGT/SCHWGT], family(bernoulli) link(logit) ||IDSCHOOL:, pweight(SCHWGT) or
	estimates store `i'm3
	meglm `i' $TFAC $SCFAC $SOFAC [pw=TCHWGT/SCHWGT], family(bernoulli) link(logit) ||IDSCHOOL:, pweight(SCHWGT) or
	estimates store `i'm4
	outreg2 [`i'm0 `i'm1 `i'm2 `i'm3 `i'm4] using "meglmResults_SGP_conwt.xls", append dec(3) eform 
	}

*******With unconditional level-1 and unconditional level-2 weights*******	
	* #0 Logistic model *
foreach i of var pd1 pd2 pd3 pd4 pd5 pd6 pd7 pd8 {
	logit `i' $TFAC $SCFAC $SOFAC [pweight=TCHWGT], or robust cl(IDSCHOOL) // unconditional level-1 weight
	estimates store `i'm0
	* #1 Multilevel Logistic model *
	melogit `i' [pw=TCHWGT] ||IDSCHOOL:, pweight(SCHWGT) or 
	estimates store `i'm1
	melogit `i' $TFAC [pw=TCHWGT] ||IDSCHOOL:, pweight(SCHWGT) or
	estimates store `i'm2
	melogit `i' $TFAC $SCFAC [pw=TCHWGT] ||IDSCHOOL:, pweight(SCHWGT) or
	estimates store `i'm3
	melogit `i' $TFAC $SCFAC $SOFAC [pw=TCHWGT] ||IDSCHOOL:, pweight(SCHWGT) or
	estimates store `i'm4
	outreg2 [`i'm0 `i'm1 `i'm2 `i'm3 `i'm4] using "LogitResults_OR_SGP_unconwt.xls", append dec(3) eform */
	}
	

log close

translate final project_0413.log final project_0413_log.pdf

exit, clear
