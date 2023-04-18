*************************************
///   AKSOY & WIERTZ (ESR 2023)   ///
///        DATA ANALYSES          ///
*************************************



	/* Table of contents of this file: 
	
		1.  Loading the data; 
		2.  Creating religious tradition variables; 
		3.  Keeping the variables that we will use; 
		4.  Recoding and relabelling of key variables; 
		5.  Reshaping the dataset to wide format; 
		6.  Generalized trust analyses (BHPS); 
		7.  Volunteering analyses (BHPS); 
		8.  Perceived cooperativeness analyses (BHPS); 
		9.  Volunteering analyses (UKHLS); 
		10. Perceived cooperativeness analyses (UKHLS); 
		11. Coefficient plots summarizing results. 		*/ 


		
		
*******************************
///   1. Loading the data   ///
*******************************


		cd "..."
		
		use "bhps_ukhls_19912021_selection_incl_geography", clear 
		
		set more off 
   
   

*****************************************************
///   2. Creating religious tradition variables   ///
*****************************************************


		egen pid_tag = tag(pidp)
		lab var pid_tag "Selects one case per respondent"

		bysort pidp: egen tot_nonmiss = total(reltrad!=.)
		lab var tot_nonmiss "# times respondent's religion is observed"
		recode tot_nonmiss (0 = 0 "No") (nonm = 1 "Yes"), into(ever_nonmiss)
		lab var ever_nonmiss "Whether respondent's religion is ever observed"
		tab ever_nonmiss if pid_tag==1 

		bysort pidp: egen tot_none = total(reltrad==0)
		lab var tot_none "# times respondent is observed as None"
		recode tot_none (0 = 0 "No") (nonm = 1 "Yes"), into(ever_none)
		replace ever_none = . if ever_nonmiss==0 
		lab var ever_none "Whether respondent is ever observed as None"
		tab ever_none if pid_tag==1

		bysort pidp: egen tot_angl = total(reltrad==1)
		lab var tot_angl "# times respondent is observed as Anglican"
		recode tot_angl (0 = 0 "No") (nonm = 1 "Yes"), into(ever_angl)
		replace ever_angl = . if ever_nonmiss==0 
		lab var ever_angl "Whether respondent is ever observed as Anglican"
		tab ever_angl if pid_tag==1

		bysort pidp: egen tot_prot = total(reltrad==3)
		lab var tot_prot "# times respondent is observed as Protestant"
		recode tot_prot (0 = 0 "No") (nonm = 1 "Yes"), into(ever_prot)
		replace ever_prot = . if ever_nonmiss==0 
		lab var ever_prot "Whether respondent is ever observed as Protestant"
		tab ever_prot if pid_tag==1

		bysort pidp: egen tot_cath = total(reltrad==2)
		lab var tot_cath "# times respondent is observed as Catholic"
		recode tot_cath (0 = 0 "No") (nonm = 1 "Yes"), into(ever_cath)
		replace ever_cath = . if ever_nonmiss==0 
		lab var ever_cath "Whether respondent is ever observed as Catholic"
		tab ever_cath if pid_tag==1

		bysort pidp: egen tot_musl = total(reltrad==5)
		lab var tot_musl "# times respondent is observed as Muslim"
		recode tot_musl (0 = 0 "No") (nonm = 1 "Yes"), into(ever_musl)
		replace ever_musl = . if ever_nonmiss==0 
		lab var ever_musl "Whether respondent is ever observed as Muslim"
		tab ever_musl if pid_tag==1

		bysort pidp: egen tot_hind = total(reltrad==6)
		lab var tot_hind "# times respondent is observed as Hindu"
		recode tot_hind (0 = 0 "No") (nonm = 1 "Yes"), into(ever_hind)
		replace ever_hind = . if ever_nonmiss==0 
		lab var ever_hind "Whether respondent is ever observed as Hindu"
		tab ever_hind if pid_tag==1

		bysort pidp: egen tot_nonmiss2 = total(religious!=.)
		lab var tot_nonmiss2 "# times respondent's religiosity is observed, v2"
		recode tot_nonmiss2 (0 = 0 "No") (nonm = 1 "Yes"), into(ever_nonmiss2)
		lab var ever_nonmiss2 "Whether respondent's religiosity is ever observed, v2"
		tab ever_nonmiss2 if pid_tag==1 

		bysort pidp: egen tot_some = total(religious==1)
		lab var tot_some "# times respondent is observed as religious"
		recode tot_some (0 = 0 "No") (nonm = 1 "Yes"), into(ever_some)
		replace ever_some = . if ever_nonmiss2==0 
		lab var ever_some "Whether respondent is ever observed as religious"
		tab ever_some if pid_tag==1



*****************************************************
///   3. Keeping the variables that we will use   ///
*****************************************************


	/* We focus here only on the analyses reported on in Tables 2 and 3 
	   and Figures 3 and 4 of our paper: */ 
	   
		keep pidp wave ///
			relattend ever_* ///
			trust cooperation volunt volunt_uB 
			
		keep if inrange(wave, 6, 30)


		
********************************************************
///   4. Recoding and relabelling of key variables   ///
********************************************************


		gen time = wave - 6
		drop wave

		rename relattend rat
		replace rat = rat/3 

		rename trust tr

		rename volunt vol
		replace vol = (vol-1)/4
		rename volunt_uB voluB
		replace voluB = (voluB-1)/4

		rename cooperation coo
		replace coo = (coo-1)/3


		
***************************************************
///   5. Reshaping the dataset to wide format   ///
***************************************************


		reshape wide rat tr vol voluB coo, i(pidp) j(time)



************************************************
///   6. Generalized trust analyses (BHPS)   ///
************************************************


	/* Log file: */
	
		log using "bhps_trust", replace text


	/* Base model: */
	
		sem ///
		(tr12  <- rat10@b tr11@c Alpha@1)       ///
		(tr11  <- rat10@b tr9@c  Alpha@1 E11@1) ///
		(tr9   <- rat8@b  tr7@c  Alpha@1 E9@1)  ///
		(tr7   <- rat5@b  tr4@c  Alpha@1 E7@1)  ///
		(tr4   <- rat3@b  tr2@c  Alpha@1 E4@1), ///
		var(e.tr4@0 e.tr7@0 e.tr9@0 e.tr11@0)  ///
		cov(Alpha*(E4 E7 E9 E11)@0) cov(_OEx*(E4 E7 E9 E11)@0) ///
		cov(E4*(E7 E9 E11)@0) ///
		cov(E7*(E9 E11)@0) cov(E9*(E11)@0) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E7*(rat8 rat10)) ///
		cov(E9*(rat10)) ///
		method(mlmv) noxconditional
		estat gof, stats(all)
		est store bhps_tr_full

		matrix T = e(b)

		
	/* Never religious: */
	
		sem ///
		(tr12  <- rat10@b tr11@c Alpha@1)       ///
		(tr11  <- rat10@b tr9@c  Alpha@1 E11@1) ///
		(tr9   <- rat8@b  tr7@c  Alpha@1 E9@1)  ///
		(tr7   <- rat5@b  tr4@c  Alpha@1 E7@1)  ///
		(tr4   <- rat3@b  tr2@c  Alpha@1 E4@1) if ever_some==0, ///
		var(e.tr4@0 e.tr7@0 e.tr9@0 e.tr11@0)  ///
		cov(Alpha*(E4 E7 E9 E11)@0) cov(_OEx*(E4 E7 E9 E11)@0) ///
		cov(E4*(E7 E9 E11)@0) ///
		cov(E7*(E9 E11)@0) cov(E9*(E11)@0) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E7*(rat8 rat10)) ///
		cov(E9*(rat10)) ///
		method(mlmv) noxconditional from(T) ///
		iterate(250) technique(nr 25 bhhh 25) 
		estat gof, stats(all)
		est store bhps_tr_norel

	  
	/* Ever Anglican: */

		sem ///
		(tr12  <- rat10@b tr11@c Alpha@1)       ///
		(tr11  <- rat10@b tr9@c  Alpha@1 E11@1) ///
		(tr9   <- rat8@b  tr7@c  Alpha@1 E9@1)  ///
		(tr7   <- rat5@b  tr4@c  Alpha@1 E7@1)  ///
		(tr4   <- rat3@b  tr2@c  Alpha@1 E4@1) if ever_angl==1, ///
		var(e.tr4@0 e.tr7@0 e.tr9@0 e.tr11@0)  ///
		cov(Alpha*(E4 E7 E9 E11)@0) cov(_OEx*(E4 E7 E9 E11)@0) ///
		cov(E4*(E7 E9 E11)@0) ///
		cov(E7*(E9 E11)@0) cov(E9*(E11)@0) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E7*(rat8 rat10)) ///
		cov(E9*(rat10)) ///
		method(mlmv) noxconditional from(T) ///
		iterate(250) technique(nr 25 bhhh 25) 
		estat gof, stats(all)
		est store bhps_tr_angl


	/* Ever Protestant: */

		sem ///
		(tr12  <- rat10@b tr11@c Alpha@1)       ///
		(tr11  <- rat10@b tr9@c  Alpha@1 E11@1) ///
		(tr9   <- rat8@b  tr7@c  Alpha@1 E9@1)  ///
		(tr7   <- rat5@b  tr4@c  Alpha@1 E7@1)  ///
		(tr4   <- rat3@b  tr2@c  Alpha@1 E4@1) if ever_prot==1, ///
		var(e.tr4@0 e.tr7@0 e.tr9@0 e.tr11@0)  ///
		cov(Alpha*(E4 E7 E9 E11)@0) cov(_OEx*(E4 E7 E9 E11)@0) ///
		cov(E4*(E7 E9 E11)@0) ///
		cov(E7*(E9 E11)@0) cov(E9*(E11)@0) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E7*(rat8 rat10)) ///
		cov(E9*(rat10)) ///
		method(mlmv) noxconditional from(T) ///
		iterate(250) technique(nr 25 bhhh 25) 
		estat gof, stats(all)
		est store bhps_tr_prot
		  
		  
	/* Ever Catholic: */

		sem ///
		(tr12  <- rat10@b tr11@c Alpha@1)       ///
		(tr11  <- rat10@b tr9@c  Alpha@1 E11@1) ///
		(tr9   <- rat8@b  tr7@c  Alpha@1 E9@1)  ///
		(tr7   <- rat5@b  tr4@c  Alpha@1 E7@1)  ///
		(tr4   <- rat3@b  tr2@c  Alpha@1 E4@1) if ever_cath==1, ///
		var(e.tr4@0 e.tr7@0 e.tr9@0 e.tr11@0)  ///
		cov(Alpha*(E4 E7 E9 E11)@0) cov(_OEx*(E4 E7 E9 E11)@0) ///
		cov(E4*(E7 E9 E11)@0) ///
		cov(E7*(E9 E11)@0) cov(E9*(E11)@0) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E7*(rat8 rat10)) ///
		cov(E9*(rat10)) ///
		method(mlmv) noxconditional from(T) ///
		iterate(250) technique(nr 25 bhhh 25) 
		estat gof, stats(all)
		est store bhps_tr_cath
		
	
	/* Log file: */ 
	
		log close



*******************************************
///   7. Volunteering analyses (BHPS)   ///
*******************************************


	/* Log file: */
	
		log using "bhps_volun", replace text


	/* Base model: */
	
		sem ///
		(vol12  <- rat10@b vol10@c Alpha@1)       ///
		(vol10  <- rat8@b  vol8@c  Alpha@1 E10@1) ///
		(vol8   <- rat8@b2 vol6@c  Alpha@1 E8@1)  ///
		(vol6   <- rat5@b  vol4@c  Alpha@1 E6@1)  ///
		(vol4   <- rat3@b  vol2@c  Alpha@1 E4@1)  ///
		(vol2   <- rat1@b  vol0@c  Alpha@1 E2@1), ///
		var(e.vol2@0 e.vol4@0 e.vol6@0 e.vol8@0 e.vol10@0) ///
		cov(Alpha*(E2 E4 E6 E8 E10)@0) cov(_OEx*(E2 E4 E6 E8 E10)@0) ///
		cov(E2*(E4 E6 E8 E10)@0) cov(E4*(E6 E8 E10)@0) ///
		cov(E6*(E8 E10)@0) cov(E8*(E10)@0) ///
		cov(E2*(rat3 rat5 rat8 rat10)) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E6*(rat8 rat10)) ///
		cov(E8*(rat10)) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) 
		estat gof, stats(all)
		est store bhps_vol_full

		matrix V = e(b) 


	/* Never religious: */
	
		sem ///
		(vol12  <- rat10@b vol10@c Alpha@1)       ///
		(vol10  <- rat8@b  vol8@c  Alpha@1 E10@1) ///
		(vol8   <- rat8@b2 vol6@c  Alpha@1 E8@1)  ///
		(vol6   <- rat5@b  vol4@c  Alpha@1 E6@1)  ///
		(vol4   <- rat3@b  vol2@c  Alpha@1 E4@1)  ///
		(vol2   <- rat1@b  vol0@c  Alpha@1 E2@1) if ever_some==0, ///
		var(e.vol2@0 e.vol4@0 e.vol6@0 e.vol8@0 e.vol10@0) ///
		cov(Alpha*(E2 E4 E6 E8 E10)@0) cov(_OEx*(E2 E4 E6 E8 E10)@0) ///
		cov(E2*(E4 E6 E8 E10)@0) cov(E4*(E6 E8 E10)@0) ///
		cov(E6*(E8 E10)@0) cov(E8*(E10)@0) ///
		cov(E2*(rat3 rat5 rat8 rat10)) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E6*(rat8 rat10)) ///
		cov(E8*(rat10)) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(V)
		estat gof, stats(all)
		est store bhps_vol_norel


	/* Ever Anglican: */
	
		sem ///
		(vol12  <- rat10@b vol10@c Alpha@1)       ///
		(vol10  <- rat8@b  vol8@c  Alpha@1 E10@1) ///
		(vol8   <- rat8@b2 vol6@c  Alpha@1 E8@1)  ///
		(vol6   <- rat5@b  vol4@c  Alpha@1 E6@1)  ///
		(vol4   <- rat3@b  vol2@c  Alpha@1 E4@1)  ///
		(vol2   <- rat1@b  vol0@c  Alpha@1 E2@1) if ever_angl==1, ///
		var(e.vol2@0 e.vol4@0 e.vol6@0 e.vol8@0 e.vol10@0) ///
		cov(Alpha*(E2 E4 E6 E8 E10)@0) cov(_OEx*(E2 E4 E6 E8 E10)@0) ///
		cov(E2*(E4 E6 E8 E10)@0) cov(E4*(E6 E8 E10)@0) ///
		cov(E6*(E8 E10)@0) cov(E8*(E10)@0) ///
		cov(E2*(rat3 rat5 rat8 rat10)) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E6*(rat8 rat10)) ///
		cov(E8*(rat10)) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(V)
		estat gof, stats(all)
		est store bhps_vol_angl

		  
	/* Ever Protestant: */
	
		sem ///
		(vol12  <- rat10@b vol10@c Alpha@1)       ///
		(vol10  <- rat8@b vol8@c  Alpha@1 E10@1) ///
		(vol8   <- rat8@b2  vol6@c  Alpha@1 E8@1)  ///
		(vol6   <- rat5@b  vol4@c  Alpha@1 E6@1)  ///
		(vol4   <- rat3@b  vol2@c  Alpha@1 E4@1)  ///
		(vol2   <- rat1@b  vol0@c  Alpha@1 E2@1) if ever_prot==1, ///
		var(e.vol2@0 e.vol4@0 e.vol6@0 e.vol8@0 e.vol10@0) ///
		cov(Alpha*(E2 E4 E6 E8 E10)@0) cov(_OEx*(E2 E4 E6 E8 E10)@0) ///
		cov(E2*(E4 E6 E8 E10)@0) cov(E4*(E6 E8 E10)@0) ///
		cov(E6*(E8 E10)@0) cov(E8*(E10)@0) ///
		cov(E2*(rat3 rat5 rat8 rat10)) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E6*(rat8 rat10)) ///
		cov(E8*(rat10)) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(V)
		estat gof, stats(all)
		est store bhps_vol_prot
		  

	/* Ever Catholic: */
	
		sem ///
		(vol12  <- rat10@b vol10@c Alpha@1)       ///
		(vol10  <- rat8@b vol8@c  Alpha@1 E10@1) ///
		(vol8   <- rat8@b2  vol6@c  Alpha@1 E8@1)  ///
		(vol6   <- rat5@b  vol4@c  Alpha@1 E6@1)  ///
		(vol4   <- rat3@b  vol2@c  Alpha@1 E4@1)  ///
		(vol2   <- rat1@b  vol0@c  Alpha@1 E2@1) if ever_cath==1, ///
		var(e.vol2@0 e.vol4@0 e.vol6@0 e.vol8@0 e.vol10@0) ///
		cov(Alpha*(E2 E4 E6 E8 E10)@0) cov(_OEx*(E2 E4 E6 E8 E10)@0) ///
		cov(E2*(E4 E6 E8 E10)@0) cov(E4*(E6 E8 E10)@0) ///
		cov(E6*(E8 E10)@0) cov(E8*(E10)@0) ///
		cov(E2*(rat3 rat5 rat8 rat10)) ///
		cov(E4*(rat5 rat8 rat10)) ///
		cov(E6*(rat8 rat10)) ///
		cov(E8*(rat10)) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(V)
		estat gof, stats(all)
		est store bhps_vol_cath

		  
	/* Log file: */ 
	
		log close



***********************************************
///   8. Perceived cooperativeness (BHPS)   ///
***********************************************


	/* Log file: */
		
		log using "bhps_coop", replace text


	/* Base model: */
		  
		sem ///
		(coo12  <- rat12@b  coo10@c Alpha@1) ///
		(coo10  <- rat10@b  coo8@c  Alpha@1 E10@1) ///
		(coo8   <- rat8@b   coo5@c  Alpha@1 E8@1)  ///
		(coo5   <- rat5@b   coo3@c  Alpha@1 E5@1)  ///
		(coo3   <- rat3@b   coo1@c  Alpha@1 E3@1)  ///
		(coo1   <- rat1@b   coo0@c  Alpha@1 E1@1), ///
		var(e.coo1@0 e.coo3@0 e.coo5@0 e.coo8@0 e.coo10@0) ///
		cov(Alpha*(E1 E3 E5 E8 E10)@0) cov(_OEx*(E1 E3 E5 E8 E10)@0) ///
		cov(E1*(E3 E5 E8 E10)@0) cov(E3*(E5 E8 E10)@0) cov(E5*(E8 E10)@0) /// 
		cov(E8*E10@0) ///
		cov(E1*(rat3 rat5 rat8 rat10 rat12)) ///
		cov(E3*(rat5 rat8 rat10 rat12)) ///
		cov(E5*(rat8 rat10 rat12)) ///
		cov(E8*(rat10 rat12)) ///
		cov(E10*rat12) ///
		noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) 
		estat gof, stats(all)

		matrix A = e(b)

		sem ///
		(coo12  <- rat12@b  coo10@c Alpha@1) ///
		(coo10  <- rat10@b  coo8@c  Alpha@1 E10@1) ///
		(coo8   <- rat8@b   coo5@c  Alpha@1 E8@1)  ///
		(coo5   <- rat5@b   coo3@c  Alpha@1 E5@1)  ///
		(coo3   <- rat3@b   coo1@c  Alpha@1 E3@1)  ///
		(coo1   <- rat1@b   coo0@c  Alpha@1 E1@1), ///
		var(e.coo1@0 e.coo3@0 e.coo5@0 e.coo8@0 e.coo10@0) ///
		cov(Alpha*(E1 E3 E5 E8 E10)@0) cov(_OEx*(E1 E3 E5 E8 E10)@0) ///
		cov(E1*(E3 E5 E8 E10)@0) cov(E3*(E5 E8 E10)@0) cov(E5*(E8 E10)@0) /// 
		cov(E8*E10@0) ///
		cov(E1*(rat3 rat5 rat8 rat10 rat12)) ///
		cov(E3*(rat5 rat8 rat10 rat12)) ///
		cov(E5*(rat8 rat10 rat12)) ///
		cov(E8*(rat10 rat12)) ///
		cov(E10*rat12) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(A)
		estat gof, stats(all)
		est store bhps_coo_full

		matrix C = e(b)


	/* Never religious: */

		sem ///
		(coo12  <- rat12@b  coo10@c Alpha@1) ///
		(coo10  <- rat10@b  coo8@c  Alpha@1 E10@1) ///
		(coo8   <- rat8@b   coo5@c  Alpha@1 E8@1)  ///
		(coo5   <- rat5@b   coo3@c  Alpha@1 E5@1)  ///
		(coo3   <- rat3@b   coo1@c  Alpha@1 E3@1)  ///
		(coo1   <- rat1@b   coo0@c  Alpha@1 E1@1) if ever_some == 0, ///
		var(e.coo1@0 e.coo3@0 e.coo5@0 e.coo8@0 e.coo10@0) ///
		cov(Alpha*(E1 E3 E5 E8 E10)@0) cov(_OEx*(E1 E3 E5 E8 E10)@0) ///
		cov(E1*(E3 E5 E8 E10)@0) cov(E3*(E5 E8 E10)@0) cov(E5*(E8 E10)@0) /// 
		cov(E8*E10@0) ///
		cov(E1*(rat3 rat5 rat8 rat10 rat12)) ///
		cov(E3*(rat5 rat8 rat10 rat12)) ///
		cov(E5*(rat8 rat10 rat12)) ///
		cov(E8*(rat10 rat12)) ///
		cov(E10*rat12) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(C)
		estat gof, stats(all)
		est store bhps_coo_norel


	/* Ever Anglican: */
	
		sem ///
		(coo12  <- rat12@b  coo10@c Alpha@1) ///
		(coo10  <- rat10@b  coo8@c  Alpha@1 E10@1) ///
		(coo8   <- rat8@b   coo5@c  Alpha@1 E8@1)  ///
		(coo5   <- rat5@b   coo3@c  Alpha@1 E5@1)  ///
		(coo3   <- rat3@b   coo1@c  Alpha@1 E3@1)  ///
		(coo1   <- rat1@b   coo0@c  Alpha@1 E1@1) if ever_angl==1, ///
		var(e.coo1@0 e.coo3@0 e.coo5@0 e.coo8@0 e.coo10@0) ///
		cov(Alpha*(E1 E3 E5 E8 E10)@0) cov(_OEx*(E1 E3 E5 E8 E10)@0) ///
		cov(E1*(E3 E5 E8 E10)@0) cov(E3*(E5 E8 E10)@0) cov(E5*(E8 E10)@0) /// 
		cov(E8*E10@0) ///
		cov(E1*(rat3 rat5 rat8 rat10 rat12)) ///
		cov(E3*(rat5 rat8 rat10 rat12)) ///
		cov(E5*(rat8 rat10 rat12)) ///
		cov(E8*(rat10 rat12)) ///
		cov(E10*rat12) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(C)
		estat gof, stats(all)
		est store bhps_coo_angl

		  
	/* Ever Protestant: */
	
		sem ///
		(coo12  <- rat12@b  coo10@c Alpha@1) ///
		(coo10  <- rat10@b  coo8@c  Alpha@1 E10@1) ///
		(coo8   <- rat8@b   coo5@c  Alpha@1 E8@1)  ///
		(coo5   <- rat5@b   coo3@c  Alpha@1 E5@1)  ///
		(coo3   <- rat3@b   coo1@c  Alpha@1 E3@1)  ///
		(coo1   <- rat1@b   coo0@c  Alpha@1 E1@1) if ever_prot==1, ///
		var(e.coo1@0 e.coo3@0 e.coo5@0 e.coo8@0 e.coo10@0) ///
		cov(Alpha*(E1 E3 E5 E8 E10)@0) cov(_OEx*(E1 E3 E5 E8 E10)@0) ///
		cov(E1*(E3 E5 E8 E10)@0) cov(E3*(E5 E8 E10)@0) cov(E5*(E8 E10)@0) /// 
		cov(E8*E10@0) ///
		cov(E1*(rat3 rat5 rat8 rat10 rat12)) ///
		cov(E3*(rat5 rat8 rat10 rat12)) ///
		cov(E5*(rat8 rat10 rat12)) ///
		cov(E8*(rat10 rat12)) ///
		cov(E10*rat12) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(C)
		estat gof, stats(all)
		est store bhps_coo_prot

		  
	/* Ever Catholic: */
	
		sem ///
		(coo12  <- rat12@b  coo10@c Alpha@1) ///
		(coo10  <- rat10@b  coo8@c  Alpha@1 E10@1) ///
		(coo8   <- rat8@b   coo5@c  Alpha@1 E8@1)  ///
		(coo5   <- rat5@b   coo3@c  Alpha@1 E5@1)  ///
		(coo3   <- rat3@b   coo1@c  Alpha@1 E3@1)  ///
		(coo1   <- rat1@b   coo0@c  Alpha@1 E1@1) if ever_cath==1, ///
		var(e.coo1@0 e.coo3@0 e.coo5@0 e.coo8@0 e.coo10@0) ///
		cov(Alpha*(E1 E3 E5 E8 E10)@0) cov(_OEx*(E1 E3 E5 E8 E10)@0) ///
		cov(E1*(E3 E5 E8 E10)@0) cov(E3*(E5 E8 E10)@0) cov(E5*(E8 E10)@0) /// 
		cov(E8*E10@0) ///
		cov(E1*(rat3 rat5 rat8 rat10 rat12)) ///
		cov(E3*(rat5 rat8 rat10 rat12)) ///
		cov(E5*(rat8 rat10 rat12)) ///
		cov(E8*(rat10 rat12)) ///
		cov(E10*rat12) ///
		method(mlmv) noxconditional ///
		iterate(250) technique(nr 25 bhhh 25) from(C)
		estat gof, stats(all)
		est store bhps_coo_cath


	/* Log file: */
	
		log close



********************************************
///   9. Volunteering analyses (UKHLS)   ///
********************************************


	/* Log file: */ 
	
		log using "ukhls_volun", text replace 


	/* Base model: */ 
	
		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1), ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  
		est store ukhls_vol_full
		estat gof, stats(all)

		matrix Vu = e(b)


	/* Never religious: */ 
	
		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1) if ever_some == 0, ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  from(Vu) 
		est store ukhls_vol_norel 
		estat gof, stats(all)
	  

	/* Ever Anglican: */ 

		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1) if ever_angl == 1, ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  from(Vu)
		est store ukhls_vol_angl
		estat gof, stats(all)
  

	/* Ever Protestant: */ 

		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1) if ever_prot == 1, ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  from(Vu)
		est store ukhls_vol_prot
		estat gof, stats(all)

  
	/* Ever Catholic: */ 
	
		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1) if ever_cath == 1, ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  from(Vu)
		est store ukhls_vol_cath
		estat gof, stats(all)

		
	/* Ever Muslim: */ 

		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1) if ever_musl == 1, ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  from(Vu)
		est store ukhls_vol_musl
		estat gof, stats(all)
	  
	  
	/* Ever Hindu: */ 
	  
		sem ///
		(voluB22  <- rat20@b voluB20@c Alpha@1) ///
		(voluB20  <- rat20@d voluB18@c Alpha@1 E20@1) ///
		(voluB18  <- rat16@b voluB16@c Alpha@1 E18@1) ///
		(voluB16  <- rat16@d voluB14@c Alpha@1 E16@1) ///
		(voluB14  <- rat13@b   vol12@c2 Alpha@1 E14@1) if ever_hind == 1, ///
		var(e.voluB14@0 e.voluB16@0 e.voluB18@0 e.voluB20@0) ///
		cov(Alpha*(E14 E16 E18 E20)@0) cov(_OEx*(E14 E16 E18 E20)@0) ///
		cov(E14*(E16 E18 E20)@0) cov(E16*(E18 E20)@0) cov(E18*(E20)@0) ///
		cov(E14*(rat16 rat20)) ///
		cov(E16*(rat20)) ///
		cov(E18*(rat20)) ///
		method(mlmv)  noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25)  from(Vu)
		est store ukhls_vol_hind
		estat gof, stats(all)


	/* Log file: */ 
	
		log close 
		
		

**********************************************************
///   10. Perceived cooperativeness analyses (UKHLS)   ///
**********************************************************
		

	/* Log file: */ 
	
		log using "ukhls_coop", text replace 
		
		
	/* Base model: */ 
	
		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1),  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional

		matrix B = e(b)

		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1),  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(B)
		est store ukhls_coo_full
		estat gof, stats(all)

		matrix Cu = e(b)


	/* Never religious: */ 

		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1) if ever_some == 0,  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(Cu)
		est store ukhls_coo_norel
		estat gof, stats(all)


	/* Ever Anglican: */ 

		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1) if ever_angl == 1,  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(Cu)
		est store ukhls_coo_angl
		estat gof, stats(all)


	/* Ever Protestant: */ 

		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1) if ever_prot == 1,  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(Cu)
		est store ukhls_coo_prot
		estat gof, stats(all)

  
	/* Ever Catholic: */ 

		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1) if ever_cath == 1,  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(Cu)
		est store ukhls_coo_cath
		estat gof, stats(all)


	/* Ever Muslim: */ 
	  
		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1) if ever_musl == 1,  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(Cu)
		est store ukhls_coo_musl
		estat gof, stats(all)


	/* Ever Hindu: */ 

		sem ///
		(coo22  <- rat24@b2 coo20@c Alpha@1) ///
		(coo20  <- rat20@b  coo16@c Alpha@1 E20@1)       ///
		(coo16  <- rat16@b  coo13@c Alpha@1 E16@1) if ever_hind == 1,  ///
		var(e.coo20@0 e.coo16@0) ///
		cov(Alpha*(E20 E16)@0) cov(_OEx*(E20 E16)@0) ///
		cov(E16*(E20)@0) ///
		cov(E16*(rat20 rat24)) ///
		cov(E20*(rat24)) ///
		noxconditional  method(mlmv)  ///
		iterate(250) technique(nr 25 bhhh 25) skipconditional from(Cu)
		est store ukhls_coo_hind
		estat gof, stats(all)		
		

	
*****************************************************
///   11. Coefficient plots summarizing results   ///
*****************************************************


	/* BHPS: */ 
	
	coefplot (bhps_tr_full, msymbol(S) mcolor(black) ciopts(color(black)))  ///
			 (bhps_tr_norel, msymbol(O) mcolor(black) ciopts(color(black)))  ///
			 (bhps_tr_angl, msymbol(D) mcolor(black) ciopts(color(black)))  ///
			 (bhps_tr_prot, msymbol(T) mcolor(black) ciopts(color(black)))  ///
			 (bhps_tr_cath, msymbol(Sh) mcolor(black) ciopts(color(black))) ///
			 ||  /// 
			 (bhps_vol_full, mcolor(black) ciopts(color(black)))  ///
			 (bhps_vol_norel, mcolor(black) ciopts(color(black)))  ///
			 (bhps_vol_angl, mcolor(black) ciopts(color(black)))  ///
			 (bhps_vol_prot, mcolor(black) ciopts(color(black)))  ///
			 (bhps_vol_cath, mcolor(black) ciopts(color(black))) ///
			 ||  ///
			 (bhps_coo_full, label(Pooled) mcolor(black) ciopts(color(black)))  ///
			 (bhps_coo_norel, label(Never relig) mcolor(black) ciopts(color(black)))  ///
			 (bhps_coo_angl, label(Anglican) mcolor(black) ciopts(color(black)))  ///
			 (bhps_coo_prot, label(Protestant) mcolor(black) ciopts(color(black)))  ///
			 (bhps_coo_cath, label(Catholic) mcolor(black) ciopts(color(black))), ///
		  keep(rat10) ///
		  graphregion(color(white)) plotregion(color(white))   ///
		  xline(0, lcolor(black) lpattern(shortdash) lwidth(vthin)) ///
		  coeflabels(rat10 = " ", wrap(20)) ///
		  legend(r(1)) ///
		  bylabels("Trust" "Volunteering" "Cooperativeness") ///
		  byopts(cols(3) xrescale)
			
	/* UKHLS: */ 
	
	coefplot (ukhls_vol_full, msymbol(S) label(Pooled) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_vol_norel, msymbol(O) label(Never relig) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_vol_angl, msymbol(D) label(Anglican) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_vol_prot, msymbol(T) label(Protestant) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_vol_cath, msymbol(Sh) label(Catholic) mcolor(black) ciopts(color(black))) ///
			 (ukhls_vol_musl, msymbol(Th) label(Muslim) mcolor(black) ciopts(color(black))) ///
			 (ukhls_vol_hind, msymbol(Oh) label(Hindu) mcolor(black) ciopts(color(black))) ///
			 ||  ///
			 (ukhls_coo_full, label(Pooled) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_coo_norel, label(Never relig) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_coo_angl, label(Anglican) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_coo_prot, label(Protestant) mcolor(black) ciopts(color(black)))  ///
			 (ukhls_coo_cath, label(Catholic) mcolor(black) ciopts(color(black))) ///
			 (ukhls_coo_musl, label(Muslim) mcolor(black) ciopts(color(black))) ///
			 (ukhls_coo_hind, label(Hindu) mcolor(black) ciopts(color(black))), ///
		  keep(rat20) ///
		  graphregion(color(white)) plotregion(color(white))   ///
		  xline(0, lcolor(black) lpattern(shortdash) lwidth(vthin)) ///
		  coeflabels(rat20 = " ", wrap(20)) ///
		  legend(r(1)) ///
		  bylabels("Volunteering" "Cooperativeness") ///
		  byopts(cols(2) xrescale)



