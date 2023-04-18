*************************************
///   AKSOY & WIERTZ (ESR 2023)   ///
///       DATA PREPARATION        ///
*************************************



	/* Table of contents of this file: 

		1.  Downloading the data and set-up; 
		2.  Preparation of wave-specific data files; 
		3.  Preparation of interviewer data; 
		4.  Stacking up all files; 
		5.  Selecting a subset of variables; 
		6.  Preparing key variables; 
		7.  Adding geographic information; 
		8.  Saving the resulting data file. 	*/ 
		
		
		
		
*********************************************
///   1. Downloading the data and set-up  ///
*********************************************


	/* The core data from BHPS waves 1-18 and UKHLS waves 1-12 can be 
	   downloaded via the UK Data Service (study number 6614): 
	   
	   https://doi.org/10.5255/UKDA-SN-6614-18			*/
   
   		cd "..."

		set maxvar 15000


	
******************************************************
///   2. Preparation of wave-specific data files   ///
******************************************************


	/* For each wave separately, we merge the data from the individual 
	   and household questionnaires, we rename all variables, and we 
	   add a wave identifier variable: */    
  
	/* BHPS: */ 
	
		foreach x in a b c d e f g h i j k l m n o p q r { 
		  
		  use "b`x'_indresp", clear 
		 
		  merge m:1 b`x'_hidp using "b`x'_hhresp", ///
						gen(merge_bhps_`x'1) update 
		  drop if merge_bhps_`x'1==2
		  drop merge_bhps_`x'1
		  
		  merge m:1 b`x'_hidp using "b`x'_hhsamp", ///
						gen(merge_bhps_`x'2) update 
		  drop if merge_bhps_`x'2==2
		  drop merge_bhps_`x'2

		  rename b`x'_* *
		  
		  gen wave = strpos("abcdefghijklmnopqr", "`x'")
		  lab var wave "Survey wave number (BHPS & UKHLS combined)"

		  tempfile bhps_`x'
		  save "`bhps_`x''", replace

		} 

	/* UKHLS: */ 
	
		foreach x in a b c d e f g h i j k l { 

		  use "`x'_indresp", clear 

		  merge m:1 `x'_hidp using "`x'_hhresp", ///
						gen(merge_ukhls_`x'1) update
		  drop if merge_ukhls_`x'1==2
		  drop merge_ukhls_`x'1

  		  merge m:1 `x'_hidp using "`x'_hhsamp", ///
						gen(merge_ukhls_`x'2) update
		  drop if merge_ukhls_`x'2==2
		  drop merge_ukhls_`x'2

		  rename `x'_* *

		  gen wave = strpos("abcdefghijkl", "`x'") + 18 
		  lab var wave "Survey wave number (BHPS & UKHLS combined)"

		  tempfile ukhls_`x'
		  save "`ukhls_`x''", replace

		} 


		
**********************************************
///   3. Preparation of interviewer data   ///
**********************************************


	/* Interviewer characteristics were provided as standard part of the 
	   harmonized longitudinal data files up to UKHLS wave 9. For more 
	   recent versions of these interviewer characteristics files, one 
	   needs to apply for special licence access (via the UK Data Service; 
	   study number 8579: https://doi.org/10.5255/UKDA-SN-8579-4.	*/ 

	/* BHPS: */ 
	
		use "xivdata_bh", clear

		renvars ivid_bh - ivyend_bh, postdrop(3)
		format ivid %8s
		drop ivno ivyend

		tempfile xivdata_bh_formerge
		save "`xivdata_bh_formerge'", replace

	/* UKHLS: */ 
	
		use "xivdata", clear

		rename sex ivsex
		rename yearofbirth ivyob
		rename yearstarted ivystrt
		rename racel_dv ivrace
		rename intnum ivid
		tostring ivid, replace
		drop oparea veteran

		tempfile xivdata_formerge
		save "`xivdata_formerge'", replace

	/* Combining BHPS & UKHLS: */ 
	
		use "`xivdata_bh_formerge'", clear

		append using "`xivdata_formerge'"

		tempfile xivdata_bhps_ukhls
		save "`xivdata_bhps_ukhls'", replace


/* Now we can append all the wave-specific data files, generating a panel
   dataset in long format. We also merge in information from XWAVEDAT on 
   any traits that are supposed to remain stable over the life course. In 
   addition, we add the XIVDATA files, containing information on interviewer
   characteristics.  */
   
   

************************************
///   4. Stacking up all files   ///
************************************


		use "`bhps_a'", clear

		foreach x in b c d e f g h i j k l m n o p q r { 

		  append using "`bhps_`x''"
		  
		}

		foreach x in a b c d e f g h i j k l { 

		  append using "`ukhls_`x''"

		}

		merge m:1 pidp using "xwavedat", ///
					gen(merge_xwavedat) update replace 

		codebook ivid
		codebook ivid if wave<=18 
		codebook ivid if wave>18 & wave<=30
		format ivid %8s
		codebook intnum 
		codebook intnum if wave<=18
		codebook intnum if wave>18 & wave<=30
		codebook intnum if wave>18 & wave<=30 & intnum>=0
		replace intnum = . if intnum<0
		tostring intnum, replace
		replace ivid = intnum if wave>18 & wave<=30
		codebook ivid

		merge m:1 ivid using "`xivdata_bhps_ukhls'", ///
					gen(merge_xivdata) update replace
		drop merge_xivdata 
		
		sort pidp wave 

		tempfile bhps_ukhls_19912021_merged_long
		save "`bhps_ukhls_19912021_merged_long'", replace



**********************************************
///   5. Selecting a subset of variables   ///
**********************************************


	/* We select a generous subset of variables, most of which are being 
	   used in our main or supplementary analyses: */ 

		use "`bhps_ukhls_19912021_merged_long'", clear
	
		keep pidp hidp wave gor_dv urban_dv country ///
				oprlg* nirel prayfreq rl* ivcoop iv4 sctrust ///
				ivlieng ivintlang ivinfnce iv2 iv6* ///
				ivaffct* ivprsnt undqus susp ///
				ivtnc ivid ivsex ivyob ivystrt ivrace ///
				lact* org* volun volfreq volhrs ///
				frna frnb ssup* xsup* ///
				ukborn plbornc generation racel_dv ///
				sex age_dv mastat* hiqual_dv ///
				tenure_dv jbstat fihhmn fihhmnnet1_dv ///
				lfsato sclfsato nch* nkids* hlstat hllt ///
				movest plnew ivio* origadd ff_ivlolw newper adcts ///
				opngbhe scopngbhe whoru* scwhoru* sim* ethid* ///
				aidhh aidhrs aidxhh aidhu1 aidhu2 aidhu1a aidhu2a ///
				naidxhh charam charfreq chargv nettalk* nbrcoh3 /// 
				indscus_xw indscus_lw psu psu_bh strata strata_bh 

		mvdecode _all, mv(-10(1)-1)

		drop if wave>30 

		tempfile bhps_ukhls_19912021_selection
		save "`bhps_ukhls_19912021_selection'", replace



**************************************
///   6. Preparing key variables   ///
**************************************


	/* We focus here on the key variables used in the analyses reported 
	   in the main text of the paper. Codings of other variables used in 
	   our (supplementary) analyses are available upon request. */ 
	   

		use "`bhps_ukhls_19912021_selection'", clear


	/* Generalized trust: */

		codebook sctrust
		rename sctrust trust_3cat
		lab var trust_3cat "Generalized trust, 3 categories"
		recode trust_3cat (1 = 1 "Yes") (nonm = 0 "No"), into(trust)
		lab var trust "Whether most people can be trusted"
		tab trust_3cat trust, miss


	/* Cooperation with interviewer: */

		codebook ivcoop iv4
		tab iv4 ivcoop, miss
		gen ivcoop_comb = . 
		replace ivcoop_comb = iv4 if wave<=18
		replace ivcoop_comb = ivcoop if wave>=19 & wave<=30
		lab var ivcoop_comb "Combination of IV4 and IVCOOP"
		recode ivcoop_comb (1 = 4 "Very good") (2 = 3 "Good") ///
							(3 = 2 "Fair") (4 5 = 1 "Poor"), ///
								into(cooperation)
		lab var cooperation "Perceived interviewee cooperation, 4 categories"
		tab ivcoop_comb cooperation, miss


	/* Volunteering: */

	 
		/* BHPS: */

		codebook lactl if wave<19
		vreverse lactl, gen(volunt)
		lab var volunt "Frequency of doing voluntary work"

		/* UKHLS: */

		codebook volun 
		recode volun (2 = 0 "No") (1 = 1 "Yes") (nonm = .), into(volun_u)
		lab var volun_u "Dummy variable for doing voluntary work (UKHLS)"
		tab volun volun_u, miss

		tab volfreq volun, miss
		tab volfreq if volfreq>0
		vreverse volfreq, gen(volunt_u)
		replace volunt_u = . if volunt_u==1
		replace volunt_u = 1 if volun_u==0
		lab define VOLUNT_U 1 "Never" 2 "One-off" 3 "Few times" ///
					4 "Quite often" 5 "Monthly" 6 "Fortnightly" ///
					7 "Weekly" 8 "Twice-weekly" 9 "Thrice-weekly"
		lab val volunt_u VOLUNT_U
		lab var volunt_u "Frequency of doing voluntary work (UKHLS)"
		tab volunt_u

		recode volunt_u (1 = 1 "Never") (2 = 2 "Once a year") ///
						(3 4 = 3 "A few times a year") ///
						(5 6 = 4 "At least once a month") ///
						(7/9 = 5 "At least once a week"), into(volunt_uB)
		/* Treating "seasonal" as "quite often but not regularly": */ 
		replace volunt_uB = 3 if volfreq==9
		lab var volunt_uB "Frequency of voluntary work (UKHLS, on BHPS scale)"
		tab volunt_u volunt_uB


	/* Willing to improve neighbourhood: */ 

		codebook opngbhe scopngbhe
		vreverse opngbhe, gen(nbhimprov_b)
		vreverse scopngbhe, gen(nbhimprov_u)
		lab var nbhimprov_b "Willing to improve neighbourhood (BHPS)"
		lab var nbhimprov_u "Willing to improve neighbourhood (UKHLS)"
		tab opngbhe nbhimprov_b, miss
		tab scopngbhe nbhimprov_u, miss


	/* Religiosity: */

		tab wave oprlg, miss
		tab wave oprlg if gor_dv==12, miss
		recode oprlg (1 = 1 "Yes") (2 = 0 "No"), into(religious)
		lab var religious "Whether one is religious"
		tab oprlg religious, miss
		tab wave religious if gor_dv==12, miss /* BHPS w18 missing. */
		replace religious = 0 if oprlg5==12 | oprlg5==16
		replace religious = 1 if oprlg5!=12 & oprlg5!=16 & oprlg5!=. 
		tab wave religious if gor_dv==12, miss 


	/* Religious belonging: */

		tab oprlg1 oprlg1_bh, miss 
		labelbook ba_oprlg1
		lab define RELTRAD 0 "None" 1 "Anglican" 2 "Catholic" 3 "Protestant" ///
							4 "Other Christian" 5 "Muslim" 6 "Hindu" 7 "Other"
		recode oprlg1 (2 6 = 1) (3 = 2) (4 5 7/9 = 3) (10 11 = 4) (12 = 5) ///
						(13 = 6) (nonm = 7), into(reltrad)
						
		replace reltrad = 3 if wave==14 & oprlg1_bh>=17 & oprlg1_bh<=21

		replace reltrad = 2 if wave==11 & oprlg1_bh==17
		replace reltrad = 3 if wave==11 & oprlg1_bh>=18 & oprlg1_bh<=24
		replace reltrad = 4 if wave==11 & oprlg1_bh==25
		replace reltrad = 7 if wave==11 & oprlg1_bh==26 | oprlg1_bh==27

		replace reltrad = 2 if oprlg5==1 
		replace reltrad = 3 if oprlg5>=2 & oprlg5<=8
		replace reltrad = 4 if oprlg5==9
		replace reltrad = 5 if oprlg5==13 
		replace reltrad = 7 if oprlg5==10 | oprlg5==11 | oprlg5==15 
		replace reltrad = 0 if oprlg5==12 | oprlg5==16

		replace reltrad = 2 if nirel==1
		replace reltrad = 3 if nirel>=2 & nirel<=8 
		replace reltrad = 4 if nirel==9 
		replace reltrad = 5 if nirel==13
		replace reltrad = 6 if nirel==11
		replace reltrad = 7 if nirel==10 | nirel==12 | nirel==14 | nirel==97

		replace reltrad = 0 if religious==0 
		 
		lab val reltrad RELTRAD
		lab var reltrad "Religius tradition one belongs to"
		tab oprlg1 reltrad, miss
		tab oprlg1 reltrad, col 
	   
		recode gor_dv (1/9 = 1 "England") (10 = 2 "Wales") ///
						(11 = 3 "Scotland") (12 = 4 "Northern Ireland") ///
						(nonm = .), into(COUNTRY)
		tab wave COUNTRY, row nofreq
		tab COUNTRY if wave>18
		tab reltrad COUNTRY if (wave==1 | wave==7 | wave==9 | ///
							wave==14 | wave==18 | wave==19 | wave==22 | ///
							wave==26), col nofreq


	/* Religious behaviour: */

		recode oprlg2 (1 = 3 "Every week") (2 = 2 "Every month") ///
						(3 = 1 "Every year") (4 5 = 0 "Never"), into(relattend)
		lab var relattend "Frequency of religious service attendance"
		tab oprlg2 relattend, miss
		tab wave relattend, miss


	/* Importance of religion: */

		tab relattend oprlg3 if wave<=18, miss
		tab relattend oprlg3 if wave>18, miss /* Different coding for BHPS & UKHLS. */

		lab define RELIMP 0 "No importance" 1 "Little importance" ///
						2 "Some importance" 3 "Great importance"
		gen relimp = . 
		replace relimp = 0 if oprlg3==4 
		replace relimp = 1 if (oprlg3==1 & wave<=18) | (oprlg3==3 & wave>18)
		replace relimp = 2 if oprlg3==2
		replace relimp = 3 if (oprlg3==3 & wave<=18) | (oprlg3==1 & wave>18)
		lab val relimp RELIMP
		lab var relimp "Importance of religious beliefs in one's life"
		tab oprlg3 relimp if wave<=18, miss
		tab oprlg3 relimp if wave>18, miss


		save "`bhps_ukhls_19912021_selection'", replace



********************************************
///   7. Adding geographic information   ///
********************************************


	/* Information on households' LSOA of residence is available via 
	   the UK Data Service, study number 6670: 
	   
	   https://doi.org/10.5255/UKDA-SN-6670-14			*/ 

	   
	/* Merging together all LSOA information: */ 
	   
		foreach x in a b c d e f g h i j k l m n o p q r { 
			use b`x'_lsoa01, clear
			drop b`x'_hid
			renvars b`x'_hidp b`x'_lsoa01, presub(b`x'_ )
			gen wave = strpos("abcdefghijklmnopqr", "`x'")
			tempfile b`x'_lsoa01_formerge
			save "`b`x'_lsoa01_formerge'", replace
		}
		foreach x in a b c d e f g h i j k l { 
			use `x'_lsoa01, clear
			renvars `x'_hidp `x'_lsoa01, presub(`x'_ )
			gen wave = strpos("abcdefghijkl", "`x'") + 18
			tempfile `x'_lsoa01_formerge
			save "``x'_lsoa01_formerge'", replace
		}
		use "`ba_lsoa01_formerge'", clear
		foreach x in b c d e f g h i j k l m n o p q r { 
			append using "`b`x'_lsoa01_formerge'"
		} 
		foreach x in a b c d e f g h i j k l { 
			append using "``x'_lsoa01_formerge'"
		} 
		tab wave if lsoa01!=""
		codebook lsoa01 if wave==19
		codebook lsoa01 if wave==30
		tempfile lsoa01_allwaves
		save "`lsoa01_allwaves'", replace 

		
	/* Adding the LSOA information to the main dataset: */ 
	
		use "`bhps_ukhls_19912021_selection'", clear 
		merge m:1 hidp wave using "`lsoa01_allwaves'"
		keep if _merge!=2
		drop _merge


	/* Adding a moving indicator: */ 

		/* Notice that different waves require different treatments: 

		   - BHPS w1 and UKHLS w1: only contain new entrants (so no movers);
		   
		   - BHPS w2-w18: core information contained in the derived 
		     individual-level variable MOVEST, and we can do cross-checks 
			 with further information contained in the individual-level 
			 variable PLNEW;
		   
		   - UKHLS w2-w12: movers defined only based on whether they have 
		     been interviewed previously and whether the household interview 
			 takes place at the same place as the last interview (ORIGADD). */
		
		sort pidp wave 
		bysort pidp: gen interview_id = _n
		lab var interview_id "Within-respondent interview number"
		tab wave interview_id

		gen interview_gap = wave - wave[_n-1] if pidp==pidp[_n-1]
		tab interview_gap
		tab wave interview_gap, row
		/* For the vast majority of all observations (about 97-99% for the 
		   BHPS waves), the gap between waves is only 1. For the UKHLS waves 
		   these numbers are a bit lower; for UKHLS w2 we can see the BHPS 
		   respondents are reinserted. */
 
		/* Let us now first deal with the BHPS respondents: */

			lab define dummy_var 0 "No" 1 "Yes" 

			/* Basic indicator: */ 
			gen mover = . 
			replace mover = 0 if interview_id!=1 & (movest==1 | movest==2) 
			replace mover = 1 if interview_id!=1 & movest==2
			lab var mover "Whether one has moved since last wave (BHPS)"
			lab val mover dummy_var 

			/* Conservative indicator: */
			gen mover2 = mover
			replace mover2 = . if mover==1 & plnew==1
			replace mover2 = . if mover==0 & plnew==2
			lab var mover2 "Whether moved since last wave (BHPS, conservative)"
			lab val mover2 dummy_var 

			/* The UKHLS restricts its moving-related questions to those who 
			   gave a full interview in the previous wave. Let's apply a 
			   similar restriction here, for consistency. We first need to 
			   fill in the blanks on the IVIOLW variable: */
			des ivio*
			tab wave iviow1
			replace iviolw = 1 if iviow1==1 & wave==2
			replace iviolw = 1 if iviow2==1 & wave==3
			replace iviolw = 1 if iviow3==1 & wave==4
			replace iviolw = 1 if iviow4==1 & wave==5
			replace iviolw = 1 if iviow5==1 & wave==6
			replace iviolw = 1 if iviow6==1 & wave==7
			replace iviolw = 1 if iviow7==1 & wave==8
			replace iviolw = 1 if iviow8==1 & wave==9
			tab wave iviolw, miss
			tab wave iviolw if interview_id!=1 & wave>=2 & wave<19, miss
			tab iviolw mover if interview_id!=1 & wave>=2 & wave<19, miss
			tab iviolw mover2 if interview_id!=1 & wave>=2 & wave<19, miss

			/* Consistent indicator: */ 
			gen mover2_lw = mover2
			replace mover2_lw = . if iviolw!=1 
			lab var mover2_lw "Whether moved since last wave (BHPS, conservative & full interview in last wave)"
			lab val mover2_lw dummy_var 
			tab mover2 mover2_lw if interview_id!=1 & wave>=2 & wave<19, miss

		/* Let us now deal with the UKHLS respondents: */  
   
			gen mover_us = 0 if (wave>=20 & wave<=30) & ///
									interview_id!=1 & ff_ivlolw==1 & ///
									(origadd==1 | origadd==2)
			replace mover_us = 1 if (wave>=20 & wave<=30) & ///
								interview_id!=1 & ff_ivlolw==1 & origadd==2
			lab var mover_us "Whether one has moved since last wave (UKHLS)"
			lab val mover_us dummy_var 
			tab wave mover_us, miss row

		/* We then combine the BHPS and UKHLS indicators: */ 

			gen move = . 
			replace move = mover2_lw if wave>=2 & wave<19
			replace move = mover_us if wave>20 & wave!=. 
			lab val move dummy_var
			lab var move "Overall moving indicator (combining BHPS and UKHLS)"
			tab wave move, row nofreq
			tab wave move if wave>=2 & wave<19, row
			tab wave move if wave>19 & wave!=., row

		/* Distinguishing intra-LSOA and inter-LSOA moves: */ 
		
			sort pidp wave 
			gen movelsoa = move
			replace movelsoa = 2 if move==1 & lsoa01!=lsoa01[_n-1] & ///
										pidp==pidp[_n-1]
			lab define movelsoa 0 "No move" 1 "Intra-LSOA move" ///
									2 "Inter-LSOA move"
			lab val movelsoa movelsoa
			lab var movelsoa "Overall moving indicator - intra vs. inter LSOA"

   
   
*********************************************
///   8. Saving the resulting data file   ///
*********************************************


		save "bhps_ukhls_19912021_selection_incl_geography", replace 
   
   
 
