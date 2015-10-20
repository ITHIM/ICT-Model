clear
clear matrix
cd XXXX  // CHANGE TO WHERE FILES ARE SAVED

	/*************
	* AT MINUTES PER STAGE
	*************
		use "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\trip.dta", clear
		keep SurveyYear IndividualID HouseholdID PSUID TripID TripID MainMode_B04ID 
		merge 1:m SurveyYear IndividualID HouseholdID PSUID TripID using "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\stage.dta", keepus(StageMode_B04ID STTXSC) nogen
			drop if _m==2
			drop _m
		
		recode MainMode_B04ID 1/6=0 7/11=1 12=0 13=1, gen(ptmode)
		gen walktime=STTXSC
		replace walktime=. if StageMode_B04ID>=2
		gen activetime=STTXSC
		replace activetime=. if StageMode_B04ID>=3
		bysort SurveyYear IndividualID HouseholdID PSUID TripID: egen totalptwalktime=sum(walktime)
		bysort SurveyYear IndividualID HouseholdID PSUID TripID: egen totalactivetime=sum(activetime)
		replace totalptwalktime=. if ptmode!=1 // make missing if non-PT
		replace totalactivetime=. if MainMode_B04ID<=2 // make missing if non-motorised
		replace totalptwalktime=. if totalactivetime>=180 // make missing if >3 hours
		replace totalactivetime=. if totalactivetime>=180 // make missing if >3 hours
		
		keep SurveyYear IndividualID HouseholdID PSUID TripID totalptwalktime totalactivetime
		duplicates drop
		saveold "H:\1 - Phys Act\1-PA main\2015_PCTmodel\1b_datacreated\0temp\NTs_activetravelstages.dta", replace
		*/
	
	*************
	* MERGE & PREPARE NTS
	*************
	/*	import excel "2015_PCTmodel\1a_dataoriginal\NTS_geographical\HHold_LInkedtoCYclingVars.xlsx", sheet("Data") firstrow case(preserve) clear
		compress
		destring, replace
		saveold "2015_PCTmodel\1a_dataoriginal\NTS_geographical\HHold_LInkedtoCYclingVars.dta", replace
		*/
	
		use "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\individual.dta", clear	
		drop if W1==0
			* NOT IN THE DIARY SAMPLE
		keep SurveyYear IndividualID HouseholdID PSUID Age_B01ID Sex_B01ID W2 /*
			*/ BicycleFreq_B01ID DrivLic_B02ID CarAccess_B02ID NSSec_B03ID WkMode_B01ID MobDiffSum_B01ID EthGroupTS_B02ID PrbJob_B01ID PrbNow_B01ID
		merge 1:m SurveyYear IndividualID HouseholdID PSUID using "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\trip.dta", keepus(TripStart_B01ID TripID MainMode_B04ID TripPurpose_B01ID TripTotalTime TripDisIncSW JJXSC W5) 
			recode _merge 2=0, gen(zerotrips)
			drop _merge
		merge 1:1 SurveyYear IndividualID HouseholdID PSUID TripID using "H:\1 - Phys Act\1-PA main\2015_PCTmodel\1b_datacreated\0temp\NTs_activetravelstages.dta"
			drop if _m==2
			drop _m
		*merge 1:m SurveyYear IndividualID HouseholdID PSUID TripID using "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\stage.dta", keepus(W5 W5xHH StageMode_B04ID StageDistance NumParty_B01ID NumPartyAdult_B01ID NumPartyChild_B01ID) nogen
			* IGNORE STAGES FOR NOW - NB STAGES COMPLICATE THINGS AS CAN'T JUST EXPAND TRIPS UP...
			* IF ADD STAGES IN, HAVE TO DO A BETTER WAY OF DOING 'TripID_expand' - base it on tripID + which one of the expanded ones
		merge m:1 SurveyYear HouseholdID PSUID using "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\household.dta", keepus (Settlement2011EW_B03ID Ten1_B02ID HHoldStruct_B02ID HHoldNumChildren NumCarVan /*
															*/ HHIncQDS2012Eng_B01ID HHIncQDS2011Eng_B01ID HHIncQDS2010Eng_B01ID HHIncQDS2009Eng_B01ID HHIncQDS2008Eng_B01ID HHIncQDS2002Eng_B01ID HHIncQDS2003Eng_B01ID HHIncQDS2004Eng_B01ID HHIncQDS2005Eng_B01ID HHIncQDS2006Eng_B01ID HHIncQDS2007Eng_B01ID) 
			drop if _m==2
			drop _m
		merge m:1 HouseholdID using "2015_PCTmodel\1a_dataoriginal\NTS_geographical\HHold_LInkedtoCYclingVars.dta"
			drop if _m==2
			drop _m
		sort PSUID 
		merge m:1 PSUID using "2015_PCTmodel\1a_dataoriginal\NTS_2002-2012\UKDA-5340-stata11\stata11\psu.dta", keepus (PSUGOR_B02ID PSUStatsReg_B01ID)
			drop if _m==2
			drop _m
		rename *, lower
		
	** ENGLAND ONLY
		keep if psugor_b02id <=9
			
	*************
	* VARIABLES
	*************
	** TRIP DISTANCE AND DURATION
		rename tripdisincsw trip_distraw_mile
		rename triptotaltime trip_durationraw_min
	
	** IDENTIFY A) CYCLE TRIP, B) ANY CYCLE PART.  DITTO FOR CAR DRIVING
		recode mainmode_b04id 1=0 2=1 3/max=0, gen(trip_maincycle)
		*recode stagemode_b04id 1=0 2=1 3/max=0, gen(stage_cycle)
		*bysort tripid: egen trip_anycycle=max(stage_cycle)

		recode mainmode_b04id 1/2=0 3/4=1 5/max=0, gen(trip_maincar)
		rename mainmode_b04id trip_mainmode
		
	** MINUTES SPENT WALKING / CYCLING IN STAGES: IN 2012 ADULTS
		total totalactivetime totalptwalktime if surveyyear==2012 & age_b01id>=8
		disp 108123 / 111720 // 97% AT time in stages is walking in PT trips
		
	
	** TRIP PURPOSE
		rename trippurpose_b01id trip_purpose
		recode trip_purpose 2/max=0, gen(trip_commute)
		recode wkmode_b01id -9/-8=. 1/5=0 6=1 7/13=0, gen(commutemode_cycle)
	
		recode tripstart_b01id -8=.
		rename tripstart_b01id trip_starttime
		
	** YEAR + GEOGRAPHIC
		rename surveyyear year
		rename psugor_b02id gor
		rename psustatsreg_b01id statsreg
		recode settlement2011ew_b03id 2=0 3=., gen(urban) 
	
	** DEMOGRAPHIC
		rename age_b01id agecat
		*recode age min/5=1 6/12=2 13=3 14=4 15/16=5 17/max=6 , gen(agecat)
		*label def agecatlab 1 "0-15" 2 "16-29" 3 "30-39" 4 "40-49" 5 "50-64" 6 "65+", modify
		recode agecat min/5=1 6/12=2 13=3 14=4 15=5 16/17=6 18/19=7 20/max=8 , gen(agecat8)
		label def agecat8lab 1 "0-15" 2 "16-29" 3 "30-39" 4 "40-49" 5 "50-59" 6 "60-69" 7 "70-79" 8 "80+", modify
		label val agecat8 agecat8lab
		recode sex_b01id 2=0, gen(male)
		recode ethgroupts_b02id -8=. 1=0 2=1, gen(nonwhite)
		rename hholdnumchildren hhnumchildunder16
	
	** SEP
		forval i=2002/2012 {
		recode hhincqds`i'eng_b01id -10/-1=.
		}
		egen hhincome5=rowmax(hhincqds2002eng_b01id hhincqds2003eng_b01id hhincqds2004eng_b01id hhincqds2005eng_b01id hhincqds2006eng_b01id hhincqds2007eng_b01id hhincqds2008eng_b01id hhincqds2009eng_b01id hhincqds2010eng_b01id hhincqds2011eng_b01id hhincqds2012eng_b01id)
		label def hhincome5lab 1 "1 (poor)" 5 "5 (rich)", modify
		label val hhincome5 hhincome5lab
		recode nssec_b03id min/-1=.
		rename nssec_b03id nssec
		
	** BICYCLE USE
		recode bicyclefreq_b01id  min/-1=.
		rename bicyclefreq_b01id bicyclefreq
		
	** MOBILITY (TRANSPORT JOB PROBLEMS	)
		recode caraccess_b02id -8=. 2=1 3=2 4=3, gen(caraccess3)
		label def caraccess3lab 1 "Access as driver" 2 "Access as non-driver" 3 "No access"
		label val caraccess3 caraccess3lab
		
		rename mobdiffsum_b01id mobdiffsum
		
	** NUMBER IN PARTY  //NEED TO REDO IF ADD THIS BACK IN, AS VARIES BY STAGE LEVEL
	*	recode numparty_b01id min/-1=., gen(numparty)
	*	recode numpartychild_b01id min/-1=., gen(numparty_child)
	*	recode numpartyadult_b01id min/-1=., gen(numparty_adult )
		
	** WEIGHTS
		rename w2 weight_person
		rename w5 weight_trip

	*************
	* FINISH
	*************
	** ORDER
		order individualid householdid psuid tripid jjxsc /*
			*/ year gor statsreg urban weight_person weight_trip /*
			*/ imdmin2010 inc2010 pcycle11 slope /*
			*/ agecat agecat8 male nonwhite /*
			*/ hhincome5 nssec hhnumchildunder16 /*
			*/ bicyclefreq numcarvan caraccess3 mobdiffsum /*
			*/ zerotrips /*
			*/ commutemode_cycle trip_starttime trip_durationraw_min trip_purpose trip_commute trip_distraw_mile trip_mainmode trip_maincar trip_maincycle 
		keep individualid-trip_maincycle
		foreach var of varlist individualid-trip_maincycle {
		label var `var' 
		}
	** DROP STAGE DATA, THEN EXPAND UP FOR SHORT WALKS
		duplicates drop  // drop stage data
		drop if jjxsc==0
		expand jjxsc
		gen tripid_expand=_n
			* expand up for short walk trips on sunday: do this with JJXSC not SSXSC since doesn't matter if there are short walk trips in a trip with another main mode that aren't counted, since these analyses are all about trip modal share
		order tripid_expand,after(tripid)
	** SAVE
		compress
		saveold "2015_PCTmodel\1b_datacreated\NTSEngland20022012.dta", replace
		keep if year>=2005
		saveold "2015_PCTmodel\1b_datacreated\NTSEngland20052012.dta", replace
		keep if year>=2008
		saveold "2015_PCTmodel\1b_datacreated\NTSEngland20082012.dta", replace
