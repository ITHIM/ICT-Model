clear
clear matrix
cd XXXX  // CHANGE TO WHERE FILES ARE SAVED

	***************
	** MERGE OVIN
	***************
		use "1a_dataoriginal\Ovin\Ovin2010v2\OViN2010_Databestand.dta" , clear
		append using "1a_dataoriginal\Ovin\Ovin2011v2\OViN2011_Databestand_revisie.dta" 
		append using "1a_dataoriginal\Ovin\Ovin2012v2\OViN2012_Databestand_revisie.dta" 
		append using "1a_dataoriginal\Ovin\Ovin2013\OViN2013_Databestand.dta" 
		
		keep opid Sted Prov Geslacht Leeftijd Herkomst HHLft1 HHLft2 HHLft3 HHLft4 HHGestInk HHAuto Jaar Maand Dag /*
			*/ OPFiets OPEFiets Verpl VerplID MotiefV KMotiefV AfstV AfstVBL KHvm Reisduur ReisduurBL FactorP FactorV /*
			*/ HvmFiets VertUur
			

	***************
	** MERGE OVIN
	***************
		rename *, lower

		* EXCLUSION CRITERIA
			drop if verplid==.
				* Missing trip id = people who made no trips
			drop if verpl==0
				* Drop the other stages of multi-modal trips
				* NB CHANGE THIS IF DO STAGE LEVEL ANALYSIS
			drop if verpl>=2
				* Drop trips that were 'travel abroad/holiday travel' OR 'travel as your job'
			* drop if motiefv==3
				* Trips that are movement in the course of your job, e.g. taxi driver: But only N=16 after exclusions above
			 drop if afstvbl>0
				* Trip outside Netherlands
	
		* GEOGRAPHIC + DATE
			recode sted 1/4=1 5=0, gen(urban)
			/*	- zeer sterk stedelijk: gemiddelde oad van 2500 of meer adressen per km2;     over 2500 addresses per km2. 
				 - sterk stedelijk: gemiddelde oad van 1500 tot 2500 adressen per km2;            between 1500 and 2500 addresses per km2.
				 - matig stedelijk: gemiddelde oad van 1000 tot 1500 adressen per km2;           between 1000 and 1500 addresses per km2.
				 - weinig stedelijk: gemiddelde oad van 500 tot 1000 adressen per km2;           between 500 and 1000 addresses per km2.
				 - niet stedelijk: gemiddelde oad van minder dan 500 adressen per km2.           less than 500 addresses per km2.
			*/
			
			rename prov nedregion
			
			gen date=mdy(maand,dag,jaar)
			gen year=jaar
		
		* INDIVIDUAL: DEMOGRAPHIC + SEP
			rename opid individualid
			
			rename leeftijd age
			recode age 0/15=1 16/29=2 30/39=3 40/49=4 50/59=5 60/69=6 70/79=7 80/max=8 , gen(agecat8)
			label def agecat8lab 1 "0-15" 2 "16-29" 3 "30-39" 4 "40-49" 5 "50-59" 6 "60-69" 7 "70-79" 8 "80+", modify
			label val agecat8 agecat8lab

			recode geslacht 2=0, gen(male)
			recode herkomst 1/2=0 3=1 4=., gen(nednonwestern)
			
			rename hhgestink nedhhincome
			recode nedhhincome 7=.
			recode hhauto 10=., gen(nednumcar)
			
			egen hhnumchildunder18=rowtotal(hhlft1 hhlft2 hhlft3)
			egen hhnumchildunder12=rowtotal(hhlft1 hhlft2)

			recode opefiets 2/4=., gen(nedownebike)
			recode opfiets 2/4=., gen(nedownbicycle)
			recode nedownbicycle 0=1 if nedownebike==1 
			recode nedownbicycle .=1 if nedownebike==1 
		
		* TRIP DATA
			rename verplid tripid
			
			rename motiefv nedtrip_purpose
			
			recode kmotiefv 2/max=0, gen(trip_commute)
			gen trip_distraw_mile= afstv*0.1*0.621371192
			gen trip_duration_min=reisduur
			gen trip_speed_mph=trip_distraw_mile/(reisduur/60)
			
			recode khvm 1/5=0 6=1 7/8=0 , gen(trip_maincycle)			
			recode hvmfiets 0=0 2=0 3=. , gen (trip_mainelectricbike)  
			replace trip_mainelectricbike=. if age<12
				* NB not requested if less than 12 years old; code 3.  Make variable missing under age 12			
			recode trip_mainelectricbike 0=. if hvmfiets==0 & trip_maincycle==1
				* NB N=2 where inconsistent as to whether trip was cycled
			gen nedweight_trip= factorv /10000
				* Original weight is to scale up to Dutch population - too big for model to handle
			gen nedweight_person= factorv /10000
			
			rename vertuur nedtrip_starthour
			
			
		* ORDER AND SAVE
			order individualid tripid nedregion sted urban year /*
				*/ male age agecat nednonwestern /*
				*/ hhnumchildunder12 hhnumchildunder18 nedhhincome nednumcar nedownbicycle nedownebike nedweight_trip nedweight_person /*
				*/ nedtrip_starthour nedtrip_purpose trip_commute trip_distraw_mile trip_duration_min trip_speed_mph trip_maincycle trip_mainelectricbike
			keep individualid - trip_mainelectricbike
			foreach var of varlist individualid - trip_mainelectricbike {
			label var `var' ""
			}
			compress
			saveold "1b_datacreated\Ovin_20102013.dta", replace
