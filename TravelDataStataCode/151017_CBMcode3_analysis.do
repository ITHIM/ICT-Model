clear
clear matrix
cd XXXX  // CHANGE TO WHERE FILES ARE SAVED
	
x	

	***************************
	* ODDS CYCLING IN TWO STAGES
	***************************	
		* UK
		use "2015_PCTmodel\1b_datacreated\NTSEngland20082012.dta", clear
			bysort individualid: egen cyclist=max(trip_maincycle)
			*recode cyclist 0=1 if bicyclefreq <= 4 //monthly
			*recode cyclist 0=1 if bicyclefreq <= 2 //weekly
			*recode cyclist 0=1 if bicyclefreq <= 1 //several times a week
			bysort individualid: gen flag_individ=_n
			recode flag_individ 2/max=.
			keep if agecat8>=2 
			gen trip_dist_mile=(round(trip_distraw_mile))
			*recode trip_dist_mile 7/9=8 10/12=11 13/15=14 16/20=18 21/max=., gen(trip_distcat)
			recode trip_dist_mile 7/9=8 10/12=11 13/15=14 16/20=18 21/30=25 31/40=35 41/max=., gen(trip_distcat)
			gen type4=.
			recode type4 .=1 if male==0 & agecat8>=2 & agecat8<=5
			recode type4 .=2 if male==0 & agecat8>=6
			recode type4 .=3 if male==1 & agecat8>=2 & agecat8<=5
			recode type4 .=4 if male==1 & agecat8>=6
		
			bysort type4: sum cyclist if flag_individ==1
			sum cyclist if flag_individ==1
			
			bysort type4: tab trip_distcat if cyclist==1, sum(trip_maincycle)
			tab trip_distcat if cyclist==1, sum(trip_maincycle)
				sum trip_maincycle if cyclist==1 & type==2 & trip_distcat>=4 & trip_distcat<=5 //smothing older females
				sum trip_maincycle if cyclist==1 & type==4 & trip_distcat>=5 & trip_distcat<=6 //smothing older males

		* DUTCH EBIKE - 2013
		use "2015_PCTmodel\1b_datacreated\Ovin_20102013.dta", clear
			keep if year==2013
			/*bysort individualid: egen cyclist=max(trip_maincycle)
			gen random=uniform()
			recode cyclist 0=1 if random < 0.5 & nedownbicycle==1
				* 50% of people with bikes are reclassified as cyclists
			*/
			bysort individualid: egen ebiker=max(trip_mainelectricbike)
			recode ebiker 0=1 if nedownebike==1
			bysort individualid: gen flag_individ=_n
			recode flag_individ 2/max=.
			keep if agecat8>=2 
			gen trip_dist_mile=(round(trip_distraw_mile))
		*	recode trip_dist_mile 7/9=8 10/12=11 13/15=14 16/20=18 21/max=., gen(trip_distcat)
			recode trip_dist_mile 7/9=8 10/12=11 13/15=14 16/20=18 21/30=25 31/40=35 41/max=., gen(trip_distcat)
			recode trip_distcat 5=4 18=14 , gen(trip_distcat2)
		
			sum ebiker if flag_individ==1
		
			tab trip_distcat2 if ebiker==1, sum(trip_maincycle)
			tab trip_distcat2 if ebiker==1 & trip_maincycle==1, sum(trip_mainelectricbike)
	

