	
	*************************************************************************
	*																	    *
	*																		*
	*				Road coding and irap conversion						    *
	*																		*
	*************************************************************************
	*************************************************************************
	
	/* The purpose of this do file is to use the output from SurveyCTO 
		and convert it into irap measurable variables
		
		USES:				road_coding_v5_WIDE.csv
			
		CREATES:			road_coding_irap.dta
							road_coding_irap.csv
		
		WRITTEN BY: 		Meyhar Mohammed
		
		LAST DATE MODIFIED: 10 July 2019
		
		*/
		
		*********************************************************************
		
		** Set globals for user
		
		clear all
		set more off
		set trace off
																		** Rob
		
		if "`c(username)'" == "" {
		
				global raw ""
				
	   
		}
																		** Meyhar
		
		else if "`c(username)'" == "meyhar" {
		
				global raw "/Users/meyhar/Dropbox/DIME_Meyhar/Kenya/roadcrash-survey/Data"
				
			
		
		}
		
		********************************************************************
		
		
		** Load raw data
		
		import delimited "$raw/RawData/Hotspot Survey/road_coding_v5_WIDE.csv", clear
		
	
