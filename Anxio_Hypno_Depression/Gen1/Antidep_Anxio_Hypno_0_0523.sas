* Creating a library where the datasets are stored ;
libname FHS '/home/u60769638/FHS-BAP/FHS-BAP_81_85' ;
libname Cur_Gen1 '/home/u60769638/FHS-BAP/FHS-BAP_81_85' ;

*Importing Medication dataset ;
proc import out= fhs81_85
	datafile='/home/u60769638/FHS-BAP/FHS-BAP_81_85/81_85_abstracted.csv'
	dbms= csv replace;
	getnames=yes;
run;

*Importing Clinical Questions (CDI) dataset ;
proc import out= FHS.clinical_81_85_exams19_32 (rename= (FO481 = cdi_depression_core22
														 FP447 = cdi_depression_core23)) 
	datafile='/home/u60769638/FHS-BAP/FHS-BAP_81_85/81_85_clinical_exams19_32.csv'
	dbms= csv replace;
	getnames=yes;
run;

data FHS.fhs81_85 ;
	* Renaming variables ;
	set work.fhs81_85 (rename = (FB75 = anxiolytics_core9 
										FC92 = anxiolytics_core10 
										FD84 = anxiolytics_core11 
										FE96 = anxiolytics_core12 
										FF98 = anxiolytics_core13 
										FG97 = anxiolytics_core14 
										FH97 = anxiolytics_core15 
										FI56 = anxiolytics_core16
										FJ33 = anxiolytics_core17
										FK122 = anxiolytics_1_core18
										FK123 = anxiolytics_2_core18
										FL155 = anxiolytics_core19
										FM193 = anxiolytics_core20
										FN132 = anxiolytics_core21
										FO140 = anxiolytics_core22
										FP094 = anxiolytics_core23
										FQ177 = anxiolytics_core24
										FR229 = anxiolytics_core25
										FS297 = anxiolytics_core26
										FT304 = anxiolytics_core27
										FG96 = hypnotics_core14
										FH96 = hypnotics_core15
										FI55 = hypnotics_core16
										FJ32 = hypnotics_core17
										FK120 = hypnotics_1_core18
										FK121 = hypnotics_2_core18
										FL156 = hypnotics_core19
										FM194 = hypnotics_core20
										FN133 = hypnotics_core21
										FO141 = hypnotics_core22
										FP095 = hypnotics_core23
										FQ178 = hypnotics_core24
										FR230 = hypnotics_core25
										FS298 = hypnotics_core26
										FT305 = hypnotics_core27
										FJ34 = antidepressants_core17
										FK124 = antidepressants_1_core18
										FK125 = antidepressants_2_core18
										FL157 = antidepressants_core19
										FM195 = antidepressants_core20
										FN134 = antidepressants_core21
										FO142 = antidepressants_core22
										FP096 = antidepressants_core23
										FQ179 = antidepressants_core24
										FR231 = antidepressants_core25
										FS299 = antidepressants_core26
										FT306 = antidepressants_core27
										));

	* Recoding 3:Maybe as 0:No for anxiolytics, hypnotics and antidepressants ;
	array anxiolytics {*} anxiolytics_core17 anxiolytics_1_core18 anxiolytics_core19-anxiolytics_core32 ;
	do i = 1 to dim(anxiolytics) ;
		if anxiolytics{i} = 3 then anxiolytics{i} = 0 ;
		else if anxiolytics{i} = 1 or anxiolytics{i} = 2 then anxiolytics{i} = 1 ;
		else if anxiolytics{i} = 88 then anxiolytics{i} = . ;
	end ;
	
	array anxiolytics_tranq {*} anxiolytics_core9-anxiolytics_core16 ;
	do i = 1 to dim(anxiolytics_tranq) ;
		if anxiolytics_tranq{i} = 1 or anxiolytics_tranq{i} = 2 then anxiolytics_tranq{i} = 1 ;
		else if anxiolytics_tranq{i} = 88 then anxiolytics_tranq{i} = . ;
	end ;
	
	array hypnotics {*} hypnotics_core14-hypnotics_core17 hypnotics_1_core18 hypnotics_core19-hypnotics_core32    ;
	do i = 1 to dim(hypnotics) ;
		if hypnotics{i} = 3 then hypnotics{i} = 0 ;
		else if hypnotics{i} = 1 or hypnotics{i} = 2 then hypnotics{i} = 1 ;
		else if hypnotics{i} = 88 then hypnotics{i} = . ;
	end ;
	
	array antidepressants {*} antidepressants_core17 antidepressants_1_core18 antidepressants_core19-antidepressants_core32;
	do i = 1 to dim(antidepressants) ;
		if antidepressants{i} = 88 then antidepressants{i} = . ;
		else if antidepressants{i} = 3 then antidepressants{i} = 0 ;
	end ;
	
	*Array for character variables ;
	array char {*} anxiolytics_2_core18 hypnotics_2_core18 antidepressants_2_core18  ;
	do i = 1 to dim(char) ;
		if char{i} = '88' then char{i} = . ;
	end ;
	
run ;

*Checking contents of the medication dataset ;
proc contents data = fhs.fhs81_85 ;
run ;


*Summary statistics for the medication variables ;
%macro summarystat (medvar) ;
	proc freq data=FHS.fhs81_85 ;
		tables &medvar ;
		title "Summary statistics for &medvar" ;
	run;
%mend summarystat ;

%summarystat (anxiolytics_core9) ; 
%summarystat (anxiolytics_core10) ;  
%summarystat (anxiolytics_core11) ; 
%summarystat (anxiolytics_core12) ; 
%summarystat (anxiolytics_core13) ;  
%summarystat (anxiolytics_core14) ;  
%summarystat (anxiolytics_core15) ; 
%summarystat (anxiolytics_core16) ; 
%summarystat (anxiolytics_core17) ; 
%summarystat (anxiolytics_1_core18) ; 
%summarystat (anxiolytics_2_core18) ; 
%summarystat (anxiolytics_core19) ; 
%summarystat (anxiolytics_core20) ; 
%summarystat (anxiolytics_core21) ; 
%summarystat (anxiolytics_core22) ; 
%summarystat (anxiolytics_core23) ; 
%summarystat (anxiolytics_core24) ; 
%summarystat (anxiolytics_core25) ; 
%summarystat (anxiolytics_core26) ; 
%summarystat (anxiolytics_core27) ; 
%summarystat (anxiolytics_core28) ; 
%summarystat (anxiolytics_core29) ; 
%summarystat (anxiolytics_core30) ; 
%summarystat (anxiolytics_core31) ; 
%summarystat (anxiolytics_core32) ; 
%summarystat (hypnotics_core14) ; 
%summarystat (hypnotics_core15) ; 
%summarystat (hypnotics_core16) ; 
%summarystat (hypnotics_core17) ; 
%summarystat (hypnotics_1_core18) ; 
%summarystat (hypnotics_2_core18) ; 
%summarystat (hypnotics_core19) ; 
%summarystat (hypnotics_core20) ; 
%summarystat (hypnotics_core21) ; 
%summarystat (hypnotics_core22) ; 
%summarystat (hypnotics_core23) ; 
%summarystat (hypnotics_core24) ; 
%summarystat (hypnotics_core25) ; 
%summarystat (hypnotics_core26) ; 
%summarystat (hypnotics_core27) ; 
%summarystat (hypnotics_core28) ; 
%summarystat (hypnotics_core29) ; 
%summarystat (hypnotics_core30) ; 
%summarystat (hypnotics_core31) ; 
%summarystat (hypnotics_core32) ; 
%summarystat (antidepressants_core17) ; 
%summarystat (antidepressants_1_core18) ; 
%summarystat (antidepressants_2_core18) ; 
%summarystat (antidepressants_core19) ; 
%summarystat (antidepressants_core20) ; 
%summarystat (antidepressants_core21) ; 
%summarystat (antidepressants_core22) ; 
%summarystat (antidepressants_core23) ; 
%summarystat (antidepressants_core24) ; 
%summarystat (antidepressants_core25) ; 
%summarystat (antidepressants_core26) ; 
%summarystat (antidepressants_core27) ; 
%summarystat (antidepressants_core28) ; 
%summarystat (antidepressants_core29) ; 
%summarystat (antidepressants_core30) ; 
%summarystat (antidepressants_core31) ; 
%summarystat (antidepressants_core32) ; 

*Sorting medication dataset before merging ;
proc sort data= FHS.fhs81_85 ;
	by id ;
run ;

*Sorting curated gen1 dataset for CESD scores before merging ;
proc sort data= FHS.curated_bap_0_0123 ;
	by id ;
run ;

*Sorting Clinical questions for 81-85 (exam 19-32) dataset before merging ;
proc sort data= FHS.clinical_81_85_exams19_32  ;
	by id ;
run ;

*Creating new dataset with anti-depressant medications, CESD scores, and Clinical diagnostic impressions for depression;
data FHS.depression_CESD_CDI ;
	merge FHS.fhs81_85 FHS.curated_bap_0_0123 FHS.clinical_81_85_exams19_32  ;
	by id ;
	
	retain idtype id framid antidepressants: cesd: cdi: depression:  anxiolytics: hypnotics: ;
	* Arrays to recode antidepressant variable: 0, 3 and . = 0, 1 and 2 = 1, 88 = . ;
	array depression {*} antidepressants_core17 antidepressants_1_core18 antidepressants_core19-antidepressants_core32 ;
	do i = 1 to dim(depression) ;
		if depression{i} in (.,88) then depression{i} = . ;
		else if depression{i} in(0,3) then depression{i} = 0 ;
		else if depression{i} in (1,2) then depression{i} = 1 ;
	end ;
	
	* Recoding clinical diagnostic impression (cdi) variables for exam 22 ;
	if cdi_depression_core22 in (.,88) then cdi_depression_core22 = . ;
	else if cdi_depression_core22 in (0,4) then cdi_depression_core22 = 0 ;
	else if cdi_depression_core22 = 1 or cdi_depression_core22 = 2 or cdi_depression_core22 = 3 then cdi_depression_core22 = 1 ;

	
	* Recoding clinical diagnostic impression (cdi) variables for exam 23 ;
	if cdi_depression_core23 in (.,88) then cdi_depression_core23 = . ;
	else if cdi_depression_core23 = 0 then cdi_depression_core23 = 0 ;
	else if cdi_depression_core23 = 1 then cdi_depression_core23 = 1 ;
	
	
	*CALCULATING CESD TOTAL FOR EACH EXAM ;
	
	*EXAM 22 ;
	*create a variable for the sum of positively worded CESD variables in exam 22;
	positive_score_22=12-sum(of CESD_4_core22, CESD_8_core22, CESD_12_core22, CESD_16_core22);
	*total score for CES-D Variable in exam 22;
	CESD_TOTAL_core22= sum(of CESD_1_core22--CESD_20_core22)-sum (of CESD_4_core22, CESD_8_core22, CESD_12_core22, CESD_16_core22) + positive_score_22 ;

	*EXAM 23 ;
	*create a variable for the sum of positively worded CESD variables in exam 23;
	positive_score_23=12-sum(of CESD_4_core23, CESD_8_core23, CESD_12_core23, CESD_16_core23);
	*total score for CES-D Variable in exam 23;
	CESD_TOTAL_core23= sum(of CESD_1_core23--CESD_20_core23)-sum (of CESD_4_core23, CESD_8_core23, CESD_12_core23, CESD_16_core23) + positive_score_23 ;

	*EXAM 25 ;
	*create a variable for the sum of positively worded CESD variables in exam 25;
	positive_score_25=12-sum(of CESD_4_core25, CESD_8_core25, CESD_12_core25, CESD_16_core25);
	*total score for CES-D Variable in exam 25;
	CESD_TOTAL_core25= sum(of CESD_1_core25--CESD_20_core25)-sum (of CESD_4_core25, CESD_8_core25, CESD_12_core25, CESD_16_core25) + positive_score_25 ;

	*EXAM 26 ;
	*create a variable for the sum of positively worded CESD variables in exam 26;
	positive_score_26=12-sum(of CESD_4_core26, CESD_8_core26, CESD_12_core26, CESD_16_core26);
	*total score for CES-D Variable in exam 26;
	CESD_TOTAL_core26= sum(of CESD_1_core26--CESD_20_core26)-sum (of CESD_4_core26, CESD_8_core26, CESD_12_core26, CESD_16_core26) + positive_score_26 ;

	*EXAM 27 ;
	*create a variable for the sum of positively worded CESD variables in exam 27;
	positive_score_27=12-sum(of CESD_4_core27, CESD_8_core27, CESD_12_core27, CESD_16_core27);
	*total score for CES-D Variable in exam 27;
	CESD_TOTAL_core27= sum(of CESD_1_core27--CESD_20_core27)-sum (of CESD_4_core27, CESD_8_core27, CESD_12_core27, CESD_16_core27) + positive_score_27 ;

	*EXAM 28 ;
	*create a variable for the sum of positively worded CESD variables in exam 28;
	positive_score_28=12-sum(of CESD_4_core28, CESD_8_core28, CESD_12_core28, CESD_16_core28);
	*total score for CES-D Variable in exam 28;
	CESD_TOTAL_core28= sum(of CESD_1_core28--CESD_20_core28)-sum (of CESD_4_core28, CESD_8_core28, CESD_12_core28, CESD_16_core28) + positive_score_28 ;

	*EXAM 29 ;
	*create a variable for the sum of positively worded CESD variables in exam 29;
	positive_score_29=12-sum(of CESD_4_core29, CESD_8_core29, CESD_12_core29, CESD_16_core29);
	*total score for CES-D Variable in exam 29;
	CESD_TOTAL_core29= sum(of CESD_1_core29--CESD_20_core29)-sum (of CESD_4_core29, CESD_8_core29, CESD_12_core29, CESD_16_core29) + positive_score_29 ;

	*EXAM 30 ;
	*create a variable for the sum of positively worded CESD variables in exam 30;
	positive_score_30=12-sum(of CESD_4_core30, CESD_8_core30, CESD_12_core30, CESD_16_core30);
	*total score for CES-D Variable in exam 30;
	CESD_TOTAL_core30= sum(of CESD_1_core30--CESD_20_core30)-sum (of CESD_4_core30, CESD_8_core30, CESD_12_core30, CESD_16_core30) + positive_score_30 ;

	*EXAM 31 ;
	*create a variable for the sum of positively worded CESD variables in exam 31;
	positive_score_31=12-sum(of CESD_4_core31, CESD_8_core31, CESD_12_core31, CESD_16_core31);
	*total score for CES-D Variable in exam 31;
	CESD_TOTAL_core31= sum(of CESD_1_core31--CESD_20_core31)-sum (of CESD_4_core31, CESD_8_core31, CESD_12_core31, CESD_16_core31) + positive_score_31 ;

	
	* Creating binary variable 'cesd' from CESD_TOTAL ;
	array cesd_total {*} CESD_TOTAL_core22 CESD_TOTAL_core23 CESD_TOTAL_core25-CESD_TOTAL_core31 ;
	array cesd {*} cesd_core22 cesd_core23 cesd_core25-cesd_core31 ;
	do i = 1 to dim(cesd_total) ;
		if cesd_total {i} = . then cesd{i} = . ;
		else if 0 <= cesd_total {i} < 16 then cesd{i} = 0 ;
		else if cesd_total {i} >= 16 then cesd{i} = 1 ;
	end ;
	
	* CREATING DERIVED VARIABLE 'depression_core' ;	
	
	*Exams 17 to 21 and 32 (antidepressant variable only) ;
	array depressionmed17_21 {6} antidepressants_core17 antidepressants_1_core18 antidepressants_core19-antidepressants_core21 antidepressants_core32  ;
	array depression_derived17_21 {6} depression_core17-depression_core21 depression_core32 ;
	do i = 1 to 6 ;
		if depressionmed17_21 {i} = . then depression_derived17_21 {i} = . ;
		else if depressionmed17_21 {i} = 1 then depression_derived17_21 {i} = 1 ;
		else if depressionmed17_21 {i} = 0 then depression_derived17_21 {i} = 0 ;
	end ;
	
	*Exams 22 and 23 (antidepressant, cesd and cdi variables) ;
	array depressionmed22_23 {2} antidepressants_core22 antidepressants_core23 ;
	array depressioncesd22_23 {2} cesd_core22 cesd_core23 ;
	array depressioncdi22_23 {2} cdi_depression_core22 cdi_depression_core23 ;
	array depression_derived22_23 {2} depression_core22 depression_core23 ;
	
	do i = 1 to 2 ;
		if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = . and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = . and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = . and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = . and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = . then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = . then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = . then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = . then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = 1 and depressioncesd22_23{i} = . and depressioncdi22_23{i} = . then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = 0 and depressioncesd22_23{i} = . and depressioncdi22_23{i} = . then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = . and depressioncdi22_23{i} = 1 then depression_derived22_23{i} = 1 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = . and depressioncdi22_23{i} = 0 then depression_derived22_23{i} = 0 ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = 1 and depressioncdi22_23{i} = . then depression_derived22_23{i} = . ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = 0 and depressioncdi22_23{i} = . then depression_derived22_23{i} = . ;
		else if depressionmed22_23{i} = . and depressioncesd22_23{i} = . and depressioncdi22_23{i} = . then depression_derived22_23{i} = . ;
	end ;
	
	* Exams 24 - 31 (antidepressant and cesd variables) ;
	array depressionmed24_31 {8} antidepressants_core24-antidepressants_core31 ;
	array depressioncesd24_31 {8} cesd_core24-cesd_core31 ;
	array depression_derived24_31 {8} depression_core24-depression_core31 ;
	
	do i = 1 to 8 ;
		if depressionmed24_31{i} = 1 and depressioncesd24_31{i} = 1 then depression_derived24_31{i} = 1 ;
		else if depressionmed24_31{i} = 1 and depressioncesd24_31{i} = 0 then depression_derived24_31{i} = 1 ;
		else if depressionmed24_31{i} = 0 and depressioncesd24_31{i} = 1 then depression_derived24_31{i} = 0 ;
		else if depressionmed24_31{i} = 0 and depressioncesd24_31{i} = 0 then depression_derived24_31{i} = 0 ;
		else if depressionmed24_31{i} = . and depressioncesd24_31{i} = 1 then depression_derived24_31{i} = . ;
		else if depressionmed24_31{i} = . and depressioncesd24_31{i} = 0 then depression_derived24_31{i} = . ;
		else if depressionmed24_31{i} = 1 and depressioncesd24_31{i} = . then depression_derived24_31{i} = 1 ;
		else if depressionmed24_31{i} = 0 and depressioncesd24_31{i} = . then depression_derived24_31{i} = 0 ;
		else if depressionmed24_31{i} = . and depressioncesd24_31{i} = . then depression_derived24_31{i} = . ;
	end ;
	
	
	keep id 
		 antidepressants_core17 antidepressants_1_core18 antidepressants_2_core18 antidepressants_core19-antidepressants_core32
		 cesd_core22 cesd_core23 cesd_core25-cesd_core31
		 cdi_depression_core22
		 cdi_depression_core23
		 depression_core17-depression_core32
		 anxiolytics: hypnotics:
		 ;
run ;




