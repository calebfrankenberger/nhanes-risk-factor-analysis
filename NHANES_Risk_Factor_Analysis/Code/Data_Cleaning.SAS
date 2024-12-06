/*==================================================================================================
  Project   : NHANES Diabetes Risk Factor Analysis 
  Purpose   : Data cleaning and preparation for analysis
  Programmer: Caleb Frankenberger
  Date      : 11/10/2024
==================================================================================================*/


/*--------------------------------------------------------------------------------------------------
  Set up paths and create blank dataset
--------------------------------------------------------------------------------------------------*/

* Define paths to data folders;
%let rawdata = /home/username/Projects/NHANES_Diabetes_Risk_Analysis/Data/Raw;
%let clndata = /home/username/Projects/NHANES_Diabetes_Risk_Analysis/Data/Processed;

libname cln_dta "/home/username/Projects/NHANES_Diabetes_Risk_Analysis/Data/Processed";

* Create an empty data set to hold the clean data;
data cln_dta.clean_data;
	* SEQN identifies unique individuals across datasets;
	length SEQN 8; 
	if 0; 
run;


/*--------------------------------------------------------------------------------------------------
  Import and process BMX CSV file [File contains height and weight data]
--------------------------------------------------------------------------------------------------*/

* Import the CSV file;
proc import datafile="&rawdata./P_BMX.csv"
	out=bmx_data
	dbms=csv
	replace;
	guessingrows=MAX;
	getnames=yes;
run;

* Keep relevant variables;
data bmx_data_clean;
	set bmx_data;
	
	BMI = BMXBMI;

	* Keep SEQN and renamed variables;
	keep SEQN BMI;
run;

* Append to clean_data dataset;
data cln_dta.clean_data;
	merge cln_dta.clean_data bmx_data_clean;
	by SEQN;
run;


/*--------------------------------------------------------------------------------------------------
  Import and process BPXO CSV file [File contains blood pressure data]
--------------------------------------------------------------------------------------------------*/

* Import the CSV file;
proc import datafile="&rawdata./P_BPXO.csv"
	out=bpxo_data
	dbms=csv
	replace;
	guessingrows=MAX;
	getnames=yes;
run;

* Calculate relevant variables;
data bpxo_data_clean;
    set bpxo_data;
    
    /* Calculate mean systolic and diastolic BP, ignoring missing values */
    if nmiss(BPXOSY1, BPXOSY2, BPXOSY3) < 3 then Systolic_BP = mean(of BPXOSY1 BPXOSY2 BPXOSY3);
    if nmiss(BPXODI1, BPXODI2, BPXODI3) < 3 then Diastolic_BP = mean(of BPXODI1 BPXODI2 BPXODI3);
    
    /* Define Hypertension based on blood pressure thresholds */
    if nmiss(Systolic_BP, Diastolic_BP) = 0 then do;
    	if Systolic_BP >= 130 or Diastolic_BP >= 80 then Hypertension = 1;
    	else Hypertension = 0;
	end;
		else Hypertension = .; 
    
    /* Keep SEQN and the new variables */
    keep SEQN Hypertension;
run;

* Append to clean_data dataset;
data cln_dta.clean_data;
	merge cln_dta.clean_data bpxo_data_clean;
	by SEQN;
run;


/*--------------------------------------------------------------------------------------------------
  Import and process DEMO CSV file [File contains demographics data]
--------------------------------------------------------------------------------------------------*/

* Import the CSV file;
proc import datafile="&rawdata./P_DEMO.csv"
	out=demo_data
	dbms=csv
	replace;
	guessingrows=MAX;
	getnames=yes;
run;

* Keep relevant variables;
data demo_data_clean;
	set demo_data;
	
	Age = RIDAGEYR;
    
    Education_Level = .;
    
    /* Recode education into broader categories */
    if DMDEDUC2 in (1, 2) then Education_Level = 1; /* Less than High School */
    else if DMDEDUC2 = 3 then Education_Level = 2; /* High School Graduate/GED */
    else if DMDEDUC2 in (4, 5) then Education_Level = 3; /* College Graduate or Above */
	
	/* Race/Ethnicity */
    Race_Ethnicity = .;
    if RIDRETH3 = 1 then Race_Ethnicity = 1; /* Mexican American */
    else if RIDRETH3 = 2 then Race_Ethnicity = 2; /* Other Hispanic */
    else if RIDRETH3 = 3 then Race_Ethnicity = 3; /* Non-Hispanic White */
    else if RIDRETH3 = 4 then Race_Ethnicity = 4; /* Non-Hispanic Black */
    else if RIDRETH3 = 6 then Race_Ethnicity = 5; /* Non-Hispanic Asian */
    else if RIDRETH3 = 7 then Race_Ethnicity = 6; /* Other Race - Including Multi-Racial */
	
	keep SEQN Age Education_Level Race_Ethnicity WTINTPRP WTMECPRP SDMVPSU SDMVSTRA;
run;

* Append to clean_data dataset;
data cln_dta.clean_data;
	merge cln_dta.clean_data demo_data_clean;
	by SEQN;
run;


/*--------------------------------------------------------------------------------------------------
  Import and process DIQ CSV file [File contains diabetes questionnaire data]
--------------------------------------------------------------------------------------------------*/

* Import the CSV file;
proc import datafile="&rawdata./P_DIQ.csv"
	out=diq_data
	dbms=csv
	replace;
	guessingrows=MAX;
	getnames=yes;
run;

* Calculate relevant variables;
data diq_data_clean;
	set diq_data;
	
	if DIQ010 = 1 then Diabetes_Status = 1;
	else if DIQ010 = 2 then Diabetes_Status = 0;
	else Diabetes_Status = .;
	
	keep SEQN Diabetes_Status;
run;

* Append to clean_data dataset;
data cln_dta.clean_data;
	merge cln_dta.clean_data diq_data_clean;
	by SEQN;
run;


/*--------------------------------------------------------------------------------------------------
  Finish processing and finalize the clean_data dataset
--------------------------------------------------------------------------------------------------*/

/* Create a separate dataset for individuals missing Diabetes_Status but with other data complete */
data cln_dta.predict_missing_diabetes;
    set cln_dta.clean_data;
    if missing(Diabetes_Status) and cmiss(of _all_) = 1;
run;

* Remove entries with any missing data;
data cln_dta.clean_data;
	set cln_dta.clean_data;
	
	* Keep only rows with no missing values;
	if cmiss(of _all_) = 0; 
run;

* SEQN is no longer needed;
data cln_dta.clean_data;
    set cln_dta.clean_data;
    drop SEQN;
run;

* Display the number of individuals left in the dataset after cleaning;
proc sql;
	select count(*) as Remaining_Individuals
	from cln_dta.clean_data;
quit;

* Export datasets to CSV;
proc export data=cln_dta.predict_missing_diabetes
    outfile="&clndata./predict_missing_diabetes.csv"
    dbms=csv
    replace;
run;

proc export data=cln_dta.clean_data
	outfile="&clndata./clean_data.csv"
	dbms=csv
	replace;
run;

/* -- END OF FILE -- */