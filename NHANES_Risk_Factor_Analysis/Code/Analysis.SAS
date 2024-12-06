/*==================================================================================================
  Project   : NHANES Diabetes Risk Factor Analysis 
  Purpose   : Data analysis
  Programmer: Caleb Frankenberger
  Date      : 11/11/2024
==================================================================================================*/


/*--------------------------------------------------------------------------------------------------
  Import the data that was cleaned in Data_Cleaning.sas
--------------------------------------------------------------------------------------------------*/

libname cln_dta "/home/username/Projects/NHANES_Diabetes_Risk_Analysis/Data/Processed";

data analysis_data;
    set cln_dta.clean_data;
run;


/*--------------------------------------------------------------------------------------------------
  Fit the logistic regression model (initially using all predictors)
--------------------------------------------------------------------------------------------------*/
proc surveylogistic data=cln_dta.clean_data;
    weight WTMECPRP;
    strata SDMVSTRA;
    cluster SDMVPSU;

    model Diabetes_Status(event='1') = Age BMI Education_Level Hypertension Race_Ethnicity;
    output out=diag predicted=pred;
run;


/*--------------------------------------------------------------------------------------------------
  Fit the logistic regression model (dropping race and education level)
--------------------------------------------------------------------------------------------------*/
proc surveylogistic data=cln_dta.clean_data;
    weight WTMECPRP;
    strata SDMVSTRA;
    cluster SDMVPSU;

    model Diabetes_Status(event='1') = Age BMI Hypertension;
    output out=diag predicted=pred;
run;