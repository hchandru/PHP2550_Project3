# PHP2550_Project3

# Introduction
Cardiovascular disease (CVD) remains a leading cause of morbidity and mortality worldwide, necessitating accurate risk prediction models for timely intervention and prevention. In the realm of predictive modeling, the application of risk prediction models across diverse populations is of paramount importance. This collaborative project, conducted in partnership with Dr. Jon Steingrimsson from the Biostatistics Department, focuses on evaluating the performance of a cardiovascular risk prediction model originally developed on a source population when applied to a target population.

The predictive model under scrutiny originates from the Framingham Heart Study, a seminal investigation comprising participants aged 30-62. Historically, such models, while valuable in their original context, have demonstrated challenges when extrapolated to populations with different demographic compositions, as seen in the example of the Framingham ATP-III model's suboptimal generalization to multi-ethnic populations.

The objectives of this project are two-fold:

- Evaluate performance of a cardiovascular risk prediction model (D'Agostino et al., 2008) in a target population underlying NHANES: The evaluation is conducted using the weighting estimator for the Brier score in the target population (Steingrimsson et al., 2022). 
- Conduct the same analysis when the target population is simulated: The simulation process involves utilizing summary statistics from the NHANES data to simulate the target population. The same estimator for weighted Brier score in the target population is used to evaluate the performance of the models in the simulated populations.

# Preprocessing and Data Overview
The preprocessing involves using data from the Framingham study to obtain a sample of source population data. From the Framingham data, the variables CVD (indicating whether myocardial infarction, fatal coronary heart disease, atherothrombotic infarction, cerebral embolism, intracerebral hemorrhage, or subarachnoid hemorrhage or fatal cerebrovascular disease occured during followup), TIMECVD (number of days from baseline exam to first CVD event during followup or number of days from baseline to censor date), SEX (participant sex), TOTCHOL (serum total cholesterol in mg/dL), AGE (age at exam), SYSBP (systolic blood pressure in mmHg; mean of last two of three measurements), DIABP (diastolic blood pressure in mmHg; mean of last two of three measurements), CURSMOKE (indicating whether participants are current smokers), DIABETES (indicating whether the participant is diabetic), BPMEDS (whether anti-hypertensive medication was being used at the time of exam), HDLC (high density lipoprotein cholesterol in mg/dL) and BMI (body mass index) are extracted, and any NA's are removed.

Different variables (`SYSBP_T` and `SYSBP_UT`) are created to store the blood pressure values depending on whether or not the individuals were on anti-hypertensive medication. In addition, any observations corresponding to individuals without cardiovascular events within 15 years were removed (removal of censored data). Two different datasets corresponding to males and females was then created.

Then, from the NHANES data for the year 2017-18, variables corresponding to those selected from the Framingham study (above) are similarly selected to create a separate dataset corresponding to a sample of the target population data. The observations are filtered to only include data corresponding to participants belonging to ages 30-62 so that the eligibility criteria for the source population (Framingham study participants) is met.

### Files

`PHP-2550_Project-3.Rmd` is an R Markdown file containing the code and report pertaining to this analysis.

`PHP 2550_Project 3.pdf` is a pdf file containing the code and report pertaining to this analysis.

`PHP 2550_Project 3_Code.R` is an R script file containing the code used in this analysis.
