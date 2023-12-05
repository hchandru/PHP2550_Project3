knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "~/Downloads")

#Packages
install.packages("riskCommunicator", repos = "http://cran.us.r-project.org")
install.packages("nhanesA", repos = "http://cran.us.r-project.org")

library(riskCommunicator)
library(tidyverse)
library(tableone)
library(nhanesA)
library(naniar)
library(mice)
library(survey)
library(corrplot)
library(rsimsum)
library(knitr)
data("framingham")

# The Framingham data has been used to create models for cardiovascular risk.
# The variable selection and model below are designed to mimic the models used
# in the paper General Cardiovascular Risk Profile for Use in Primary Care 
# This paper is available (cvd_risk_profile.pdf) on Canvas.

framingham_df <- framingham %>% select(c(CVD, TIMECVD, SEX, TOTCHOL, AGE,
                                         SYSBP, DIABP, CURSMOKE, DIABETES, BPMEDS,
                                         HDLC, BMI))
framingham_df <- na.omit(framingham_df)

framingham_df$CURSMOKE <- as.factor(framingham_df$CURSMOKE)
framingham_df$DIABETES <- as.factor(framingham_df$DIABETES)
framingham_df$BPMEDS <- as.factor(framingham_df$BPMEDS)

CreateTableOne(data=framingham_df, strata = c("SEX"))

# Get blood pressure based on whether or not on BPMEDS
framingham_df$SYSBP_UT <- ifelse(framingham_df$BPMEDS == 0, 
                                 framingham_df$SYSBP, 0)
framingham_df$SYSBP_T <- ifelse(framingham_df$BPMEDS == 1, 
                                framingham_df$SYSBP, 0)

# Looking at risk within 15 years - remove censored data
# dim(framingham_df)
framingham_df <- framingham_df %>%
  filter(!(CVD == 0 & TIMECVD <= 365*15)) %>%
  select(-c(TIMECVD)) %>%
  mutate(S = 1)
# dim(framingham_df)


# Filter to each sex
framingham_df_men <- framingham_df %>% filter(SEX == 1) 
framingham_df_women <- framingham_df %>% filter(SEX == 2) 

# # Fit models with log transforms for all continuous variables
# mod_men <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
#                  log(SYSBP_T+1)+CURSMOKE+DIABETES, 
#       data= framingham_df_men, family= "binomial")
# 
# 
# mod_women <- glm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
#                    log(SYSBP_T+1)+CURSMOKE+DIABETES, 
#                data= framingham_df_women, family= "binomial")

# The NHANES data here finds the same covariates among this national survey data

# blood pressure, demographic, bmi, smoking, and hypertension info
bpx_2017 <- nhanes("BPX_J") %>% 
  select(SEQN, BPXSY1, BPXDI1) %>% 
  rename(SYSBP = BPXSY1, DIABP = BPXDI1)
demo_2017 <- nhanes("DEMO_J") %>% 
  select(SEQN, RIAGENDR, RIDAGEYR) %>% 
  rename(SEX = RIAGENDR, AGE = RIDAGEYR)
bmx_2017 <- nhanes("BMX_J") %>% 
  select(SEQN, BMXBMI) %>% 
  rename(BMI = BMXBMI)
smq_2017 <- nhanes("SMQ_J") %>%
  mutate(CURSMOKE = case_when(SMQ040 %in% c(1,2) ~ 1,
                              SMQ040 == 3 ~ 0, 
                              SMQ020 == 2 ~ 0)) %>%
  select(SEQN, CURSMOKE)
bpq_2017 <- nhanes("BPQ_J") %>% 
  mutate(BPMEDS = ifelse(BPQ050A == 1, 1, 0)) %>%
  select(SEQN, BPMEDS) 
tchol_2017 <- nhanes("TCHOL_J") %>% 
  select(SEQN, LBXTC) %>% 
  rename(TOTCHOL = LBXTC)
hdl_2017 <- nhanes("HDL_J") %>% 
  select(SEQN, LBDHDD) %>% 
  rename(HDLC = LBDHDD)
diq_2017 <- nhanes("DIQ_J") %>% 
  mutate(DIABETES = case_when(DIQ010 == 1 ~ 1, 
                              DIQ010 %in% c(2,3) ~ 0, 
                              TRUE ~ NA)) %>%
  select(SEQN, DIABETES) 

# Join data from different tables
df_2017 <- bpx_2017 %>%
  full_join(demo_2017, by = "SEQN") %>%
  full_join(bmx_2017, by = "SEQN") %>%
  full_join(hdl_2017, by = "SEQN") %>%
  full_join(smq_2017, by = "SEQN") %>%
  full_join(bpq_2017, by = "SEQN") %>%
  full_join(tchol_2017, by = "SEQN") %>%
  full_join(diq_2017, by = "SEQN") %>%
  filter(AGE < 63 & AGE > 29) #subset of data that meets the eligibility criteria for the Framingham study

df_2017$CURSMOKE <- as.factor(df_2017$CURSMOKE)
df_2017$DIABETES <- as.factor(df_2017$DIABETES)
df_2017$BPMEDS <- as.factor(df_2017$BPMEDS)

CreateTableOne(data = df_2017[-1], strata = c("SEX"))

df_2017$SYSBP_UT <- ifelse(df_2017$BPMEDS == 0, 
                           df_2017$SYSBP, 0)
df_2017$SYSBP_T <- ifelse(df_2017$BPMEDS == 1, 
                          df_2017$SYSBP, 0)
miss_var_summary(df_2017)
set.seed(1234)

#data sampled from the target population
complete_df_2017 <- df_2017[complete.cases(df_2017),] %>%
  mutate(S = 0)

#filtering by sex
complete_df_2017_men <- complete_df_2017 %>%
  filter(SEX == 1)
complete_df_2017_women <- complete_df_2017 %>%
  filter(SEX == 2)

#to create test and train data sets
ignore_target_men <- sample(c(TRUE, FALSE), nrow(complete_df_2017_men), replace = TRUE, prob = c(0.3,0.7))
ignore_target_women <- sample(c(TRUE, FALSE), nrow(complete_df_2017_women), replace = TRUE, prob = c(0.3,0.7))

complete_df_2017_men_train <- complete_df_2017_men[!ignore_target_men,]
complete_df_2017_men_test <- complete_df_2017_men[ignore_target_men,]

complete_df_2017_women_train <- complete_df_2017_women[!ignore_target_women,]
complete_df_2017_women_test <- complete_df_2017_women[ignore_target_women,]

#data sampled from the source population
#to create test and train data sets
ignore_source_men <- sample(c(TRUE, FALSE), nrow(framingham_df_men), replace = TRUE, prob = c(0.3,0.7))
ignore_source_women <- sample(c(TRUE, FALSE), nrow(framingham_df_women), replace = TRUE, prob = c(0.3,0.7))

framingham_df_men_train <- framingham_df_men[!ignore_source_men,]
framingham_df_men_test <- framingham_df_men[ignore_source_men,]

framingham_df_women_train <- framingham_df_women[!ignore_source_women,]
framingham_df_women_test <- framingham_df_women[ignore_source_women,]

#combining the training data from both the source and target populations to get the inverse odds weights
men_train_full_df <- rbind(framingham_df_men_train[-1], complete_df_2017_men_train[-1])
women_train_full_df <- rbind(framingham_df_women_train[-1], complete_df_2017_women_train[-1])

#models for odds of belonging to the source population

#men
men_source_lo_train <- glm(S ~ HDLC + TOTCHOL + AGE + SYSBP_UT + SYSBP_T + CURSMOKE + DIABETES,
                           data = men_train_full_df, family = "binomial")
men_train_full_df$odds <- (predict(men_source_lo_train, newdata = men_train_full_df, type = "response"))/(1 - (predict(men_source_lo_train, newdata = men_train_full_df, type = "response"))) #obtaining odds
men_train_full_df$weights <- 1/men_train_full_df$odds #inverse odds weights
men_train_source <- men_train_full_df %>% filter(S == 1) #only source population data to fit model
men_train_source <- cbind(framingham_df_men_train$CVD, men_train_source) %>%
  rename("CVD" = "framingham_df_men_train$CVD")
men_train_source$CVD <- as.integer(men_train_source$CVD)

#women
women_source_lo_train <- glm(S ~ HDLC + TOTCHOL + AGE + SYSBP_UT + SYSBP_T + CURSMOKE + DIABETES,
                             data = women_train_full_df, family = "binomial")
women_train_full_df$odds <- (predict(women_source_lo_train, newdata = women_train_full_df, type = "response"))/(1 - (predict(women_source_lo_train, newdata = women_train_full_df, type = "response")))
women_train_full_df$weights <- 1/women_train_full_df$odds
women_train_source <- women_train_full_df %>% filter(S == 1)
women_train_source <- cbind(framingham_df_women_train$CVD, women_train_source) %>%
  rename("CVD" = "framingham_df_women_train$CVD")
women_train_source$CVD <- as.integer(women_train_source$CVD)

#fitting risk score models on the training data from the source population
#men
design_men <- svydesign(~1, weights = men_train_source$weights, data = men_train_source)
mod_men_train <- svyglm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                          log(SYSBP_T+1)+CURSMOKE+DIABETES, 
                        design= design_men, family= "binomial")

#women
design_women <- svydesign(~1, weights = women_train_source$weights, data = women_train_source)
mod_women_train <- svyglm(CVD~log(HDLC)+log(TOTCHOL)+log(AGE)+log(SYSBP_UT+1)+
                            log(SYSBP_T+1)+CURSMOKE+DIABETES, 
                          design= design_women, family= "binomial")
#combining the testing data from both the source and target populations to get the inverse odds weights
men_test_full_df <- rbind(framingham_df_men_test[-1], complete_df_2017_men_test[-1])
women_test_full_df <- rbind(framingham_df_women_test[-1], complete_df_2017_women_test[-1])

#models for odds of belonging to the source population

#men
men_source_lo_test <- glm(S ~ HDLC + TOTCHOL + AGE + SYSBP_UT + SYSBP_T + CURSMOKE + DIABETES,
                          data = men_test_full_df, family = "binomial")
men_test_full_df$odds <- (predict(men_source_lo_test, newdata = men_test_full_df, type = "response"))/(1-(predict(men_source_lo_test, newdata = men_test_full_df, type = "response"))) #obtaining odds
men_test_full_df$weights <- 1/men_test_full_df$odds #inverse odds weights
men_test_source <- men_test_full_df %>% filter(S == 1) #only source population data to fit model
men_test_source <- cbind(framingham_df_men_test$CVD, men_test_source) %>%
  rename("CVD" = "framingham_df_men_test$CVD")
men_test_source$CVD <- as.integer(men_test_source$CVD)

#brier score
men_test_source$pred_probs <- predict(mod_men_train, newdata = men_test_source, type = "response") 
men_test_source$brier_num <- (men_test_source$weights)*((men_test_source$CVD - men_test_source$pred_probs)^2)

men_brier_score <- sum(men_test_source$brier_num)/nrow(complete_df_2017_men_test) #0.0874

#women
women_source_lo_test <- glm(S ~ HDLC + TOTCHOL + AGE + SYSBP_UT + SYSBP_T + CURSMOKE + DIABETES,
                            data = women_test_full_df, family = "binomial")
women_test_full_df$odds <- (predict(women_source_lo_test, newdata = women_test_full_df, type = "response"))/(1-(predict(women_source_lo_test, newdata = women_test_full_df, type = "response")))
women_test_full_df$weights <- 1/women_test_full_df$odds
women_test_source <- women_test_full_df %>% filter(S == 1)
women_test_source <- cbind(framingham_df_women_test$CVD, women_test_source) %>%
  rename("CVD" = "framingham_df_women_test$CVD")
women_test_source$CVD <- as.integer(women_test_source$CVD)

#brier score
women_test_source$pred_probs <- predict(mod_women_train, newdata = women_test_source, type = "response") 
women_test_source$brier_num <- (women_test_source$weights)*((women_test_source$CVD - women_test_source$pred_probs)^2)

women_brier_score <- sum(women_test_source$brier_num)/nrow(complete_df_2017_women_test) #0.0854

#table of brier scores
brier_df <- data.frame(
  Model = c("Men", "Women"),
  Brier_Score = c(men_brier_score, women_brier_score)
)

kable(brier_df, col.names = c("Model", "Brier Score"), caption = "Weighted Brier Score Estimates in the Target Population")
set.seed(1234)
sim_brier_function_m <- function(sample_size){
  sim_target_male_df <- data.frame(
    SYSBP = rnorm(n = sample_size, mean = 126.07, sd = 16.63),
    AGE = rnorm(n = sample_size, mean = 47.16, sd = 9.97),
    HDLC = rnorm(n = sample_size, mean = 47.45, sd = 14.54),
    CURSMOKE = rbinom(n = sample_size, size = 1, prob = 0.259),
    BPMEDS = rbinom(n = sample_size, size = 1, prob = 0.755),
    TOTCHOL = rnorm(n = sample_size, mean = 192.86, sd = 40.71),
    DIABETES = rbinom(n = sample_size, size = 1, prob = 0.131),
    S = 0
  )
  
  sim_target_male_df$CURSMOKE <- as.factor(sim_target_male_df$CURSMOKE)
  sim_target_male_df$DIABETES <- as.factor(sim_target_male_df$DIABETES)
  sim_target_male_df$BPMEDS <- as.factor(sim_target_male_df$BPMEDS)
  
  # Get blood pressure based on whether or not on BPMEDS
  sim_target_male_df$SYSBP_UT <- ifelse(sim_target_male_df$BPMEDS == 0, 
                                        sim_target_male_df$SYSBP, 0)
  sim_target_male_df$SYSBP_T <- ifelse(sim_target_male_df$BPMEDS == 1, 
                                       sim_target_male_df$SYSBP, 0)
  
  #combining the testing data from both the source and target populations to get the inverse odds weights
  men_test_full_df_sim <- rbind(framingham_df_men_test[,-c(1,2,6,11)], sim_target_male_df)
  
  #models for odds of belonging to the source population
  
  #men
  men_source_lo_test_sim <- glm(S ~ HDLC + TOTCHOL + AGE + SYSBP_UT + SYSBP_T + CURSMOKE + DIABETES,
                                data = men_test_full_df_sim, family = "binomial")
  men_test_full_df_sim$odds <- (predict(men_source_lo_test_sim, newdata = men_test_full_df_sim, type = "response"))/(1-(predict(men_source_lo_test_sim, newdata = men_test_full_df_sim, type = "response"))) #obtaining odds
  men_test_full_df_sim$weights <- 1/men_test_full_df_sim$odds #inverse odds weights
  men_test_source_sim <- men_test_full_df_sim %>% filter(S == 1) #only source population data to fit model
  men_test_source_sim <- cbind(framingham_df_men_test$CVD, men_test_source_sim) %>%
    rename("CVD" = "framingham_df_men_test$CVD")
  
  #brier score
  men_test_source_sim$pred_probs <- predict(mod_men_train, newdata = men_test_source_sim, type = "response") 
  men_test_source_sim$brier_num <- (men_test_source_sim$weights)*((men_test_source_sim$CVD - men_test_source_sim$pred_probs)^2)
  
  men_brier_score_sim <- sum(men_test_source_sim$brier_num)/nrow(sim_target_male_df)
  
  return(men_brier_score_sim)
}

men_brier_sim_res <- replicate(1600, sim_brier_function_m(100))
output_men_df <- data.frame(dataset = 1:1600, bs = men_brier_sim_res)
simsum_men <- simsum(data = output_men_df, estvarname = "bs", true = men_brier_score)
rsimsum::kable(simsum_men, stats = c("thetamean", "thetamedian", "bias", "empse", "mse"), caption = "Performance Measures for the Simulations Estimating Brier Scores for Men", col.names = c("Measure", "Estimate", "MCSE"))
set.seed(1234)
sim_brier_function_f <- function(sample_size){
  sim_target_female_df <- data.frame(
    SYSBP = rnorm(n = sample_size, mean = 122.46, sd = 18.76),
    AGE = rnorm(n = sample_size, mean = 46.75, sd = 9.85),
    HDLC = rnorm(n = sample_size, mean = 57.59, sd = 16.25),
    CURSMOKE = rbinom(n = sample_size, size = 1, prob = 0.171),
    BPMEDS = rbinom(n = sample_size, size = 1, prob = 0.8),
    TOTCHOL = rnorm(n = sample_size, mean = 195.40, sd = 38.95),
    DIABETES = rbinom(n = sample_size, size = 1, prob = 0.11),
    S = 0
  )
  
  sim_target_female_df$CURSMOKE <- as.factor(sim_target_female_df$CURSMOKE)
  sim_target_female_df$DIABETES <- as.factor(sim_target_female_df$DIABETES)
  sim_target_female_df$BPMEDS <- as.factor(sim_target_female_df$BPMEDS)
  
  # Get blood pressure based on whether or not on BPMEDS
  sim_target_female_df$SYSBP_UT <- ifelse(sim_target_female_df$BPMEDS == 0, 
                                          sim_target_female_df$SYSBP, 0)
  sim_target_female_df$SYSBP_T <- ifelse(sim_target_female_df$BPMEDS == 1, 
                                         sim_target_female_df$SYSBP, 0)
  
  #combining the testing data from both the source and target populations to get the inverse odds weights
  women_test_full_df_sim <- rbind(framingham_df_women_test[,-c(1,2,6,11)], sim_target_female_df)
  
  #models for odds of belonging to the source population
  
  #women
  women_source_lo_test_sim <- glm(S ~ HDLC + TOTCHOL + AGE + SYSBP_UT + SYSBP_T + CURSMOKE + DIABETES,
                                  data = women_test_full_df_sim, family = "binomial")
  women_test_full_df_sim$odds <- (predict(women_source_lo_test_sim, newdata = women_test_full_df_sim, type = "response"))/(1-(predict(women_source_lo_test_sim, newdata = women_test_full_df_sim, type = "response"))) #obtaining odds
  women_test_full_df_sim$weights <- 1/women_test_full_df_sim$odds #inverse odds weights
  women_test_source_sim <- women_test_full_df_sim %>% filter(S == 1) #only source population data to fit model
  women_test_source_sim <- cbind(framingham_df_women_test$CVD, women_test_source_sim) %>%
    rename("CVD" = "framingham_df_women_test$CVD")
  
  #brier score
  women_test_source_sim$pred_probs <- predict(mod_women_train, newdata = women_test_source_sim, type = "response") 
  women_test_source_sim$brier_num <- (women_test_source_sim$weights)*((women_test_source_sim$CVD - women_test_source_sim$pred_probs)^2)
  
  women_brier_score_sim <- sum(women_test_source_sim$brier_num)/nrow(sim_target_female_df)
  
  return(women_brier_score_sim)
}

women_brier_sim_res <- replicate(1600, sim_brier_function_f(100))
output_women_df <- data.frame(dataset = 1:1600, bs = women_brier_sim_res)
simsum_women <- simsum(data = output_women_df, estvarname = "bs", true = women_brier_score)
rsimsum::kable(simsum_women, stats = c("thetamean", "thetamedian", "bias", "empse", "mse"), caption = "Performance Measures for the Simulations Estimating Brier Scores for Women", col.names = c("Measure", "Estimate", "MCSE"))