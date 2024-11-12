##after Booster_basic_clean
library(tidyverse)
library(stringr)
library(data.table)
library(matrixStats)
library(forcats)
library(DataCombine)
library(flextable)
library(gridExtra)

#call the booster data

load("C:/Users/ETay/Documents/Booster doses/Data/2024-11-12_booster_bc.rda")

booster <-  as.data.table(booster)

booster_sd <- booster[!(is.na(booster$ANY_EVENT)) &
                      !(is.na(booster$IS_INDIGENOUS)) &
                      !(is.na(booster$SEX)) & 
                      !(is.na(booster$VAX_BRAND)) &
                      !(is.na(booster$PMH)), ]

#booster <- booster[!(!is.na(SEX) & SEX =="Another Term"),]
booster_sd <- booster[,c("AGE_AT_VAX_YEARS", "SEX", "IS_INDIGENOUS","VAX_BRAND","VAX_GROUP",
                             "SURVEY_RESPONSE_TIMESTAMP","ANY_EVENT" ,"IMPACT",                   
                             "MA","LOCAL","FEVER","RASH","CHILLS","SYSTEMIC", "SYSTEMIC_HEADACHE","SYSTEMIC_MYALGIA","SYSTEMIC_ARTHRALGIA","GI",
                             "LETHARGY", "LOC", "SEIZURE", "CHEST",
                             "PMH_ANAPHYL", "PMH", "PMH_HEART_DIS", "PMH_BP", "PMH_DIABETES", "PMH_LUNG",
                             "PMH_OBESITY" ,"PMH_KIDNEY" ,"PMH_LIVER","PMH_CANCER" ,              
                             "PMH_BLOOD_CANCER", "PMH_CHEMO_RAD", "PMH_TRANSPLANT_ORGAN","PMH_TRANSPLANT_BONE","PMH_NEURO",                
                             "PMH_INFLAM" ,"PMH_IMMUNODEF",
                             "INFLUENZA","dTpa", "SHINGRIX",            
                             "PREV")]
booster_sd[is.na(booster_sd)] <- 0
colnames(booster_sd) <- tolower(colnames(booster_sd))
setnames(booster_sd, "is_indigenous", "indig")
pmh_vars <- c("pmh_anaphyl", "pmh_heart_dis", "pmh_bp", "pmh_diabetes", "pmh_lung", "pmh_obesity", "pmh_kidney",
              "pmh_liver", "pmh_cancer", "pmh_blood_cancer", "pmh_chemo_rad", "pmh_transplant_organ",
              "pmh_transplant_bone", "pmh_neuro", "pmh_inflam", "pmh_immunodef")
for(var in pmh_vars) booster_sd[pmh == 0, paste(var) := 0]
booster_sd <- booster_sd[pmh == 0 |
                                 (!is.na(pmh_anaphyl) & !is.na(pmh_heart_dis) & !is.na(pmh_bp) & !is.na(pmh_diabetes) & !is.na(pmh_lung) &
                                  !is.na(pmh_obesity) & !is.na(pmh_kidney) & !is.na(pmh_liver) & !is.na(pmh_cancer) &
                                  !is.na(pmh_blood_cancer) & !is.na(pmh_chemo_rad) & !is.na(pmh_transplant_organ) &
                                  !is.na(pmh_transplant_bone) & !is.na(pmh_neuro) & !is.na(pmh_inflam) & !is.na(pmh_immunodef))]

booster_sd[, epoch := cut(as.Date(stringr::str_sub(survey_response_timestamp, 1, 10), "%d/%m/%Y"), breaks = as.Date(c("2023-01-01", "2023-07-01", "2024-01-01", "2024-06-30")), labels = FALSE)]


#changing to factors
booster_sd[,
         `:=`(
              sex = factor(sex, levels = c("1", "2")), #Female = 1
              indig = factor(indig, levels = c("1", "2")),#Non_aboriginal = 1
              vax_group = factor(vax_group, levels = c("1","2","3")),#Comirnaty = 1, Spikevax = 2, Novavax = 3
              age = as.factor(age_at_vax_years),
              epoch = forcats::fct_rev(factor(epoch))#,
              
)]

#ages <-

save(booster_sd, file = "C:/Users/ETay/Documents/Booster doses/Data/booster_sd_sample_20241112.Rda")
