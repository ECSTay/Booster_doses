##works for cleaned SV v3 data

#loading files in

load("Z:/Manual stuff/COVID-19 Booster Doses/Data/2024-10-22_booster_sample_data.rda")


# Basic cleaning function for demographics and non-analysis tables

library(gtsummary)
library(tidyverse)
library(officer)
library(readxl)
library(matrixStats)
library(scales)

library(here)

library(data.table)
library(grid)
library(forcats)
library(gridExtra)
library(stringr)

booster <- COVID_SAMPLE


#ETHNICITY = IS_INDIGENOUS

## Splitting vax_list into the component concoms
#exclude Zostavax as no longer available

a <- tstrsplit(booster$VAX_LIST, ", ", fixed = TRUE)
a <- sapply(a, as.vector)
colnames(a) <- paste("VAX", 1:ncol(a))

vax_list <- c("Flucelvax Quad", "Fluad Quad", "Fluarix", "FluQuadri", "Fluzone High-Dose Quadrivalent",
              "Vaxigrip Tetra", "Influvac", "Influvax Tetra", "Afluria Quad (NIP)", "Afluria Quad (Non NIP)",
              "Fluarix Tetra (NIP)", "Fluarix Tetra (Non NIP)", "Fluad")


vax_list <- c("Boostrix", "ADT Booster","Adacel")

vax_list <- c("Havrix","Engerix-B","Twinrix", "Engerix_B (adult)", 
              "Vaqta (adult)", "H-B-Vax II (adult)", "Vivaxim","Typhim Vi",
              "Stamaril", "Varilrix", "Vivotif Oral", "Priorix",
               "Rabipur", "Verorab 3.25IU", "JESPECT", "Imojev",
              "Avaxim", "Arexvy", "MenQuadfi", "Merieux Inactivated Rabies Vaccine", "M-M-R II")


vax_list <- c("Prevenar 13")

vax_list <- c("Shingrix")



b <- sapply(vax_list, function(vax) apply(apply(a, 2, function(x) vax == x), 1, any))
b[is.na(b)] <- FALSE
booster$INFLUENZA <- rowSums(b[,c("Flucelvax Quad", "Fluad Quad", "Fluarix", "FluQuadri", "Fluzone High-Dose Quadrivalent",
                                  "Vaxigrip Tetra", "Influvac", "Influvax Tetra", "Afluria Quad (NIP)", "Afluria Quad (Non NIP)",
                                  "Fluarix Tetra (NIP)", "Fluarix Tetra (Non NIP)", "Fluad")])

booster$OTHER <- b
booster$OTHER <- rowSums(b[,c("Havrix","Engerix-B","Twinrix", "Engerix_B (adult)", 
                              "Vaqta (adult)", "H-B-Vax II (adult)", "Vivaxim","Typhim Vi",
                              "Stamaril", "Varilrix", "Vivotif Oral", "Priorix",
                               "Rabipur", "Verorab 3.25IU", "JESPECT", "Imojev",
                              "Avaxim", "Arexvy", "MenQuadfi", "Merieux Inactivated Rabies Vaccine", "M-M-R II")])
table(booster$OTHER)###too mixed - allow to go into the intercept

booster$TYPHIM_VI <- b
booster$TYPHIM_VI <- as.integer(as.logical(booster$TYPHIM_VI))

booster$dTpa <- rowSums(b[,c("Boostrix", "ADT Booster", "Adacel")])

booster$HEPATITIS_VAX <- rowSums(b[,c("Havrix","Engerix-B","Twinrix", "Engerix_B (adult)", "Vaqta (adult)", "H-B-Vax II (adult)", "Vivaxim")])

booster$SHINGRIX <- b
booster$SHINGRIX <- as.integer(as.logical(booster$SHINGRIX))

booster$PREV <- b
booster$PREV <- as.integer(as.logical(booster$PREV))

booster$PNEUMO <- b
booster$PNEUMO <- as.integer(as.logical(booster$PNEUMO))


##VAX_BRAND into VAX_GROUP

booster[VAX_BRAND %in% c("Comirnaty Bivalent BA.1 (Pfizer)","Comirnaty Bivalent BA.4-5 (Pfizer)","Comirnaty XBB.1.5 (Pfizer)"),
        VAX_GROUP := "1"]

booster[VAX_BRAND %in% c("Spikevax Bivalent BA.1 (Moderna)", "Spikevax Bivalent BA.4-5 (Moderna)" ,"Spikevax XBB.1.5 (Moderna)"),
        VAX_GROUP := "2"]

booster[VAX_BRAND %in% "Nuvaxovid (Novavax)",
        VAX_GROUP := "3"]

##replace NA with 0
booster[is.na(booster)] <- 0

## allocating 1 and 2 to SEX and IS_INDIGENOUS
#booster[SEX %in% c("Female", "Male"), SEX := c(1,2)]
booster$SEX <- str_replace_all(booster$SEX, c("Female" = "1", "Male" = "2"))
#booster[, := booster$IS_INDIGENOUS ]

booster$IS_INDIGENOUS <- booster$IS_INDIGENOUS + 1

#save(booster, file = "Z:/R files/Manual Stuff/COVID-19 Booster Doses/Analysis Code/2024-11-05_booster_bc.Rda")

save(booster, file = "C:/Users/ETay/Documents/Booster doses/Data/2024-11-12_booster_bc.Rda")




###

mutate( # ifelse ok - no NA
       across(LOCAL_PAIN:LOCAL_ITCHING, ~ ifelse(PARTICIPANT == 1 & ANY_EVENT == 1 & LOCAL == 0, 0, .x)), # ifelse ok - NA goes to NA
       across(SYSTEMIC_MYALGIA:SYSTEMIC_ARTHRALGIA, ~ ifelse(PARTICIPANT == 1 & ANY_EVENT == 1 & SYSTEMIC_PAIN == 0, 0, .x)), # ifelse ok - NA goes to NA
       across(GI_NAUSEA:GI_PAIN, ~ ifelse(PARTICIPANT == 1 & ANY_EVENT == 1 & GI == 0, 0, .x)) # ifelse ok - NA goes to NA
)