# ******************************************************************************************************************************************
# Introduction to Gen 3 thyroid derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: August 2023
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 3 thyroid function.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Thyroid ATC information - ATC4_Thyroid.xlsx
# 
# 2)  Full ATC medication info for patients
# vr_meds_ex01_3_0242.sas7bdat (Gen 3 Exam 1)
# vr_meds_ex01_3b_0825_v1.sas7bdat (Combined NOS Exam 1 and Omni 2 Exam 1)
# vr_meds_2011_m_0675.sas7bdat (Combined Gen 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
# vr_meds_ex03_3b_1071_v1.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
# 
# 3)  Individual FHS exam questionnaires:
# e_exam_ex01_2_0813.sas7bdat (NOS Exam 1)
# e_exam_ex01_3_0086_v2.sas7bdat (Gen 3 Exam 1)
# e_exam_ex01_72_0652.sas7bdat (Omni 2 Exam 1)
# e_exam_2011_m_0017_v1.sas7bdat (Combined Gen 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
# e_exam_ex03_3b_1069.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
# 
# 
# Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files

#Reading ATC list from excel spreadsheet
thyro_xlsx <- read_excel("ATC4_Thyroid.xlsx", sheet = "H03_Thyroid_All")
thyro_atc_list <- thyro_xlsx$`ATC Code`

#### Exam 1 ####
#G3A399
exam_01_3 <- read_sas("e_exam_ex01_3_0086_v2.sas7bdat")
select_01_3 <- exam_01_3[,c("ID", "IDTYPE", "G3A399")]
#create framid column
select_01_3$FRAMID <- (30000 + select_01_3$ID)

exam_01_2 <- read_sas("e_exam_ex01_2_0813.sas7bdat")
select_01_2 <- exam_01_2[,c("id", "idtype", "g3a399")]
#create framid column
select_01_2$framid <- (20000 + select_01_2$id)

exam_01_72 <- read_sas("e_exam_ex01_72_0652.sas7bdat")
select_01_72 <- exam_01_72[,c("id", "idtype", "g3a399")]
#create framid column
select_01_72$framid <- (720000 + select_01_72$id)

#Combine Exam 1 sections into one data frame
colnames(select_01_3) <- colnames(select_01_2)
select_01_3b <- do.call("rbind", list(select_01_3, select_01_2, select_01_72))

#recode maybe answers as NA
select_01_3b$g3a399 <- replace(select_01_3b$g3a399, select_01_3b$g3a399 == 2, NA)

#medications from Gen 3 Exam 1
meds_01_3 <- read_sas("vr_meds_ex01_3_0242.sas7bdat")
cod_01_3 <- meds_01_3[,c("ID", "IDTYPE", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#medications from NOS Exam 1 and Omni 2 Exam 1
meds_01_2_72 <- read_sas("vr_meds_ex01_3b_0825_v1.sas7bdat")
cod_01_2_72 <- meds_01_2_72[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Combine medication sections into one data frame
colnames(cod_01_3) <- colnames(cod_01_2_72)
cod_01_3b <- do.call("rbind", list(cod_01_3, cod_01_2_72))
#create framid column
cod_01_3b$framid <- with(cod_01_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))

#Placeholder values, used for testing
select_01_3b$thyroid_core1 = 9999

#for loop to determine thyroid_core1
for (i in 1:nrow(select_01_3b)) {
  if (is.na(select_01_3b$g3a399[i])) {
    #If g3a399 = NA then thyroid_core1 = NA
    select_01_3b$thyroid_core1[i] = NA
  } else if (select_01_3b$g3a399[i] == 0) {
    #If g3a399 = 0 then thyroid_core1 = 0
    select_01_3b$thyroid_core1[i] = 0
  } else {
    #Else thyroid_core1 = 1
    select_01_3b$thyroid_core1[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered01_3b <- cod_01_3b %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_framid_01_3b <- unique(cod_01_3b$framid)
med_found_01_3b <- data.frame(atc_framid_01_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_01_3b['thyroid_med_core1'] <- as.integer(med_found_01_3b$atc_framid_01_3b %in% thyro_filtered01_3b$framid)
#prepare for merge
colnames(med_found_01_3b)[1] <- "framid"

#merge
all_01_3b <- merge(select_01_3b, med_found_01_3b, all = TRUE)

#remove unneeded columns of original questions
all_01_3b <- select(all_01_3b, -g3a399)

#replacement of missing values with .
all_01_3b <- all_01_3b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))



#### Exam 2 ####
#G3B0420
exam_02_3b <- read_sas("e_exam_2011_m_0017_v1.sas7bdat")
select_02_3b <- exam_02_3b[,c("id", "idtype", "g3b0420")]
#create framid column
select_02_3b$framid <- with(select_02_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))

#recode maybe answers as NA
select_02_3b$g3b0420 <- replace(select_02_3b$g3b0420, select_02_3b$g3b0420 == 2, NA)

#medications from Gen 3 Exam 2, NOS Exam 2 and Omni 2 Exam 2
meds_02_3b <- read_sas("vr_meds_2011_m_0675.sas7bdat")
cod_02_3b <- meds_02_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]


#create framid column
cod_02_3b$framid <- with(cod_02_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))

#Placeholder values, used for testing
select_02_3b$thyroid_core2 = 9999

#for loop to determine thyroid_core2
for (i in 1:nrow(select_02_3b)) {
  if (is.na(select_02_3b$g3b0420[i])) {
    #If g3b0420 = NA then thyroid_core2 = NA
    select_02_3b$thyroid_core2[i] = NA
  } else if (select_02_3b$g3b0420[i] == 0) {
    #If g3b0420 = 0 then thyroid_core2 = 0
    select_02_3b$thyroid_core2[i] = 0
  } else {
    #Else thyroid_core2 = 1
    select_02_3b$thyroid_core2[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered02_3b <- cod_02_3b %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_framid_02_3b <- unique(cod_02_3b$framid)
med_found_02_3b <- data.frame(atc_framid_02_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_02_3b['thyroid_med_core2'] <- as.integer(med_found_02_3b$atc_framid_02_3b %in% thyro_filtered02_3b$framid)
#prepare for merge
colnames(med_found_02_3b)[1] <- "framid"

#merge
all_02_3b <- merge(select_02_3b, med_found_02_3b, all = TRUE)

#remove unneeded columns of original questions
all_02_3b <- select(all_02_3b, -g3b0420)

#replacement of missing values with .
all_02_3b <- all_02_3b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 3 ####
#G3C0610
exam_03_3b <- read_sas("e_exam_ex03_3b_1069.sas7bdat")
select_03_3b <- exam_03_3b[,c("id", "idtype", "G3C0610")]
#create framid column
select_03_3b$framid <- with(select_03_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))

#recode maybe answers as NA
select_03_3b$G3C0610 <- replace(select_03_3b$G3C0610, select_03_3b$G3C0610 == 2, NA)

#medications from Gen 3 Exam 3, NOS Exam 3 and Omni 2 Exam 3
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
cod_03_3b <- meds_03_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]


#create framid column
cod_03_3b$framid <- with(cod_03_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))

#Placeholder values, used for testing
select_03_3b$thyroid_core3 = 9999

#for loop to determine thyroid_core3
for (i in 1:nrow(select_03_3b)) {
  if (is.na(select_03_3b$G3C0610[i])) {
    #If G3C0610 = NA then thyroid_core3 = NA
    select_03_3b$thyroid_core3[i] = NA
  } else if (select_03_3b$G3C0610[i] == 0) {
    #If G3C0610 = 0 then thyroid_core3 = 0
    select_03_3b$thyroid_core3[i] = 0
  } else {
    #Else thyroid_core3 = 1
    select_03_3b$thyroid_core3[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_framid_03_3b <- unique(cod_03_3b$framid)
med_found_03_3b <- data.frame(atc_framid_03_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_3b['thyroid_med_core3'] <- as.integer(med_found_03_3b$atc_framid_03_3b %in% thyro_filtered03_3b$framid)
#prepare for merge
colnames(med_found_03_3b)[1] <- "framid"

#merge
all_03_3b <- merge(select_03_3b, med_found_03_3b, all = TRUE)

#remove unneeded columns of original questions
all_03_3b <- select(all_03_3b, -G3C0610)

#replacement of missing values with .
all_03_3b <- all_03_3b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Final Merge ####

#list all exam data frames
gen3_all_list <- list(all_01_3b, all_02_3b, all_03_3b)
#join all exam data frames together
gen3_joined <- gen3_all_list %>% reduce(full_join, by = c("idtype", "id", "framid"))
#relocate framid column
gen3_joined <- gen3_joined %>% relocate(framid, .after = idtype)

#Write final dataset to file
write.csv(gen3_joined, file = "Thyroid_Data_Gen_3_Full_Spreadsheet.csv", row.names = FALSE)


