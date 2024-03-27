# ******************************************************************************************************************************************
# Introduction to Gen 3/NOS/Omni 2 inflammation drugs data abstraction source code
# ******************************************************************************************************************************************
# 
# Created by Michael Cummings
# Last updated: December 2023
# 
# 
# The purpose of this R code is to abstract data from questions related to inflammation drugs in Gen 2/NOS/Omni 1,
# exams 1-3.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex01_2_0813.sas7bdat (NOS Exam 1)
# e_exam_ex01_3_0086_v2.sas7bdat (Gen 3 Exam 1)
# e_exam_ex01_72_0652.sas7bdat (Omni 2 Exam 1)
# e_exam_2011_m_0017_v1.sas7bdat (Combined Gen 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
# e_exam_ex03_3b_1069.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
# 
# 2)  Inflammation ATC information - ATC3_InflammationCodes.xlsx
# 
# 3)  Full ATC medication info for patients
# vr_meds_ex01_3_0242.sas7bdat (Gen 3 Exam 1)
# vr_meds_ex01_3b_0825_v1.sas7bdat (Combined NOS Exam 1 and Omni 2 Exam 1)
# vr_meds_2011_m_0675.sas7bdat (Combined Gen 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
# vr_meds_ex03_3b_1071_v1.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")

library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #library for reading .xlsx files


#reading three drug lists from excel spreadsheet
anti_codes <- read_excel("ATC3_InflammationCodes.xlsx", sheet = "All_inflammation_NoASA")
anti_atc_list <- na.omit(anti_codes$`ATC CODE FOR MEDICATION`)
nsaid_codes <- read_excel("ATC3_InflammationCodes.xlsx", sheet = "NSAIDS")
nsaid_atc_list <- nsaid_codes$`ATC Code`
steroid_codes <- read_excel("ATC3_InflammationCodes.xlsx", sheet = "Steroids(Glucocorticoids)")
steroid_atc_list <- steroid_codes$H02AB


#### Gen 3 Exam 1 / NOS Exam 1 / Omni 2 Exam 1 ####
exam_01_3 <- read_sas("e_exam_ex01_3_0086_v2.sas7bdat")
uniq_01_3 <- exam_01_3[,c("ID", "IDTYPE")]
#create framid column
uniq_01_3$FRAMID <- (30000 + uniq_01_3$ID)

exam_01_2 <- read_sas("e_exam_ex01_2_0813.sas7bdat")
uniq_01_2 <- exam_01_2[,c("id", "idtype")]
#create framid column
uniq_01_2$framid <- (20000 + uniq_01_2$id)

exam_01_72 <- read_sas("e_exam_ex01_72_0652.sas7bdat")
uniq_01_72 <- exam_01_72[,c("id", "idtype")]
#create framid column
uniq_01_72$framid <- (720000 + uniq_01_72$id)

#Combine Exam 1 sections into one data frame
colnames(uniq_01_3) <- colnames(uniq_01_2)
uniq_01_3b <- do.call("rbind", list(uniq_01_3, uniq_01_2, uniq_01_72))

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


#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered_01_3b <- cod_01_3b %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered_01_3b <- cod_01_3b %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered_01_3b <- cod_01_3b %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
atc_framid_01_3b <- unique(cod_01_3b$framid)
med_found_01_3b <- data.frame(atc_framid_01_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_01_3b['anti_column'] <- as.integer(med_found_01_3b$atc_framid_01_3b %in% anti_filtered_01_3b$framid)
med_found_01_3b['nsaid_column'] <- as.integer(med_found_01_3b$atc_framid_01_3b %in% nsaid_filtered_01_3b$framid)
med_found_01_3b['steroid_column'] <- as.integer(med_found_01_3b$atc_framid_01_3b %in% steroid_filtered_01_3b$framid)

#standardizing column names
colnames(med_found_01_3b) <- c("framid", "all_inflammation_core1", "nsaids_core1", "steroids_core1")
select_01_3b <- left_join(uniq_01_3b, med_found_01_3b, by = "framid")


#### Gen 3 Exam 2 / NOS Exam 2 / Omni 2 Exam 2 ####
exam_02_3b <- read_sas("e_exam_2011_m_0017_v1.sas7bdat")
uniq_02_3b <- exam_02_3b[,c("id", "idtype")]
#create framid column
uniq_02_3b$framid <- with(uniq_02_3b, ifelse(idtype == 3, 30000 + id, 
                                             ifelse(idtype == 2, 20000 + id, 
                                                    ifelse(idtype == 72, 720000 + id, id))))


meds_02_3b <- read_sas("vr_meds_2011_m_0675.sas7bdat")
cod_02_3b <- meds_02_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#create framid column
cod_02_3b$framid <- with(cod_02_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))


#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered_02_3b <- cod_02_3b %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered_02_3b <- cod_02_3b %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered_02_3b <- cod_02_3b %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
atc_framid_02_3b <- unique(cod_02_3b$framid)
med_found_02_3b <- data.frame(atc_framid_02_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_02_3b['anti_column'] <- as.integer(med_found_02_3b$atc_framid_02_3b %in% anti_filtered_02_3b$framid)
med_found_02_3b['nsaid_column'] <- as.integer(med_found_02_3b$atc_framid_02_3b %in% nsaid_filtered_02_3b$framid)
med_found_02_3b['steroid_column'] <- as.integer(med_found_02_3b$atc_framid_02_3b %in% steroid_filtered_02_3b$framid)

#standardizing column names
colnames(med_found_02_3b) <- c("framid", "all_inflammation_core2", "nsaids_core2", "steroids_core2")
select_02_3b <- left_join(uniq_02_3b, med_found_02_3b, by = "framid")


#### Gen 3 Exam 3 / NOS Exam 3 / Omni 2 Exam 3 ####
exam_03_3b <- read_sas("e_exam_ex03_3b_1069.sas7bdat")
uniq_03_3b <- exam_03_3b[,c("id", "idtype")]
#create framid column
uniq_03_3b$framid <- with(uniq_03_3b, ifelse(idtype == 3, 30000 + id, 
                                             ifelse(idtype == 2, 20000 + id, 
                                                    ifelse(idtype == 72, 720000 + id, id))))


meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
cod_03_3b <- meds_03_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#create framid column
cod_03_3b$framid <- with(cod_03_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))


#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered_03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered_03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered_03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
atc_framid_03_3b <- unique(cod_03_3b$framid)
med_found_03_3b <- data.frame(atc_framid_03_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_3b['anti_column'] <- as.integer(med_found_03_3b$atc_framid_03_3b %in% anti_filtered_03_3b$framid)
med_found_03_3b['nsaid_column'] <- as.integer(med_found_03_3b$atc_framid_03_3b %in% nsaid_filtered_03_3b$framid)
med_found_03_3b['steroid_column'] <- as.integer(med_found_03_3b$atc_framid_03_3b %in% steroid_filtered_03_3b$framid)

#standardizing column names
colnames(med_found_03_3b) <- c("framid", "all_inflammation_core3", "nsaids_core3", "steroids_core3")
select_03_3b <- left_join(uniq_03_3b, med_found_03_3b, by = "framid")


#### Final Merge ####
#Joining columns together

df_list <- list(select_01_3b, select_02_3b, select_03_3b)

df_joined <- df_list %>% reduce(full_join, by=c("id", "idtype", "framid"))
#Ordering the columns, they can be out of order after the join
df_ordered <- df_joined %>% arrange(as.numeric(framid))

df_final <- df_ordered[,c("id", "idtype", "framid", "all_inflammation_core1", "nsaids_core1", "steroids_core1",
                          "all_inflammation_core2", "nsaids_core2", "steroids_core2",
                          "all_inflammation_core3", "nsaids_core3", "steroids_core3")]

#write final CSV containing abstracted data
write.csv(df_final, file = "Gen3_all_inflammation_core.csv", row.names = FALSE)




