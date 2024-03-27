# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS Anxiety/Depression derived variable exam 1-3 drug usage source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: August 2023
# 
# 
# The purpose of this R code is to
# 1) abstract data about clinical exams for anxiety/depression in all relevant Gen 3/Omni 2/NOS exams.
# 2) find individuals in Gen 3/Omni 2/NOS exams 1-3 who used drugs in categories 81-85 (Anxiolytics/Hypnotics/Antidepressants),
# using ATC codes
# 3) create derived variables based on those data sources and return data containing all important variables, also explained in
# documentation
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Category 81-85 (Anxiolytics/Hypnotics/Antidepressants) ATC information - ATClabels_Anxio_Hyp_Depr_ALL.xlsx
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
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files


#### Core exam section ####

#In each of these sections an individual .sas7bdat file is read
#Then columns containing the data for questions we want are isolated
#This can change between exams

#ex01_3 (GEN 3 Exam 1)
#G3A426, G3A427
exam_01_3 <- read_sas("e_exam_ex01_3_0086_v2.sas7bdat")
columns_01_3 <- exam_01_3[,c("ID", "IDTYPE", "G3A426", "G3A427")]

#ex01_2 (NOS Exam 1)
#G3A426, G3A427
exam_01_2 <- read_sas("e_exam_ex01_2_0813.sas7bdat")
columns_01_2 <- exam_01_2[,c("id", "idtype", "g3a426", "g3a427")]

#ex01_72 (Omni 2 Exam 1)
#G3A426, G3A427
exam_01_72 <- read_sas("e_exam_ex01_72_0652.sas7bdat")
columns_01_72 <- exam_01_72[,c("id", "idtype", "g3a426", "g3a427")]

#Combine Exam 1 sections into one data frame
colnames(columns_01_3) <- colnames(columns_01_2)
columns_01_3b <- do.call("rbind", list(columns_01_3, columns_01_2, columns_01_72))


#ex02_3b (combined GEN 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
#e_exam_2011_m_0017 should have been renamed something like ex_exam_ex02_3b_0017
#G3B0448, G3B0449
exam_02_3b <- read_sas("e_exam_2011_m_0017_v1.sas7bdat")
columns_02_3b <- exam_02_3b[,c("id", "idtype", "g3b0448", "g3b0449")]

#ex03_3b (combined GEN 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
#G3C0639, G3C0640
exam_03_3b <- read_sas("e_exam_ex03_3b_1069.sas7bdat")
columns_03_3b <- exam_03_3b[,c("id", "idtype", "G3C0639", "G3C0640")]

#Joining columns together
df_list <- list(columns_01_3b, columns_02_3b, columns_03_3b)
df_joined <- df_list %>% reduce(full_join, by=c("id", "idtype"))

#framid create
df_joined$framid <- with(df_joined, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))
#framid relocate
core_exam_data <- df_joined %>% relocate(framid, .after = idtype)

#### Reading ATC data ####

#reading three drug lists from excel spreadsheet
anxiolytics <- read_excel("ATClabels_Anxio_Hyp_Depr_ALL.xlsx", sheet = "Anxiolytics")
anxio_atc_list <- anxiolytics$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`
hypnotics <- read_excel("ATClabels_Anxio_Hyp_Depr_ALL.xlsx", sheet = "Hypnotics")
hypno_atc_list <- hypnotics$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`
antidep <- read_excel("ATClabels_Anxio_Hyp_Depr_ALL.xlsx", sheet = "Antidep")
antidep_atc_list <- antidep$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`


#### ATC Exam 1 ####

#ex01_3 (GEN 3 Exam 1)
#Read drugs file
meds_01_3 <- read_sas("vr_meds_ex01_3_0242.sas7bdat")

#Select columns containing ATC numbers
select_01_3 <- meds_01_3[,c("ID", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_01_3 <- select_01_3 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_01_3 <- select_01_3 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_01_3 <- select_01_3 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_01_3 <- unique(select_01_3$ID)
med_found_01_3 <- data.frame(ID_01_3)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_01_3['anxio_column'] <- as.integer(med_found_01_3$ID_01_3 %in% anxio_filtered_01_3$ID)
med_found_01_3['hypno_column'] <- as.integer(med_found_01_3$ID_01_3 %in% hypno_filtered_01_3$ID)
med_found_01_3['antidep_column'] <- as.integer(med_found_01_3$ID_01_3 %in% antidep_filtered_01_3$ID)

#standardizing column names
colnames(med_found_01_3) <- c("ID", "Anxiolytics_core1", "Hypnotics_core1", "Antidepressants_core1")

#adding IDTYPE and FRAMID columns
med_found_01_3$IDTYPE <- 3
med_found_01_3$FRAMID <- (30000 + med_found_01_3$ID)


#ex01_3b (combined NOS Exam 1 and Omni 2 Exam 1)
#Read drugs file
meds_01_3b <- read_sas("vr_meds_ex01_3b_0825_v1.sas7bdat")
meds_01_2 <- subset(meds_01_3b, idtype == 2)
meds_01_72 <- subset(meds_01_3b, idtype == 72)

#ex01_2 (NOS Exam 1)
#Select columns containing ATC numbers
select_01_2 <- meds_01_2[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_01_2 <- select_01_2 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_01_2 <- select_01_2 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_01_2 <- select_01_2 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_01_2 <- unique(select_01_2$id)
med_found_01_2 <- data.frame(ID_01_2)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_01_2['anxio_column'] <- as.integer(med_found_01_2$ID_01_2 %in% anxio_filtered_01_2$id)
med_found_01_2['hypno_column'] <- as.integer(med_found_01_2$ID_01_2 %in% hypno_filtered_01_2$id)
med_found_01_2['antidep_column'] <- as.integer(med_found_01_2$ID_01_2 %in% antidep_filtered_01_2$id)

#standardizing column names
colnames(med_found_01_2) <- c("ID", "Anxiolytics_core1", "Hypnotics_core1", "Antidepressants_core1")

#adding IDTYPE and FRAMID columns
med_found_01_2$IDTYPE <- 2
med_found_01_2$FRAMID <- (20000 + med_found_01_2$ID)


#ex01_72 (Omni 2 Exam 1)
#Select columns containing ATC numbers
select_01_72 <- meds_01_72[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_01_72 <- select_01_72 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_01_72 <- select_01_72 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_01_72 <- select_01_72 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_01_72 <- unique(select_01_72$id)
med_found_01_72 <- data.frame(ID_01_72)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_01_72['anxio_column'] <- as.integer(med_found_01_72$ID_01_72 %in% anxio_filtered_01_72$id)
med_found_01_72['hypno_column'] <- as.integer(med_found_01_72$ID_01_72 %in% hypno_filtered_01_72$id)
med_found_01_72['antidep_column'] <- as.integer(med_found_01_72$ID_01_72 %in% antidep_filtered_01_72$id)

#standardizing column names
colnames(med_found_01_72) <- c("ID", "Anxiolytics_core1", "Hypnotics_core1", "Antidepressants_core1")

#adding IDTYPE and FRAMID columns
med_found_01_72$IDTYPE <- 72
med_found_01_72$FRAMID <- (720000 + med_found_01_72$ID)


#Combine Exam 1 sections into one data frame
med_found_exam_1 <- do.call("rbind", list(med_found_01_3, med_found_01_2, med_found_01_72))


#### ATC Exam 2 ####

#ex02_3b (combined GEN 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
#vr_meds_2011_m_0675 should have been renamed vr_meds_ex02_3b_0675
#Read drugs file
meds_02_3b <- read_sas("vr_meds_2011_m_0675.sas7bdat")

meds_02_3 <- subset(meds_02_3b, idtype == 3)
meds_02_2 <- subset(meds_02_3b, idtype == 2)
meds_02_72 <- subset(meds_02_3b, idtype == 72)

#ex02_3 (Gen 3 Exam 2)
#Select columns containing ATC numbers
select_02_3 <- meds_02_3[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_02_3 <- select_02_3 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_02_3 <- select_02_3 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_02_3 <- select_02_3 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_02_3 <- unique(select_02_3$id)
med_found_02_3 <- data.frame(ID_02_3)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_02_3['anxio_column'] <- as.integer(med_found_02_3$ID_02_3 %in% anxio_filtered_02_3$id)
med_found_02_3['hypno_column'] <- as.integer(med_found_02_3$ID_02_3 %in% hypno_filtered_02_3$id)
med_found_02_3['antidep_column'] <- as.integer(med_found_02_3$ID_02_3 %in% antidep_filtered_02_3$id)

#standardizing column names
colnames(med_found_02_3) <- c("ID", "Anxiolytics_core2", "Hypnotics_core2", "Antidepressants_core2")

#adding IDTYPE and FRAMID columns
med_found_02_3$IDTYPE <- 3
med_found_02_3$FRAMID <- (30000 + med_found_02_3$ID)


#ex02_2 (NOS Exam 2)
#Select columns containing ATC numbers
select_02_2 <- meds_02_2[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_02_2 <- select_02_2 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_02_2 <- select_02_2 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_02_2 <- select_02_2 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_02_2 <- unique(select_02_2$id)
med_found_02_2 <- data.frame(ID_02_2)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_02_2['anxio_column'] <- as.integer(med_found_02_2$ID_02_2 %in% anxio_filtered_02_2$id)
med_found_02_2['hypno_column'] <- as.integer(med_found_02_2$ID_02_2 %in% hypno_filtered_02_2$id)
med_found_02_2['antidep_column'] <- as.integer(med_found_02_2$ID_02_2 %in% antidep_filtered_02_2$id)

#standardizing column names
colnames(med_found_02_2) <- c("ID", "Anxiolytics_core2", "Hypnotics_core2", "Antidepressants_core2")

#adding IDTYPE and FRAMID columns
med_found_02_2$IDTYPE <- 2
med_found_02_2$FRAMID <- (20000 + med_found_02_2$ID)


#ex02_72 (Omni 2 Exam 2)
#Select columns containing ATC numbers
select_02_72 <- meds_02_72[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_02_72 <- select_02_72 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_02_72 <- select_02_72 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_02_72 <- select_02_72 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_02_72 <- unique(select_02_72$id)
med_found_02_72 <- data.frame(ID_02_72)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_02_72['anxio_column'] <- as.integer(med_found_02_72$ID_02_72 %in% anxio_filtered_02_72$id)
med_found_02_72['hypno_column'] <- as.integer(med_found_02_72$ID_02_72 %in% hypno_filtered_02_72$id)
med_found_02_72['antidep_column'] <- as.integer(med_found_02_72$ID_02_72 %in% antidep_filtered_02_72$id)

#standardizing column names
colnames(med_found_02_72) <- c("ID", "Anxiolytics_core2", "Hypnotics_core2", "Antidepressants_core2")

#adding IDTYPE and FRAMID columns
med_found_02_72$IDTYPE <- 72
med_found_02_72$FRAMID <- (720000 + med_found_02_72$ID)


#Combine Exam 2 sections into one data frame
med_found_exam_2 <- do.call("rbind", list(med_found_02_3, med_found_02_2, med_found_02_72))


#### ATC Exam 3 ####

#ex03_3b (combined GEN 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
#Read drugs file
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")

meds_03_3 <- subset(meds_03_3b, idtype == 3)
meds_03_2 <- subset(meds_03_3b, idtype == 2)
meds_03_72 <- subset(meds_03_3b, idtype == 72)

#ex03_3 (Gen 3 Exam 3)
#Select columns containing ATC numbers
select_03_3 <- meds_03_3[,c("id", "atc_cod1", "atc_cod2", "atc_cod3")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_03_3 <- select_03_3 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_03_3 <- select_03_3 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_03_3 <- select_03_3 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_03_3 <- unique(select_03_3$id)
med_found_03_3 <- data.frame(ID_03_3)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_3['anxio_column'] <- as.integer(med_found_03_3$ID_03_3 %in% anxio_filtered_03_3$id)
med_found_03_3['hypno_column'] <- as.integer(med_found_03_3$ID_03_3 %in% hypno_filtered_03_3$id)
med_found_03_3['antidep_column'] <- as.integer(med_found_03_3$ID_03_3 %in% antidep_filtered_03_3$id)

#standardizing column names
colnames(med_found_03_3) <- c("ID", "Anxiolytics_core3", "Hypnotics_core3", "Antidepressants_core3")

#adding IDTYPE and FRAMID columns
med_found_03_3$IDTYPE <- 3
med_found_03_3$FRAMID <- (30000 + med_found_03_3$ID)


#ex03_2 (NOS Exam 3)
#Select columns containing ATC numbers
select_03_2 <- meds_03_2[,c("id", "atc_cod1", "atc_cod2", "atc_cod3")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_03_2 <- select_03_2 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_03_2 <- select_03_2 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_03_2 <- select_03_2 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_03_2 <- unique(select_03_2$id)
med_found_03_2 <- data.frame(ID_03_2)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_2['anxio_column'] <- as.integer(med_found_03_2$ID_03_2 %in% anxio_filtered_03_2$id)
med_found_03_2['hypno_column'] <- as.integer(med_found_03_2$ID_03_2 %in% hypno_filtered_03_2$id)
med_found_03_2['antidep_column'] <- as.integer(med_found_03_2$ID_03_2 %in% antidep_filtered_03_2$id)

#standardizing column names
colnames(med_found_03_2) <- c("ID", "Anxiolytics_core3", "Hypnotics_core3", "Antidepressants_core3")

#adding IDTYPE and FRAMID columns
med_found_03_2$IDTYPE <- 2
med_found_03_2$FRAMID <- (20000 + med_found_03_2$ID)


#ex03_72 (Omni 2 Exam 3)
#Select columns containing ATC numbers
select_03_72 <- meds_03_72[,c("id", "atc_cod1", "atc_cod2", "atc_cod3")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_03_72 <- select_03_72 %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_03_72 <- select_03_72 %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_03_72 <- select_03_72 %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
ID_03_72 <- unique(select_03_72$id)
med_found_03_72 <- data.frame(ID_03_72)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_72['anxio_column'] <- as.integer(med_found_03_72$ID_03_72 %in% anxio_filtered_03_72$id)
med_found_03_72['hypno_column'] <- as.integer(med_found_03_72$ID_03_72 %in% hypno_filtered_03_72$id)
med_found_03_72['antidep_column'] <- as.integer(med_found_03_72$ID_03_72 %in% antidep_filtered_03_72$id)

#standardizing column names
colnames(med_found_03_72) <- c("ID", "Anxiolytics_core3", "Hypnotics_core3", "Antidepressants_core3")

#adding IDTYPE and FRAMID columns
med_found_03_72$IDTYPE <- 72
med_found_03_72$FRAMID <- (720000 + med_found_03_72$ID)


#Combine Exam 3 sections into one data frame
med_found_exam_3 <- do.call("rbind", list(med_found_03_3, med_found_03_2, med_found_03_72))


#### Merging ATC data ####

#list all exam data frames
gen3_all_list <- list(med_found_exam_1, med_found_exam_2, med_found_exam_3)
#join all exam data frames together
gen3_joined <- gen3_all_list %>% reduce(full_join, by = c("IDTYPE", "ID", "FRAMID"))
#reorder columns
gen3_reordered <- gen3_joined[, c("IDTYPE", "ID", "FRAMID", 
                                  "Antidepressants_core1", "Antidepressants_core2", "Antidepressants_core3", 
                                  "Anxiolytics_core1", "Anxiolytics_core2", "Anxiolytics_core3", 
                                  "Hypnotics_core1", "Hypnotics_core2", "Hypnotics_core3")] 
#reorder rows
anxio_hyp_depr_data <- gen3_reordered %>% arrange(FRAMID)


#### ATC and Core data merge ####

colnames(core_exam_data) <- c("ID", "IDTYPE", "FRAMID", "CDI_Depression_core1", "CDI_Anxiety_core1",
                              "CDI_Depression_core2", "CDI_Anxiety_core2", "CDI_Depression_core3", "CDI_Anxiety_core3")

#recode 2 as 0 for important columns (this was NOT done earlier but will be here)
core_exam_data$CDI_Depression_core1 <- replace(core_exam_data$CDI_Depression_core1, core_exam_data$CDI_Depression_core1 == 2, 0)
core_exam_data$CDI_Anxiety_core1 <- replace(core_exam_data$CDI_Anxiety_core1, core_exam_data$CDI_Anxiety_core1 == 2, 0)
core_exam_data$CDI_Depression_core2 <- replace(core_exam_data$CDI_Depression_core2, core_exam_data$CDI_Depression_core2 == 2, 0)
core_exam_data$CDI_Anxiety_core2 <- replace(core_exam_data$CDI_Anxiety_core2, core_exam_data$CDI_Anxiety_core2 == 2, 0)
core_exam_data$CDI_Depression_core3 <- replace(core_exam_data$CDI_Depression_core3, core_exam_data$CDI_Depression_core3 == 2, 0)
core_exam_data$CDI_Anxiety_core3 <- replace(core_exam_data$CDI_Anxiety_core3, core_exam_data$CDI_Anxiety_core3 == 2, 0)

#join two datasets together
maindata <- full_join(anxio_hyp_depr_data, core_exam_data, by = c("IDTYPE", "ID", "FRAMID"))  

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
maindata <- maindata %>% mutate_at(c(4:9, 13:18), ~replace_na(.,9))

#Placeholder values, used for testing
maindata$Depression_core1 = 9999
maindata$Anxiety_core1 = 9999
maindata$Depression_core2 = 9999
maindata$Anxiety_core2 = 9999
maindata$Depression_core3 = 9999
maindata$Anxiety_core3 = 9999

#### Exam 1 Logic ####

#for loop to determine depression derived variable
for (i in 1:nrow(maindata)) {
  if (maindata$Antidepressants_core1[i] == 1 & maindata$CDI_Depression_core1[i] == 1) {
    #If Antidepressants_core1 = 1 and CDI_Depression_core1 = 1 then Depression_core1 = 1
    maindata$Depression_core1[i] = 1
  } else if (maindata$Antidepressants_core1[i] == 1 & maindata$CDI_Depression_core1[i] == 0) {
    #If Antidepressants_core1 = 1 and CDI_Depression_core1 = 0 then Depression_core1 = 1
    maindata$Depression_core1[i] = 1
  } else if (maindata$Antidepressants_core1[i] == 0 & maindata$CDI_Depression_core1[i] == 1) {
    #If Antidepressants_core1 = 0 and CDI_Depression_core1 = 1 then Depression_core1 = 1
    maindata$Depression_core1[i] = 1
  } else if (maindata$Antidepressants_core1[i] == 0 & maindata$CDI_Depression_core1[i] == 0) {
    #If Antidepressants_core1 = 0 and CDI_Depression_core1 = 0 then Depression_core1 = 0
    maindata$Depression_core1[i] = 0
  } else if (maindata$Antidepressants_core1[i] == 9 & maindata$CDI_Depression_core1[i] == 1) {
    #If Antidepressants_core1 = NA and CDI_Depression_core1 = 1 then Depression_core1 = 1
    maindata$Depression_core1[i] = 1
  } else if (maindata$Antidepressants_core1[i] == 9 & maindata$CDI_Depression_core1[i] == 0) {
    #If Antidepressants_core1 = NA and CDI_Depression_core1 = 0 then Depression_core1 = 0
    maindata$Depression_core1[i] = 0
  } else if (maindata$Antidepressants_core1[i] == 1 & maindata$CDI_Depression_core1[i] == 9) {
    #If Antidepressants_core1 = 1 and CDI_Depression_core1 = NA then Depression_core1 = 1
    maindata$Depression_core1[i] = 1
  } else if (maindata$Antidepressants_core1[i] == 0 & maindata$CDI_Depression_core1[i] == 9) {
    #If Antidepressants_core1 = 0 and CDI_Depression_core1 = NA then Depression_core1 = 0
    maindata$Depression_core1[i] = 0
  } else if (maindata$Antidepressants_core1[i] == 9 & maindata$CDI_Depression_core1[i] == 9) {
    #If Antidepressants_core1 = NA and CDI_Depression_core1 = NA then Depression_core1 = NA
    maindata$Depression_core1[i] = NA
  }
}

#for loop to determine anxiety derived variable
for (i in 1:nrow(maindata)) {
  if (maindata$Anxiolytics_core1[i] == 1 & maindata$CDI_Anxiety_core1[i] == 1) {
    #If Anxiolytics_core1 = 1 and CDI_Anxiety_core1 = 1 then Anxiety_core1 = 1
    maindata$Anxiety_core1[i] = 1
  } else if (maindata$Anxiolytics_core1[i] == 1 & maindata$CDI_Anxiety_core1[i] == 0) {
    #If Anxiolytics_core1 = 1 and CDI_Anxiety_core1 = 0 then Anxiety_core1 = 1
    maindata$Anxiety_core1[i] = 1
  } else if (maindata$Anxiolytics_core1[i] == 0 & maindata$CDI_Anxiety_core1[i] == 1) {
    #If Anxiolytics_core1 = 0 and CDI_Anxiety_core1 = 1 then Anxiety_core1 = 1
    maindata$Anxiety_core1[i] = 1
  } else if (maindata$Anxiolytics_core1[i] == 0 & maindata$CDI_Anxiety_core1[i] == 0) {
    #If Anxiolytics_core1 = 0 and CDI_Anxiety_core1 = 0 then Anxiety_core1 = 0
    maindata$Anxiety_core1[i] = 0
  } else if (maindata$Anxiolytics_core1[i] == 9 & maindata$CDI_Anxiety_core1[i] == 1) {
    #If Anxiolytics_core1 = NA and CDI_Anxiety_core1 = 1 then Anxiety_core1 = 1
    maindata$Anxiety_core1[i] = 1
  } else if (maindata$Anxiolytics_core1[i] == 9 & maindata$CDI_Anxiety_core1[i] == 0) {
    #If Anxiolytics_core1 = NA and CDI_Anxiety_core1 = 0 then Anxiety_core1 = 0
    maindata$Anxiety_core1[i] = 0
  } else if (maindata$Anxiolytics_core1[i] == 1 & maindata$CDI_Anxiety_core1[i] == 9) {
    #If Anxiolytics_core1 = 1 and CDI_Anxiety_core1 = NA then Anxiety_core1 = 1
    maindata$Anxiety_core1[i] = 1
  } else if (maindata$Anxiolytics_core1[i] == 0 & maindata$CDI_Anxiety_core1[i] == 9) {
    #If Anxiolytics_core1 = 0 and CDI_Anxiety_core1 = NA then Anxiety_core1 = 0
    maindata$Anxiety_core1[i] = 0
  } else if (maindata$Anxiolytics_core1[i] == 9 & maindata$CDI_Anxiety_core1[i] == 9) {
    #If Anxiolytics_core1 = NA and CDI_Anxiety_core1 = NA then Anxiety_core1 = NA
    maindata$Anxiety_core1[i] = NA
  }
}

#### Exam 2 Logic ####

#for loop to determine depression derived variable
for (i in 1:nrow(maindata)) {
  if (maindata$Antidepressants_core2[i] == 1 & maindata$CDI_Depression_core2[i] == 1) {
    #If Antidepressants_core2 = 1 and CDI_Depression_core2 = 1 then Depression_core2 = 1
    maindata$Depression_core2[i] = 1
  } else if (maindata$Antidepressants_core2[i] == 1 & maindata$CDI_Depression_core2[i] == 0) {
    #If Antidepressants_core2 = 1 and CDI_Depression_core2 = 0 then Depression_core2 = 1
    maindata$Depression_core2[i] = 1
  } else if (maindata$Antidepressants_core2[i] == 0 & maindata$CDI_Depression_core2[i] == 1) {
    #If Antidepressants_core2 = 0 and CDI_Depression_core2 = 1 then Depression_core2 = 1
    maindata$Depression_core2[i] = 1
  } else if (maindata$Antidepressants_core2[i] == 0 & maindata$CDI_Depression_core2[i] == 0) {
    #If Antidepressants_core2 = 0 and CDI_Depression_core2 = 0 then Depression_core2 = 0
    maindata$Depression_core2[i] = 0
  } else if (maindata$Antidepressants_core2[i] == 9 & maindata$CDI_Depression_core2[i] == 1) {
    #If Antidepressants_core2 = NA and CDI_Depression_core2 = 1 then Depression_core2 = 1
    maindata$Depression_core2[i] = 1
  } else if (maindata$Antidepressants_core2[i] == 9 & maindata$CDI_Depression_core2[i] == 0) {
    #If Antidepressants_core2 = NA and CDI_Depression_core2 = 0 then Depression_core2 = 0
    maindata$Depression_core2[i] = 0
  } else if (maindata$Antidepressants_core2[i] == 1 & maindata$CDI_Depression_core2[i] == 9) {
    #If Antidepressants_core2 = 1 and CDI_Depression_core2 = NA then Depression_core2 = 1
    maindata$Depression_core2[i] = 1
  } else if (maindata$Antidepressants_core2[i] == 0 & maindata$CDI_Depression_core2[i] == 9) {
    #If Antidepressants_core2 = 0 and CDI_Depression_core2 = NA then Depression_core2 = 0
    maindata$Depression_core2[i] = 0
  } else if (maindata$Antidepressants_core2[i] == 9 & maindata$CDI_Depression_core2[i] == 9) {
    #If Antidepressants_core2 = NA and CDI_Depression_core2 = NA then Depression_core2 = NA
    maindata$Depression_core2[i] = NA
  }
}

#for loop to determine anxiety derived variable
for (i in 1:nrow(maindata)) {
  if (maindata$Anxiolytics_core2[i] == 1 & maindata$CDI_Anxiety_core2[i] == 1) {
    #If Anxiolytics_core2 = 1 and CDI_Anxiety_core2 = 1 then Anxiety_core2 = 1
    maindata$Anxiety_core2[i] = 1
  } else if (maindata$Anxiolytics_core2[i] == 1 & maindata$CDI_Anxiety_core2[i] == 0) {
    #If Anxiolytics_core2 = 1 and CDI_Anxiety_core2 = 0 then Anxiety_core2 = 1
    maindata$Anxiety_core2[i] = 1
  } else if (maindata$Anxiolytics_core2[i] == 0 & maindata$CDI_Anxiety_core2[i] == 1) {
    #If Anxiolytics_core2 = 0 and CDI_Anxiety_core2 = 1 then Anxiety_core2 = 1
    maindata$Anxiety_core2[i] = 1
  } else if (maindata$Anxiolytics_core2[i] == 0 & maindata$CDI_Anxiety_core2[i] == 0) {
    #If Anxiolytics_core2 = 0 and CDI_Anxiety_core2 = 0 then Anxiety_core2 = 0
    maindata$Anxiety_core2[i] = 0
  } else if (maindata$Anxiolytics_core2[i] == 9 & maindata$CDI_Anxiety_core2[i] == 1) {
    #If Anxiolytics_core2 = NA and CDI_Anxiety_core2 = 1 then Anxiety_core2 = 1
    maindata$Anxiety_core2[i] = 1
  } else if (maindata$Anxiolytics_core2[i] == 9 & maindata$CDI_Anxiety_core2[i] == 0) {
    #If Anxiolytics_core2 = NA and CDI_Anxiety_core2 = 0 then Anxiety_core2 = 0
    maindata$Anxiety_core2[i] = 0
  } else if (maindata$Anxiolytics_core2[i] == 1 & maindata$CDI_Anxiety_core2[i] == 9) {
    #If Anxiolytics_core2 = 1 and CDI_Anxiety_core2 = NA then Anxiety_core2 = 1
    maindata$Anxiety_core2[i] = 1
  } else if (maindata$Anxiolytics_core2[i] == 0 & maindata$CDI_Anxiety_core2[i] == 9) {
    #If Anxiolytics_core2 = 0 and CDI_Anxiety_core2 = NA then Anxiety_core2 = 0
    maindata$Anxiety_core2[i] = 0
  } else if (maindata$Anxiolytics_core2[i] == 9 & maindata$CDI_Anxiety_core2[i] == 9) {
    #If Anxiolytics_core2 = NA and CDI_Anxiety_core2 = NA then Anxiety_core2 = NA
    maindata$Anxiety_core2[i] = NA
  }
}


#### Exam 3 Logic ####

#for loop to determine depression derived variable
for (i in 1:nrow(maindata)) {
  if (maindata$Antidepressants_core3[i] == 1 & maindata$CDI_Depression_core3[i] == 1) {
    #If Antidepressants_core3 = 1 and CDI_Depression_core3 = 1 then Depression_core3 = 1
    maindata$Depression_core3[i] = 1
  } else if (maindata$Antidepressants_core3[i] == 1 & maindata$CDI_Depression_core3[i] == 0) {
    #If Antidepressants_core3 = 1 and CDI_Depression_core3 = 0 then Depression_core3 = 1
    maindata$Depression_core3[i] = 1
  } else if (maindata$Antidepressants_core3[i] == 0 & maindata$CDI_Depression_core3[i] == 1) {
    #If Antidepressants_core3 = 0 and CDI_Depression_core3 = 1 then Depression_core3 = 1
    maindata$Depression_core3[i] = 1
  } else if (maindata$Antidepressants_core3[i] == 0 & maindata$CDI_Depression_core3[i] == 0) {
    #If Antidepressants_core3 = 0 and CDI_Depression_core3 = 0 then Depression_core3 = 0
    maindata$Depression_core3[i] = 0
  } else if (maindata$Antidepressants_core3[i] == 9 & maindata$CDI_Depression_core3[i] == 1) {
    #If Antidepressants_core3 = NA and CDI_Depression_core3 = 1 then Depression_core3 = 1
    maindata$Depression_core3[i] = 1
  } else if (maindata$Antidepressants_core3[i] == 9 & maindata$CDI_Depression_core3[i] == 0) {
    #If Antidepressants_core3 = NA and CDI_Depression_core3 = 0 then Depression_core3 = 0
    maindata$Depression_core3[i] = 0
  } else if (maindata$Antidepressants_core3[i] == 1 & maindata$CDI_Depression_core3[i] == 9) {
    #If Antidepressants_core3 = 1 and CDI_Depression_core3 = NA then Depression_core3 = 1
    maindata$Depression_core3[i] = 1
  } else if (maindata$Antidepressants_core3[i] == 0 & maindata$CDI_Depression_core3[i] == 9) {
    #If Antidepressants_core3 = 0 and CDI_Depression_core3 = NA then Depression_core3 = 0
    maindata$Depression_core3[i] = 0
  } else if (maindata$Antidepressants_core3[i] == 9 & maindata$CDI_Depression_core3[i] == 9) {
    #If Antidepressants_core3 = NA and CDI_Depression_core3 = NA then Depression_core3 = NA
    maindata$Depression_core3[i] = NA
  }
}

#for loop to determine anxiety derived variable
for (i in 1:nrow(maindata)) {
  if (maindata$Anxiolytics_core3[i] == 1 & maindata$CDI_Anxiety_core3[i] == 1) {
    #If Anxiolytics_core3 = 1 and CDI_Anxiety_core3 = 1 then Anxiety_core3 = 1
    maindata$Anxiety_core3[i] = 1
  } else if (maindata$Anxiolytics_core3[i] == 1 & maindata$CDI_Anxiety_core3[i] == 0) {
    #If Anxiolytics_core3 = 1 and CDI_Anxiety_core3 = 0 then Anxiety_core3 = 1
    maindata$Anxiety_core3[i] = 1
  } else if (maindata$Anxiolytics_core3[i] == 0 & maindata$CDI_Anxiety_core3[i] == 1) {
    #If Anxiolytics_core3 = 0 and CDI_Anxiety_core3 = 1 then Anxiety_core3 = 1
    maindata$Anxiety_core3[i] = 1
  } else if (maindata$Anxiolytics_core3[i] == 0 & maindata$CDI_Anxiety_core3[i] == 0) {
    #If Anxiolytics_core3 = 0 and CDI_Anxiety_core3 = 0 then Anxiety_core3 = 0
    maindata$Anxiety_core3[i] = 0
  } else if (maindata$Anxiolytics_core3[i] == 9 & maindata$CDI_Anxiety_core3[i] == 1) {
    #If Anxiolytics_core3 = NA and CDI_Anxiety_core3 = 1 then Anxiety_core3 = 1
    maindata$Anxiety_core3[i] = 1
  } else if (maindata$Anxiolytics_core3[i] == 9 & maindata$CDI_Anxiety_core3[i] == 0) {
    #If Anxiolytics_core3 = NA and CDI_Anxiety_core3 = 0 then Anxiety_core3 = 0
    maindata$Anxiety_core3[i] = 0
  } else if (maindata$Anxiolytics_core3[i] == 1 & maindata$CDI_Anxiety_core3[i] == 9) {
    #If Anxiolytics_core3 = 1 and CDI_Anxiety_core3 = NA then Anxiety_core3 = 1
    maindata$Anxiety_core3[i] = 1
  } else if (maindata$Anxiolytics_core3[i] == 0 & maindata$CDI_Anxiety_core3[i] == 9) {
    #If Anxiolytics_core3 = 0 and CDI_Anxiety_core3 = NA then Anxiety_core3 = 0
    maindata$Anxiety_core3[i] = 0
  } else if (maindata$Anxiolytics_core3[i] == 9 & maindata$CDI_Anxiety_core3[i] == 9) {
    #If Anxiolytics_core3 = NA and CDI_Anxiety_core3 = NA then Anxiety_core3 = NA
    maindata$Anxiety_core3[i] = NA
  }
}

#### Final Section ####
#returning NA values to columns, removing placeholder value
maindata[c(4:9, 13:18)] <- replace(maindata[c(4:9, 13:18)], maindata[c(4:9, 13:18)] == 9, NA)

#reorder columns
main_reordered <- maindata[, c("IDTYPE", "ID", "FRAMID", 
                               "Antidepressants_core1", "Antidepressants_core2", "Antidepressants_core3", 
                               "CDI_Depression_core1", "CDI_Depression_core2", "CDI_Depression_core3", 
                               "Depression_core1", "Depression_core2", "Depression_core3", 
                               "Anxiolytics_core1", "Anxiolytics_core2", "Anxiolytics_core3", 
                               "CDI_Anxiety_core1", "CDI_Anxiety_core2", "CDI_Anxiety_core3", 
                               "Anxiety_core1", "Anxiety_core2", "Anxiety_core3", 
                               "Hypnotics_core1", "Hypnotics_core2", "Hypnotics_core3")] 


#writing final CSV to file
write.csv(main_reordered, file = "Full_Sheet_Merged_Derived_81_85_2372_0823.csv", row.names = FALSE)
