# ******************************************************************************************************************************************
# Introduction to Gen 3/NOS/Omni 2 hormones derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: November 2023
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 3/NOS/Omni 2 hormones usage.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Hormones ATC information - ATC2_HormonesCodes_ALL_20231012.xlsx
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
hormone_codes <- read_excel("ATC2_HormonesCodes_ALL_20231012.xlsx", sheet = "Hormones_ALL")
hormone_atc_list <- hormone_codes$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`

#### Exam 1 ####
#Hormone questions: G3A053, G3A061, G3A065
#Menopause questions: G3A045
exam_01_3 <- read_sas("e_exam_ex01_3_0086_v2.sas7bdat")
select_01_3 <- exam_01_3[,c("ID", "IDTYPE", "G3A045", "G3A053", "G3A061", "G3A065")]
#create framid column
select_01_3$FRAMID <- (30000 + select_01_3$ID)

exam_01_2 <- read_sas("e_exam_ex01_2_0813.sas7bdat")
select_01_2 <- exam_01_2[,c("id", "idtype", "g3a045", "g3a053", "g3a061", "g3a065")]
#create framid column
select_01_2$framid <- (20000 + select_01_2$id)

exam_01_72 <- read_sas("e_exam_ex01_72_0652.sas7bdat")
select_01_72 <- exam_01_72[,c("id", "idtype", "g3a045", "g3a053", "g3a061", "g3a065")]
#create framid column
select_01_72$framid <- (720000 + select_01_72$id)

#Combine Exam 1 sections into one data frame
colnames(select_01_3) <- colnames(select_01_2)
select_01_3b <- do.call("rbind", list(select_01_3, select_01_2, select_01_72))

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
hormone_filtered_01_3b <- cod_01_3b %>% filter_all(any_vars(. %in% hormone_atc_list))
#Constructing the output data frame
atc_framid_01_3b <- unique(cod_01_3b$framid)
med_found_01_3b <- data.frame(atc_framid_01_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_01_3b['atc_hormones_01_3b'] <- as.integer(med_found_01_3b$atc_framid_01_3b %in% hormone_filtered_01_3b$framid)
#prepare for merge
colnames(med_found_01_3b)[1] <- "framid"

#merge
all_01_3b <- merge(select_01_3b, med_found_01_3b, all = TRUE)


#Replacing NA with 10 avoids R treating those values differently
all_01_3b <- all_01_3b %>% mutate_at(c("g3a045", "g3a053", "g3a061", "g3a065", "atc_hormones_01_3b"), ~replace_na(.,10))

#Placeholder values, used for testing
all_01_3b$hormones_data1 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_01_3b)) {
  if (all_01_3b$g3a053[i] %in% c(1,2) | all_01_3b$g3a061[i] %in% c(1,2) 
      | all_01_3b$g3a065[i] %in% c(1,2)) {
    #g3a053 in (1,2) or g3a061 in (1,2) or g3a065 in (1,2) then 1
    all_01_3b$hormones_data1[i] = 1
  } else if (all_01_3b$atc_hormones_01_3b[i] == 1) {
    #if ATC data = 1 then 1
    all_01_3b$hormones_data1[i] = 1
  } else if (all_01_3b$g3a053[i] == 0 & all_01_3b$g3a061[i] == 0 
             & all_01_3b$g3a065[i] == 0) {
    #g3a053 = 0 and g3a061 = 0 and g3a065 = 0 then 0
    all_01_3b$hormones_data1[i] = 0
  } else if (all_01_3b$g3a053[i] %in% c(10,8) & all_01_3b$g3a061[i] %in% c(10,8) 
             & all_01_3b$g3a065[i] %in% c(10,8)) {
    #g3a053 in (.,8) and g3a061 in (.,8) and g3a065 in (.,8) then NA
    all_01_3b$hormones_data1[i] = NA
  } else {
    #Else NA
    all_01_3b$hormones_data1[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_01_3b)) {
  if (is.na(all_01_3b$hormones_data1[i])) {
    #NA remains NA
    all_01_3b$hormones_core1[i] = NA
  } else if (all_01_3b$g3a045[i] == 0 & all_01_3b$hormones_data1[i] == 1) {
    #taking hormones though not in menopause
    all_01_3b$hormones_core1[i] = 2
  } else {
    #Else same value as before
    all_01_3b$hormones_core1[i] = all_01_3b$hormones_data1[i]
  }
}


#### Exam 2 ####
#Hormone questions: g3b0083
#Menopause questions: g3b0073

exam_02_3b <- read_sas("e_exam_2011_m_0017_v1.sas7bdat")
select_02_3b <- exam_02_3b[,c("id", "idtype", "g3b0073", "g3b0083")]
#create framid column
select_02_3b$framid <- with(select_02_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))

#medications from Gen 3 Exam 2, NOS Exam 2 and Omni 2 Exam 2
meds_02_3b <- read_sas("vr_meds_2011_m_0675.sas7bdat")
cod_02_3b <- meds_02_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_02_3b$framid <- with(cod_02_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))




#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
hormone_filtered_02_3b <- cod_02_3b %>% filter_all(any_vars(. %in% hormone_atc_list))
#Constructing the output data frame
atc_framid_02_3b <- unique(cod_02_3b$framid)
med_found_02_3b <- data.frame(atc_framid_02_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_02_3b['atc_hormones_02_3b'] <- as.integer(med_found_02_3b$atc_framid_02_3b %in% hormone_filtered_02_3b$framid)
#prepare for merge
colnames(med_found_02_3b)[1] <- "framid"

#merge
all_02_3b <- merge(select_02_3b, med_found_02_3b, all = TRUE)


#Replacing NA with 10 avoids R treating those values differently
all_02_3b <- all_02_3b %>% mutate_at(c("g3b0073", "g3b0083", "atc_hormones_02_3b"), ~replace_na(.,10))

#Placeholder values, used for testing
all_02_3b$hormones_data2 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_02_3b)) {
  if (all_02_3b$g3b0083[i] %in% c(1,2)) {
    #g3b0083 in (1,2) then 1
    all_02_3b$hormones_data2[i] = 1
  } else if (all_02_3b$atc_hormones_02_3b[i] == 1) {
    #if ATC data = 1 then 1
    all_02_3b$hormones_data2[i] = 1
  } else if (all_02_3b$g3b0083[i] == 0) {
    #g3b0083 = 0 then 0
    all_02_3b$hormones_data2[i] = 0
  } else if (all_02_3b$g3b0083[i] %in% c(10,8)) {
    #g3b0083 in (.,8) then NA
    all_02_3b$hormones_data2[i] = NA
  } else {
    #Else NA
    all_02_3b$hormones_data2[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_02_3b)) {
  if (is.na(all_02_3b$hormones_data2[i])) {
    #NA remains NA
    all_02_3b$hormones_core2[i] = NA
  } else if (all_02_3b$g3b0073[i] %in% c(0,1,2) & all_02_3b$hormones_data2[i] == 1) {
    #taking hormones though not in menopause
    all_02_3b$hormones_core2[i] = 2
  } else {
    #Else same value as before
    all_02_3b$hormones_core2[i] = all_02_3b$hormones_data2[i]
  }
}


#### Exam 3 ####
#Hormone questions: G3C0236
#Menopause questions: G3C0228

exam_03_3b <- read_sas("e_exam_ex03_3b_1069.sas7bdat")
select_03_3b <- exam_03_3b[,c("id", "idtype", "G3C0228", "G3C0236")]
#create framid column
select_03_3b$framid <- with(select_03_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))

#medications from Gen 3 Exam 3, NOS Exam 3 and Omni 2 Exam 3
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
cod_03_3b <- meds_03_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]

#create framid column
cod_03_3b$framid <- with(cod_03_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))




#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
hormone_filtered_03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% hormone_atc_list))
#Constructing the output data frame
atc_framid_03_3b <- unique(cod_03_3b$framid)
med_found_03_3b <- data.frame(atc_framid_03_3b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_3b['atc_hormones_03_3b'] <- as.integer(med_found_03_3b$atc_framid_03_3b %in% hormone_filtered_03_3b$framid)
#prepare for merge
colnames(med_found_03_3b)[1] <- "framid"

#merge
all_03_3b <- merge(select_03_3b, med_found_03_3b, all = TRUE)


#Replacing NA with 10 avoids R treating those values differently
all_03_3b <- all_03_3b %>% mutate_at(c("G3C0228", "G3C0236", "atc_hormones_03_3b"), ~replace_na(.,10))

#Placeholder values, used for testing
all_03_3b$hormones_data3 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_03_3b)) {
  if (all_03_3b$G3C0236[i] %in% c(1,2)) {
    #G3C0236 in (1,2) then 1
    all_03_3b$hormones_data3[i] = 1
  } else if (all_03_3b$atc_hormones_03_3b[i] == 1) {
    #if ATC data = 1 then 1
    all_03_3b$hormones_data3[i] = 1
  } else if (all_03_3b$G3C0236[i] == 0) {
    #G3C0236 = 0 then 0
    all_03_3b$hormones_data3[i] = 0
  } else if (all_03_3b$G3C0236[i] %in% c(10,8)) {
    #G3C0236 in (.,8) then NA
    all_03_3b$hormones_data3[i] = NA
  } else {
    #Else NA
    all_03_3b$hormones_data3[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_03_3b)) {
  if (is.na(all_03_3b$hormones_data3[i])) {
    #NA remains NA
    all_03_3b$hormones_core3[i] = NA
  } else if (all_03_3b$G3C0228[i] %in% c(1,2,3) & all_03_3b$hormones_data3[i] == 1) {
    #taking hormones though not in menopause
    all_03_3b$hormones_core3[i] = 2
  } else {
    #Else same value as before
    all_03_3b$hormones_core3[i] = all_03_3b$hormones_data3[i]
  }
}


#### Hormones Logic Merge ####
#merge datasets
horm_list <- list(all_01_3b, all_02_3b, all_03_3b)
horm_joined <- horm_list %>% reduce(full_join, by = c("framid", "id", "idtype"))

#subset for only ID and menopause columns
horm_only <- subset(horm_joined, select = c(framid, id, idtype, hormones_core1, hormones_core2, hormones_core3))

write.csv(horm_only, file = "Hormone_Usage_Variables_Gen3_Omni2_NOS.csv", row.names = FALSE)
