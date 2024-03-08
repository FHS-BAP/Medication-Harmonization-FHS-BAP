# ******************************************************************************************************************************************
# Introduction to Gen 1 hormones/menopause complete dataset creation source code
# ******************************************************************************************************************************************
# 
# Created by Michael Cummings
# Last updated: November 2023
# 
# 
# The purpose of this R code is to create a dataset with five kinds of data relating to hormones and menopause:
# 1) Data from selected questions related to hormone drug categories in exams 1-13 in Gen 1
# 2) Data of the calculated age at menopause of FHS Gen 1 individuals
# 3) Data about drug usage in categories 67-73 in all relevant Gen 1 core exams
# 4) Data about individuals in Gen 1 exams 28-32 who used hormones, collected using ATC codes
# 5) Data about Gen 1 individuals taking hormone drugs, in hormones_core() derived variables
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex07_0_0076_v1.sas7bdat (Gen 1 Exams 1-7)
# e_exam_ex08_0_0077.sas7bdat (Gen 1 Exam 8)
# e_exam_ex09_0_0078.sas7bdat (Gen 1 Exam 9)
# e_exam_ex10_0_0058.sas7bdat (Gen 1 Exam 10)
# e_exam_ex11_0_0059.sas7bdat (Gen 1 Exam 11)
# e_exam_ex12_0_0060_v1.sas7bdat (Gen 1 Exam 12)
# e_exam_ex13_0_0061_v1.sas7bdat (Gen 1 Exam 13)
# e_exam_ex14_0_0062.sas7bdat (Gen 1 Exam 14)
# e_exam_ex15_0_0063.sas7bdat (Gen 1 Exam 15)
# e_exam_ex16_0_0064.sas7bdat (Gen 1 Exam 16)
# e_exam_ex17_0_0065.sas7bdat (Gen 1 Exam 17)
# e_exam_ex18_0_0066.sas7bdat (Gen 1 Exam 18)
# e_exam_ex19_0_0067.sas7bdat (Gen 1 Exam 19)
# e_exam_ex20_0_0068.sas7bdat (Gen 1 Exam 20)
# e_exam_ex21_0_0069.sas7bdat (Gen 1 Exam 21)
# e_exam_ex22_0_0070.sas7bdat (Gen 1 Exam 22)
# e_exam_ex23_0_0071.sas7bdat (Gen 1 Exam 23)
# e_exam_ex24_0_0072.sas7bdat (Gen 1 Exam 24)
# e_exam_ex25_0_0073.sas7bdat (Gen 1 Exam 25)
# e_exam_ex26_0_0074.sas7bdat (Gen 1 Exam 26)
# e_exam_ex27_0_0075.sas7bdat (Gen 1 Exam 27)
# e_exam_ex28_0_0256.sas7bdat (Gen 1 Exam 28)
#
# 2)  Hormone ATC information - ATC2_HormonesCodes_ALL.xlsx
# 
# 3)  Full ATC medication info for patients
# vr_meds_ex28_0_0441.sas7bdat (ATC Info Exam 28)
# vr_meds_ex31_0_0763.sas7bdat (ATC Info Exams 29/30/31)
# vr_meds_ex32_0_0880.sas7bdat (ATC Info Exam 32)
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")

library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files

#### Core exam file reading ####

#This reads the on the individual .sas7bdat files for exams 1-13, which are used below.
exam7 <- read_sas("e_exam_ex07_0_0076_v1.sas7bdat")
exam8 <- read_sas("e_exam_ex08_0_0077.sas7bdat")
exam9 <- read_sas("e_exam_ex09_0_0078.sas7bdat")
exam10 <- read_sas("e_exam_ex10_0_0058.sas7bdat")
exam11 <- read_sas("e_exam_ex11_0_0059.sas7bdat")
exam12 <- read_sas("e_exam_ex12_0_0060_v1.sas7bdat")
exam13 <- read_sas("e_exam_ex13_0_0061_v1.sas7bdat")
exam14 <- read_sas("e_exam_ex14_0_0062.sas7bdat")
exam15 <- read_sas("e_exam_ex15_0_0063.sas7bdat")
exam16 <- read_sas("e_exam_ex16_0_0064.sas7bdat")
exam17 <- read_sas("e_exam_ex17_0_0065.sas7bdat")
exam18 <- read_sas("e_exam_ex18_0_0066.sas7bdat")
exam19 <- read_sas("e_exam_ex19_0_0067.sas7bdat")
exam20 <- read_sas("e_exam_ex20_0_0068.sas7bdat")
exam21 <- read_sas("e_exam_ex21_0_0069.sas7bdat")
exam22 <- read_sas("e_exam_ex22_0_0070.sas7bdat")
exam23 <- read_sas("e_exam_ex23_0_0071.sas7bdat")
exam24 <- read_sas("e_exam_ex24_0_0072.sas7bdat")
exam25 <- read_sas("e_exam_ex25_0_0073.sas7bdat")
exam26 <- read_sas("e_exam_ex26_0_0074.sas7bdat")
exam27 <- read_sas("e_exam_ex27_0_0075.sas7bdat")
exam28 <- read_sas("e_exam_ex28_0_0256.sas7bdat")


#### 67-73 Related Highlights ####

#In each of these sections an individual .sas7bdat file is read
#Then columns containing the data for questions we want are isolated
#The NA data is replaced with what the documentation denotes for "unknown"
#This can change between exams

#Exam 1-7
#MF121, MF238, MF239, MF326, MF327, MF422, MF423, MF533
related_select7 <- exam7[,c("ID", "MF121", "MF238", "MF239", "MF326", "MF327", "MF422", "MF423", "MF533")]
related_select7 <- related_select7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9999")))

#Exam 8
#FA131, FA132, FA133
related_select8 <- exam8[,c("ID", "FA131", "FA132", "FA133")]
related_select8 <- related_select8 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9999")))

#Exam 9
#FB77, FB78
related_select9 <- exam9[,c("ID", "FB77", "FB78")]
related_select9 <- related_select9 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9999")))

#Exam 10
#FC93, FC94
related_select10 <- exam10[,c("ID", "FC93", "FC94")]
related_select10$ID <- as.character(related_select10$ID)
related_select10$FC93[is.na(related_select10$FC93)] <- 9
related_select10$FC94[is.na(related_select10$FC94)] <- 99

#Exam 11
#FD87, FD88
related_select11 <- exam11[,c("ID", "FD87", "FD88")]
related_select11$ID <- as.character(related_select11$ID)
related_select11$FD87[is.na(related_select11$FD87)] <- 9
related_select11$FD88[is.na(related_select11$FD88)] <- 99

#Exam 12
#FE101, FE102
related_select12 <- exam12[,c("ID", "FE101", "FE102")]
related_select12$ID <- as.character(related_select12$ID)
related_select12$FE101[is.na(related_select12$FE101)] <- 9
related_select12$FE102[is.na(related_select12$FE102)] <- 99

#Exam 13
#FF103, FF104
related_select13 <- exam13[,c("ID", "FF103", "FF104")]
related_select13 <- related_select13 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Joining columns together
rs_list <- list(related_select7, related_select8, related_select9, related_select10, related_select11, related_select12, related_select13)
rs_joined <- rs_list %>% reduce(full_join, by='ID')
#Ordering the columns, they can be out of order after the join
rs_ordered <- rs_joined %>% arrange(as.numeric(ID))

#### 67-73 Drugs ####

#In each of these sections an individual .sas7bdat file is read
#Then columns containing the data for questions we want are isolated
#The NA data is replaced with what the documentation denotes for "unknown"
#This can change between exams

#Exam 1-7
#MF130, MF328, MF534, MF535
drugs_select7 <- exam7[,c("ID", "MF130", "MF328", "MF534", "MF535")]
drugs_select7 <- drugs_select7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9999")))

#Exam 8
#FA135, FA136
drugs_select8 <- exam8[,c("ID", "FA135", "FA136")]
drugs_select8 <- drugs_select8 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9999")))

#Exam 9
#FB80
drugs_select9 <- exam9[,c("ID", "FB80")]
drugs_select9 <- drugs_select9 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9999")))

#Exam 10
#FC96
drugs_select10 <- exam10[,c("ID", "FC96")]
drugs_select10 <- drugs_select10 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9")))

#Exam 11
#FD86
drugs_select11 <- exam11[,c("ID", "FD86")]
drugs_select11 <- drugs_select11 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9")))

#Exam 12
#FE99
drugs_select12 <- exam12[,c("ID", "FE99")]
drugs_select12 <- drugs_select12 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), "9")))

#Exam 13
#FF101
drugs_select13 <- exam13[,c("ID", "FF101")]
drugs_select13 <- drugs_select13 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 14
#FG100
drugs_select14 <- exam14[,c("ID", "FG100")]
drugs_select14 <- drugs_select14 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 15
#FH99
drugs_select15 <- exam15[,c("ID", "FH99")]
drugs_select15 <- drugs_select15 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 16
#FI58
drugs_select16 <- exam16[,c("ID", "FI58")]
drugs_select16 <- drugs_select16 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 17
#FJ28
drugs_select17 <- exam17[,c("ID", "FJ28")]
drugs_select17 <- drugs_select17 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 18
#FK112, FK113
drugs_select18 <- exam18[,c("ID", "FK112", "FK113")]
drugs_select18 <- drugs_select18 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 19
#FL147, FL174-177
drugs_select19 <- exam19[,c("ID", "FL147", "FL174", "FL175", "FL176", "FL177")]
drugs_select19 <- drugs_select19 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 20
#FM185, FM202-206
drugs_select20 <- exam20[,c("ID", "FM185", "FM202", "FM203", "FM204", "FM205", "FM206")]
drugs_select20 <- drugs_select20 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 21
#FN124, FN152-156
drugs_select21 <- exam21[,c("ID", "FN124", "FN152", "FN153", "FN154", "FN155", "FN156")]
drugs_select21 <- drugs_select21 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 22
#FO131, FO151-FO156
exam22 <- read_sas("e_exam_ex22_0_0070.sas7bdat")
drugs_select22 <- exam22[,c("ID", "FO131", "FO151", "FO152", "FO153", "FO154", "FO155", "FO156")]
drugs_select22 <- drugs_select22 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 23
#FP085, FP121-FP127
exam23 <- read_sas("e_exam_ex23_0_0071.sas7bdat")
drugs_select23 <- exam23[,c("ID", "FP085", "FP121", "FP122", "FP123", "FP124", "FP125", "FP126", "FP127")]
drugs_select23 <- drugs_select23 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 24
#FQ170, FQ186-FE190
drugs_select24 <- exam24[,c("ID", "FQ170", "FQ186", "FQ187", "FQ188", "FQ189", "FQ190")]
drugs_select24 <- drugs_select24 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 25
#FR222, FR241-FR248
drugs_select25 <- exam25[,c("ID", "FR222", "FR241", "FR242", "FR243", "FR244", "FR245", "FR246", "FR247", "FR248")]
drugs_select25 <- drugs_select25 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 26
#FS290, FS314-FS321
drugs_select26 <- exam26[,c("ID", "FS290", "FS314", "FS315", "FS316", "FS317", "FS318", "FS319", "FS320", "FS321")]
drugs_select26 <- drugs_select26 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 27
#FT297, FT323-FT330
drugs_select27 <- exam27[,c("ID", "FT297", "FT323", "FT324", "FT325", "FT326", "FT327", "FT328", "FT329", "FT330")]
drugs_select27 <- drugs_select27 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 28
#FU023-FU031
drugs_select28 <- exam28[,c("ID", "FU023", "FU024", "FU025", "FU026", "FU027", "FU028", "FU029", "FU030", "FU031")]
drugs_select28 <- drugs_select28 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Joining columns together
ds_list <- list(drugs_select7, drugs_select8, drugs_select9, drugs_select10, drugs_select11,
                drugs_select12, drugs_select13, drugs_select14, drugs_select15, drugs_select16,
                drugs_select17, drugs_select18, drugs_select19, drugs_select20, drugs_select21, 
                drugs_select22, drugs_select23, drugs_select24, drugs_select25, drugs_select26,
                drugs_select27, drugs_select28)
ds_joined <- ds_list %>% reduce(full_join, by='ID')
#Ordering the columns, they can be out of order after the join
ds_ordered <- ds_joined %>% arrange(as.numeric(ID))

#### Hormones ATC ####

#Reading drug files and selecting columns containing ATC numbers
#Meds 28
meds_28 <- read_sas("vr_meds_ex28_0_0441.sas7bdat")
select28 <- meds_28[,c("ID", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Reading joined 29/30/31 drugs file
meds_29_30_31 <- read_sas("vr_meds_ex31_0_0763.sas7bdat")

#Meds 29
meds_29 <- subset(meds_29_30_31, exam == 29)
select29 <- meds_29[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Meds 30
meds_30 <- subset(meds_29_30_31, exam == 30)
select30 <- meds_30[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Meds 31
meds_31 <- subset(meds_29_30_31, exam == 31)
select31 <- meds_31[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Meds 32
meds_32 <- read_sas("vr_meds_ex32_0_0880.sas7bdat")
select32 <- meds_32[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#reading hormone drug lists from excel spreadsheet
hormone_codes <- read_excel("ATC2_HormonesCodes_ALL.xlsx", sheet = "Hormones_ALL")
hormone_atc_list <- hormone_codes$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
hormone_filtered28 <- select28 %>% filter_all(any_vars(. %in% hormone_atc_list))
hormone_filtered29 <- select29 %>% filter_all(any_vars(. %in% hormone_atc_list))
hormone_filtered30 <- select30 %>% filter_all(any_vars(. %in% hormone_atc_list))
hormone_filtered31 <- select31 %>% filter_all(any_vars(. %in% hormone_atc_list))
hormone_filtered32 <- select32 %>% filter_all(any_vars(. %in% hormone_atc_list))

#Constructing the output data frame
ID28 <- unique(select28$ID)
med_found28 <- data.frame(ID28)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found28['horm_column'] <- as.integer(med_found28$ID28 %in% hormone_filtered28$ID)

#Constructing the output data frame
ID29 <- unique(select29$id)
med_found29 <- data.frame(ID29)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found29['horm_column'] <- as.integer(med_found29$ID29 %in% hormone_filtered29$id)

#Constructing the output data frame
ID30 <- unique(select30$id)
med_found30 <- data.frame(ID30)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found30['horm_column'] <- as.integer(med_found30$ID30 %in% hormone_filtered30$id)

#Constructing the output data frame
ID31 <- unique(select31$id)
med_found31 <- data.frame(ID31)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found31['horm_column'] <- as.integer(med_found31$ID31 %in% hormone_filtered31$id)

#Constructing the output data frame
ID32 <- unique(select32$id)
med_found32 <- data.frame(ID32)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found32['horm_column'] <- as.integer(med_found32$ID32 %in% hormone_filtered32$id)

#standardizing column names
colnames(med_found28) <- c("ID", "Hormones_28")
colnames(med_found29) <- c("ID", "Hormones_29")
colnames(med_found30) <- c("ID", "Hormones_30")
colnames(med_found31) <- c("ID", "Hormones_31")
colnames(med_found32) <- c("ID", "Hormones_32")

#merge seperate exam data
med_list <- list(med_found28, med_found29, med_found30, med_found31, med_found32)
med_joined <- med_list %>% reduce(full_join, by = "ID")
#reorder by ID
med_final <- med_joined %>% arrange(ID)
#change ID column class to character for merge later
med_final$ID <- as.character(med_final$ID)

#### Menopause Section ####

#### Menopause data abstraction ####

#Each of these sections is based on the individual .sas7bdat file read above
#The question on an individual's sex is isolated and used to create a subset of only women
#The columns containing the data for menopause questions we want are isolated
#In some cases recoding of the data is necessary to create  a 0/1/NA menopause variable
#These all can change between exams

#Exam 1-7
#MF3 (Sex), MF121, MF238, MF326, MF422, MF533
read1_both7 <- exam7[,c("ID", "MF3", "MF121", "MF238", "MF326", "MF422", "MF533")]
read1_select7 <- subset(read1_both7, MF3 == 2)
read1_select7$menopause_status_core2 <- read1_select7$MF121
read1_select7$menopause_status_core2 <- replace(read1_select7$menopause_status_core2, read1_select7$menopause_status_core2 > 0, 1)
read1_select7$menopause_status_core4 <- read1_select7$MF238
read1_select7$menopause_status_core4 <- replace(read1_select7$menopause_status_core4, read1_select7$menopause_status_core4 > 0, 1)
read1_select7$menopause_status_core5 <- read1_select7$MF326
read1_select7$menopause_status_core5 <- replace(read1_select7$menopause_status_core5, read1_select7$menopause_status_core5 == 3 | read1_select7$menopause_status_core5 == 4 | read1_select7$menopause_status_core5 == 8, 0)
read1_select7$menopause_status_core5 <- replace(read1_select7$menopause_status_core5, read1_select7$menopause_status_core5 == 1 | read1_select7$menopause_status_core5 == 2 | read1_select7$menopause_status_core5 == 7, 1)
read1_select7$menopause_status_core5 <- replace(read1_select7$menopause_status_core5, read1_select7$menopause_status_core5 == 5 | read1_select7$menopause_status_core5 == 6, NA)
read1_select7$menopause_status_core6 <- read1_select7$MF422
read1_select7$menopause_status_core7 <- read1_select7$MF533
read1_select7$menopause_status_core7 <- replace(read1_select7$menopause_status_core7, read1_select7$menopause_status_core7 == 2000, 0)
read1_select7$menopause_status_core7 <- replace(read1_select7$menopause_status_core7, read1_select7$menopause_status_core7 >= 16 & read1_select7$menopause_status_core7 <= 58, 1)
read1_select7$menopause_status_core7 <- replace(read1_select7$menopause_status_core7, read1_select7$menopause_status_core7 == 1999, NA)

#Exam 8
#FA29 (Sex), FA132
read1_both8 <- exam8[,c("ID", "FA29", "FA132")]
read1_select8 <- subset(read1_both8, FA29 == 2)
read1_select8$menopause_status_core8 <- read1_select8$FA132
read1_select8$menopause_status_core8 <- replace(read1_select8$menopause_status_core8, read1_select8$menopause_status_core8 > 0, 1)

#Exam 9
#FB2 (Sex), FB77
read1_both9 <- exam9[,c("ID", "FB2", "FB77")]
read1_select9 <- subset(read1_both9, FB2 == 2)
read1_select9$menopause_status_core9 <- read1_select9$FB77
read1_select9$menopause_status_core9 <- replace(read1_select9$menopause_status_core9, read1_select9$menopause_status_core9 == 992, 0)

#Exam 10
#FC2 (Sex), FC93
read1_both10 <- exam10[,c("ID", "FC2", "FC93")]
read1_select10 <- subset(read1_both10, FC2 == 2)
read1_select10$menopause_status_core10 <- read1_select10$FC93

#Exam 11
#FD2 (Sex), FD87
read1_both11 <- exam11[,c("ID", "FD2", "FD87")]
read1_select11 <- subset(read1_both11, FD2 == 2)
read1_select11$menopause_status_core11 <- read1_select11$FD87

#Exam 12
#FE2 (Sex), FE101
read1_both12 <- exam12[,c("ID", "FE2", "FE101")]
read1_select12 <- subset(read1_both12, FE2 == 2)
read1_select12$menopause_status_core12 <- read1_select12$FE101

#Exam 13
#FF103
read1_both13 <- exam13[,c("ID", "FF103")]
read1_select13 <- subset(read1_both13, (read1_both13$ID %in% read1_select7$ID))
read1_select13$menopause_status_core13 <- read1_select13$FF103

#join data frames containing menopause columns
read1_df_list <- list(read1_select7, read1_select8, read1_select9, read1_select10, read1_select11, read1_select12, read1_select13)
read1_df_joined <- read1_df_list %>% reduce(full_join, by='ID')

#subset for only ID and menopause columns
df_1_orig <- subset(read1_df_joined, select = c(ID, menopause_status_core2, menopause_status_core4, menopause_status_core5, menopause_status_core6, menopause_status_core7, menopause_status_core8, menopause_status_core9, menopause_status_core10, menopause_status_core11, menopause_status_core12, menopause_status_core13))

#fixing error found with high rate of 0 in menopause_status_core4
for (i in 1:nrow(df_1_orig)) {
  if (df_1_orig$menopause_status_core2[i] == 1 & !is.na(df_1_orig$menopause_status_core2[i]) & !is.na(df_1_orig$menopause_status_core4[i])) {
    df_1_orig$menopause_status_core4[i] <- 1
  }
}

#column that determines if when an individual reports menopause they remain in menopause in future exams 
df_1_orig$is_logical = NA
for (i in 1:nrow(df_1_orig)) {
  df_1_orig$is_logical[i] <- !is.unsorted(as.numeric(df_1_orig[i,2:12]), na.rm = TRUE)
}

#### Age data abstraction ####

#Each of these sections is based on the individual .sas7bdat file read above
#The question on an individual's sex is isolated and used to create a subset of only women
#The columns containing the data for age questions we want are isolated
#In some cases NA data is replaced with what the documentation denotes for "unknown"
#Also recoding of the 0 data is sometimes necessary. These both can change between exams

#Exam 1-7
#MF3 (Sex), MF239, MF327, MF423, MF533
read2_both7 <- exam7[,c("ID", "MF3", "MF239", "MF327", "MF423", "MF533")]
read2_select7 <- subset(read2_both7, MF3 == 2)
read2_select7$menopause_age_core4 <- read2_select7$MF239
read2_select7$menopause_age_core5 <- read2_select7$MF327
read2_select7$menopause_age_core6 <- read2_select7$MF423
read2_select7$menopause_age_core7 <- read2_select7$MF533
read2_select7$menopause_age_core7 <- replace(read2_select7$menopause_age_core7, read2_select7$menopause_age_core7 == 2000, 0)
read2_select7$menopause_age_core7 <- replace(read2_select7$menopause_age_core7, read2_select7$menopause_age_core7 == 1999, NA)

#Exam 8
#FA29 (Sex), FA131
read2_both8 <- exam8[,c("ID", "FA29", "FA131")]
read2_select8 <- subset(read2_both8, FA29 == 2)
read2_select8$menopause_age_core8 <- read2_select8$FA131

#Exam 9
#FB2 (Sex), FB78
read2_both9 <- exam9[,c("ID", "FB2", "FB78")]
read2_select9 <- subset(read2_both9, FB2 == 2)
read2_select9$menopause_age_core9 <- read2_select9$FB78
read2_select9$menopause_age_core9 <- replace(read2_select9$menopause_age_core9, read2_select9$menopause_age_core9 == 992, 0)

#Exam 10
#FC2 (Sex), FC93
read2_both10 <- exam10[,c("ID", "FC2", "FC94")]
read2_select10 <- subset(read2_both10, FC2 == 2)
read2_select10$menopause_age_core10 <- read2_select10$FC94

#Exam 11
#FD2 (Sex), FD88
read2_both11 <- exam11[,c("ID", "FD2", "FD88")]
read2_select11 <- subset(read2_both11, FD2 == 2)
read2_select11$menopause_age_core11 <- read2_select11$FD88
read2_select11$menopause_age_core11 <- replace(read2_select11$menopause_age_core11, read2_select11$menopause_age_core11 == 99, 9999)
#this is the only one in all the questions that actually records 99 as an answer

#Exam 12
#FE2 (Sex), FE101
read2_both12 <- exam12[,c("ID", "FE2", "FE102")]
read2_select12 <- subset(read2_both12, FE2 == 2)
read2_select12$menopause_age_core12 <- read2_select12$FE102

#Exam 13
#FF104
read2_both13 <- exam13[,c("ID", "FF104")]
read2_select13 <- subset(read2_both13, (read2_both13$ID %in% read2_select7$ID))
read2_select13$menopause_age_core13 <- read2_select13$FF104

#join data frames containing age columns
read2_df_list <- list(read2_select7, read2_select8, read2_select9, read2_select10, read2_select11, read2_select12, read2_select13)
read2_df_joined <- read2_df_list %>% reduce(full_join, by='ID')

#subset for only ID and age columns
read2_df_subset <- subset(read2_df_joined, select = c(ID, menopause_age_core4, menopause_age_core5, menopause_age_core6, menopause_age_core7, menopause_age_core8, menopause_age_core9, menopause_age_core10, menopause_age_core11, menopause_age_core12, menopause_age_core13))
#replace 0 and 9999 with NA
read2_df_logic <- replace(read2_df_subset, read2_df_subset == 0 | read2_df_subset == 9999, NA)

#find maximum age
read2_df_logic$age_max<-pmax(read2_df_logic$menopause_age_core4, read2_df_logic$menopause_age_core5, read2_df_logic$menopause_age_core6, read2_df_logic$menopause_age_core7,
                             read2_df_logic$menopause_age_core8, read2_df_logic$menopause_age_core9, read2_df_logic$menopause_age_core10, read2_df_logic$menopause_age_core11,
                             read2_df_logic$menopause_age_core12, read2_df_logic$menopause_age_core13, na.rm = TRUE)
#find minimum age
read2_df_logic$age_min<-pmin(read2_df_logic$menopause_age_core4, read2_df_logic$menopause_age_core5, read2_df_logic$menopause_age_core6, read2_df_logic$menopause_age_core7,
                             read2_df_logic$menopause_age_core8, read2_df_logic$menopause_age_core9, read2_df_logic$menopause_age_core10, read2_df_logic$menopause_age_core11,
                             read2_df_logic$menopause_age_core12, read2_df_logic$menopause_age_core13, na.rm = TRUE)
#find age difference
read2_df_logic$age_difference<-(read2_df_logic$age_max-read2_df_logic$age_min)

#bring columns over from other data frame to new one
df_2_orig <- read2_df_subset
df_2_orig$age_max = read2_df_logic$age_max
df_2_orig$age_min = read2_df_logic$age_min
df_2_orig$age_difference = read2_df_logic$age_difference

#find individuals with large ranges between reported ages of menopause in different exams 
df_large_range <- subset(df_2_orig, age_difference > 5)


#### Large range checking ####

#subset df_1_orig with large range IDs only
df_logic_subset <- subset(df_1_orig, ID %in% df_large_range$ID)
#removing menopause_status_core2 for loop below - all discrepancies have been previously checked
df_logic_final <- df_logic_subset[,-2] 
#create empty columns before use
df_logic_final[ , c('col_num', 'corresponding_age')] <- NA

#finds the corresponding age from maximum age in row in other dataset
for (i in 1:nrow(df_logic_final)) {
  df_logic_final$col_num[i] <- match(1, df_logic_final[i,])
  df_logic_final$corresponding_age[i] <- df_large_range[i,df_logic_final$col_num[i]]
  if (is.na(df_logic_final$corresponding_age[i]) | df_logic_final$corresponding_age[i] == 0) {
    df_logic_final$corresponding_age[i] <- as.numeric(df_large_range[i,2:11][which.max(df_large_range[i,2:11] > 0)])
  }
}

#create column to get number of column data is in, which the exam number can be found from
df_logic_subset$col_num = NA
for (i in 1:nrow(df_logic_subset)) {
  df_logic_subset$col_num[i] <- (1 + match(1, df_logic_subset[i,2:12]))
}
df_logic_subset$col_num[is.na(df_logic_subset$col_num)] <- 0
df_logic_subset$overall_menopause_exam = NA
#finding the exam number, taking account of exams without data present
for (i in 1:nrow(df_logic_subset)) {
  if (df_logic_subset$col_num[i] == 2) {
    df_logic_subset$overall_menopause_exam[i] = 2
  } else if (df_logic_subset$col_num[i] > 2) {
    df_logic_subset$overall_menopause_exam[i] = (df_logic_subset$col_num[i] + 1)
  }
}

#removing columns that don't need to be in future datasets
pre_merge <- subset(df_logic_subset, select = -c(is_logical, col_num) )
age_df <- subset(df_logic_final, select = c(ID, corresponding_age) )
df_ages <- subset(df_large_range, select = -c(age_max, age_min, age_difference) )

#merging to create list of 156 observations that have menopause age discrepancies with range greater than five
first_merge <- merge(pre_merge, age_df, by="ID")
checked_156 <- merge(df_ages, first_merge, by="ID")

#### Final menopause data summary construction ####

#removes the 156 from the two datasets
df_1_main <- subset(df_1_orig, !(ID %in% checked_156$ID))
df_2_main <- subset(df_2_orig, !(ID %in% checked_156$ID))

#create column to get number of column data is in, which the exam number can be gotten from
df_1_main$col_num = NA
for (i in 1:nrow(df_1_main)) {
  df_1_main$col_num[i] <- (1 + match(1, df_1_main[i,2:12]))
}
df_1_main$col_num[is.na(df_1_main$col_num)] <- 0
#finding the exam number, taking account of exams without data present
df_1_main$overall_menopause_exam = NA
for (i in 1:nrow(df_1_main)) {
  if (df_1_main$col_num[i] == 2) {
    df_1_main$overall_menopause_exam[i] = 2
  } else if (df_1_main$col_num[i] > 2) {
    df_1_main$overall_menopause_exam[i] = (df_1_main$col_num[i] + 1)
  }
}

#NA replacement
df_2_main$menopause_age_core11 <- replace(df_2_main$menopause_age_core11, df_2_main$menopause_age_core11 == 9999, NA)
#create age column
df_2_main$corresponding_age <- rowSums(df_2_main[2:11], na.rm=TRUE)/rowSums(!!df_2_main[2:11], na.rm = TRUE)

#removing columns that won't be in the final spreadsheet
df_1_final <- subset(df_1_main, select = -c(is_logical, col_num) )
df_2_final <- subset(df_2_main, select = -c(age_max, age_min, age_difference) )
#merging datasets 1 and 2
df_merged <- merge(df_2_final, df_1_final, by="ID")
#changing position of column
df_relocated <- df_merged %>% relocate(corresponding_age, .after = last_col())

#replaces the 156 individuals' ages with 9999 meaning NA
checked_156$menopause_age_core11 <- replace(checked_156$menopause_age_core11, checked_156$menopause_age_core11 == 9999, NA)
checked_156$corresponding_age <- 9999
checked_156$corresponding_age <- as.numeric(checked_156$corresponding_age)

#bringing the 156 and the rest of the individuals back together
df_all <- rbind(df_relocated, checked_156)
colnames(df_all)[24] <- "overall_menopause_age"

#change NaN to NA
df_all$overall_menopause_age[is.nan(df_all$overall_menopause_age)] <- NA
#put IDs in order
df_all_ordered <- df_all %>% arrange(ID)
#change ID column class to character for merge later
df_all_ordered$ID <- as.character(df_all_ordered$ID)

#### Four-Source Merge ####

#merge four data sources
new_list <- list(rs_ordered, df_all_ordered, ds_ordered, med_final)
new_joined <- new_list %>% reduce(full_join, by = "ID")

#write final spreadsheet (currently commented out)
#write.csv(new_joined, file = "Hormones_Menopause_Full_Merged_Data.csv", row.names = FALSE)

#### Hormones Logic Section 2-13 ####

num_df <-  subset(new_joined, select = c(ID, overall_menopause_exam)) 
num_df$ID <- as.numeric(num_df$ID)
num_df$overall_menopause_exam_old <- as.numeric(num_df$overall_menopause_exam)
num_df$sex <- exam7$MF3
num_df$overall_menopause_exam <- num_df$overall_menopause_exam_old
#set all NA females to 13 
for (i in 1:nrow(num_df)) {
  if (is.na(num_df$overall_menopause_exam_old[i])) {
    #second loop regarding sex
    if (num_df$sex[i] == 2) {
      #If menopause exam is NA and the participant sex = 2 (female), then set as 13.
      num_df$overall_menopause_exam[i] = 13
    } else {
      #If menopause exam is NA and the participant sex = 1 (male), then stay NA.
      num_df$overall_menopause_exam[i] = NA
    }
  }
}


#Exam 2
#MF130
select_02_0 <- exam7[,c("ID", "MF130")]

#for consistency
select_02_0$hormones_data2 <- select_02_0$MF130

#preparation for final hormones_core variable
final_02_0 <- left_join(select_02_0, num_df, by = "ID")

#Placeholder values, used for testing
final_02_0$hormones_core2 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_02_0)) {
  if (is.na(final_02_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_02_0$hormones_data2[i])) {
      #correct NA
      final_02_0$hormones_core2[i] = NA
    } else if (final_02_0$hormones_data2[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_02_0$hormones_core2[i] = 1000
    } else {
      #Male should be NA no matter what
      final_02_0$hormones_core2[i] = NA
    }
  } else if (final_02_0$overall_menopause_exam[i] > 2) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_02_0$hormones_data2[i])) {
      #NA = NA
      final_02_0$hormones_core2[i] = NA
    } else if (final_02_0$hormones_data2[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_02_0$hormones_core2[i] = 2
    } else if (final_02_0$hormones_data2[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_02_0$hormones_core2[i] = 0
    }
  } else if (final_02_0$overall_menopause_exam[i] <= 2) {
    #no alteration
    final_02_0$hormones_core2[i] = final_02_0$hormones_data2[i]
  }
}

#replace NA
final_02_0 <- final_02_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 5
#MF328
select_05_0 <- exam7[,c("ID", "MF328")]

#Placeholder values, used for testing
select_05_0$hormones_data5 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_05_0)) {
  if (is.na(select_05_0$MF328[i])) {
    #Unknown = NA
    select_05_0$hormones_data5[i] = NA
  } else if (select_05_0$MF328[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #MF328 in (9997,9999,88,2000,7,8,9,992) then NA
    select_05_0$hormones_data5[i] = NA
  } else if (select_05_0$MF328[i] == 0) {
    #MF328 = 0 then 0
    select_05_0$hormones_data5[i] = 0
  } else if (select_05_0$MF328[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_05_0$hormones_data5[i] = 1
  }
}

#preparation for final hormones_core variable
final_05_0 <- left_join(select_05_0, num_df, by = "ID")

#Placeholder values, used for testing
final_05_0$hormones_core5 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_05_0)) {
  if (is.na(final_05_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_05_0$hormones_data5[i])) {
      #correct NA
      final_05_0$hormones_core5[i] = NA
    } else if (final_05_0$hormones_data5[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_05_0$hormones_core5[i] = 1000
    } else {
      #Male should be NA no matter what
      final_05_0$hormones_core5[i] = NA
    }
  } else if (final_05_0$overall_menopause_exam[i] > 5) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_05_0$hormones_data5[i])) {
      #NA = NA
      final_05_0$hormones_core5[i] = NA
    } else if (final_05_0$hormones_data5[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_05_0$hormones_core5[i] = 2
    } else if (final_05_0$hormones_data5[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_05_0$hormones_core5[i] = 0
    }
  } else if (final_05_0$overall_menopause_exam[i] <= 5) {
    #no alteration
    final_05_0$hormones_core5[i] = final_05_0$hormones_data5[i]
  }
}

#replace NA
final_05_0 <- final_05_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 7
#MF535
select_07_0 <- exam7[,c("ID", "MF535")]

#Placeholder values, used for testing
select_07_0$hormones_data7 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_07_0)) {
  if (is.na(select_07_0$MF535[i])) {
    #Unknown = NA
    select_07_0$hormones_data7[i] = NA
  } else if (select_07_0$MF535[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #MF535 in (9997,9999,88,2000,7,8,9,992) then NA
    select_07_0$hormones_data7[i] = NA
  } else if (select_07_0$MF535[i] == 0) {
    #MF535 = 0 then 0
    select_07_0$hormones_data7[i] = 0
  } else if (select_07_0$MF535[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_07_0$hormones_data7[i] = 1
  }
}

#preparation for final hormones_core variable
final_07_0 <- left_join(select_07_0, num_df, by = "ID")

#Placeholder values, used for testing
final_07_0$hormones_core7 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_07_0)) {
  if (is.na(final_07_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_07_0$hormones_data7[i])) {
      #correct NA
      final_07_0$hormones_core7[i] = NA
    } else if (final_07_0$hormones_data7[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_07_0$hormones_core7[i] = 1000
    } else {
      #Male should be NA no matter what
      final_07_0$hormones_core7[i] = NA
    }
  } else if (final_07_0$overall_menopause_exam[i] > 7) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_07_0$hormones_data7[i])) {
      #NA = NA
      final_07_0$hormones_core7[i] = NA
    } else if (final_07_0$hormones_data7[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_07_0$hormones_core7[i] = 2
    } else if (final_07_0$hormones_data7[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_07_0$hormones_core7[i] = 0
    }
  } else if (final_07_0$overall_menopause_exam[i] <= 7) {
    #no alteration
    final_07_0$hormones_core7[i] = final_07_0$hormones_data7[i]
  }
}

#replace NA
final_07_0 <- final_07_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 8
#FA135, FA136
select_08_0 <- exam8[,c("ID", "FA135", "FA136")]

#Placeholder values, used for testing
select_08_0$combined_core8 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_08_0)) {
  if (is.na(select_08_0$FA135[i]) | is.na(select_08_0$FA136[i])) {
    #Unknown = NA
    select_08_0$combined_core8[i] = NA
  } else if (select_08_0$FA135[i] %in% c(2000,9999,88) | select_08_0$FA136[i] %in% c(2000,9999,88)) {
    #FA135 in (2000,9999,88) or FA136 in (2000,9999,88) then NA
    select_08_0$combined_core8[i] = NA
  } else if (select_08_0$FA135[i] %in% c(1,2,3,4,5,6,7,8) | select_08_0$FA136[i] %in% c(0,1,2,3,4,5,6,7,8)) {
    #FA135 in (1,2,3,4,5,6,7,8) or FA136 in (0,1,2,3,4,5,6,7,8) then 1
    select_08_0$combined_core8[i] = 1
  } else {
    #else 0
    select_08_0$combined_core8[i] = 0
  }
}

#preparation for final hormones_core variable
final_08_0 <- left_join(select_08_0, num_df, by = "ID")

#Placeholder values, used for testing
final_08_0$hormones_core8 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_08_0)) {
  if (is.na(final_08_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_08_0$combined_core8[i])) {
      #correct NA
      final_08_0$hormones_core8[i] = NA
    } else if (final_08_0$combined_core8[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_08_0$hormones_core8[i] = 1000
    } else {
      #Male should be NA no matter what
      final_08_0$hormones_core8[i] = NA
    }
  } else if (final_08_0$overall_menopause_exam[i] > 8) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_08_0$combined_core8[i])) {
      #NA = NA
      final_08_0$hormones_core8[i] = NA
    } else if (final_08_0$combined_core8[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_08_0$hormones_core8[i] = 2
    } else if (final_08_0$combined_core8[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_08_0$hormones_core8[i] = 0
    }
  } else if (final_08_0$overall_menopause_exam[i] <= 8) {
    #no alteration
    final_08_0$hormones_core8[i] = final_08_0$combined_core8[i]
  }
}

#replace NA
final_08_0 <- final_08_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 9
#FB80
select_09_0 <- exam9[,c("ID", "FB80")]

#Placeholder values, used for testing
select_09_0$hormones_data9 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_09_0)) {
  if (is.na(select_09_0$FB80[i])) {
    #Unknown = NA
    select_09_0$hormones_data9[i] = NA
  } else if (select_09_0$FB80[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #FB80 in (9997,9999,88,2000,7,8,9,992) then NA
    select_09_0$hormones_data9[i] = NA
  } else if (select_09_0$FB80[i] == 0) {
    #FB80 = 0 then 0
    select_09_0$hormones_data9[i] = 0
  } else if (select_09_0$FB80[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_09_0$hormones_data9[i] = 1
  }
}

#preparation for final hormones_core variable
final_09_0 <- left_join(select_09_0, num_df, by = "ID")

#Placeholder values, used for testing
final_09_0$hormones_core9 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_09_0)) {
  if (is.na(final_09_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_09_0$hormones_data9[i])) {
      #correct NA
      final_09_0$hormones_core9[i] = NA
    } else if (final_09_0$hormones_data9[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_09_0$hormones_core9[i] = 1000
    } else {
      #Male should be NA no matter what
      final_09_0$hormones_core9[i] = NA
    }
  } else if (final_09_0$overall_menopause_exam[i] > 9) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_09_0$hormones_data9[i])) {
      #NA = NA
      final_09_0$hormones_core9[i] = NA
    } else if (final_09_0$hormones_data9[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_09_0$hormones_core9[i] = 2
    } else if (final_09_0$hormones_data9[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_09_0$hormones_core9[i] = 0
    }
  } else if (final_09_0$overall_menopause_exam[i] <= 9) {
    #no alteration
    final_09_0$hormones_core9[i] = final_09_0$hormones_data9[i]
  }
}

#replace NA
final_09_0 <- final_09_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 10
#FC96
select_10_0 <- exam10[,c("ID", "FC96")]

#Placeholder values, used for testing
select_10_0$hormones_data10 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_10_0)) {
  if (is.na(select_10_0$FC96[i])) {
    #Unknown = NA
    select_10_0$hormones_data10[i] = NA
  } else if (select_10_0$FC96[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #FC96 in (9997,9999,88,2000,7,8,9,992) then NA
    select_10_0$hormones_data10[i] = NA
  } else if (select_10_0$FC96[i] == 0) {
    #FC96 = 0 then 0
    select_10_0$hormones_data10[i] = 0
  } else if (select_10_0$FC96[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_10_0$hormones_data10[i] = 1
  }
}


#preparation for final hormones_core variable
final_10_0 <- left_join(select_10_0, num_df, by = "ID")

#Placeholder values, used for testing
final_10_0$hormones_core10 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_10_0)) {
  if (is.na(final_10_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_10_0$hormones_data10[i])) {
      #correct NA
      final_10_0$hormones_core10[i] = NA
    } else if (final_10_0$hormones_data10[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_10_0$hormones_core10[i] = 1000
    } else {
      #Male should be NA no matter what
      final_10_0$hormones_core10[i] = NA
    }
  } else if (final_10_0$overall_menopause_exam[i] > 10) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_10_0$hormones_data10[i])) {
      #NA = NA
      final_10_0$hormones_core10[i] = NA
    } else if (final_10_0$hormones_data10[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_10_0$hormones_core10[i] = 2
    } else if (final_10_0$hormones_data10[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_10_0$hormones_core10[i] = 0
    }
  } else if (final_10_0$overall_menopause_exam[i] <= 10) {
    #no alteration
    final_10_0$hormones_core10[i] = final_10_0$hormones_data10[i]
  }
}

#replace NA
final_10_0 <- final_10_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 11
#FD86
select_11_0 <- exam11[,c("ID", "FD86")]

#Placeholder values, used for testing
select_11_0$hormones_data11 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_11_0)) {
  if (is.na(select_11_0$FD86[i])) {
    #Unknown = NA
    select_11_0$hormones_data11[i] = NA
  } else if (select_11_0$FD86[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #FD86 in (9997,9999,88,2000,7,8,9,992) then NA
    select_11_0$hormones_data11[i] = NA
  } else if (select_11_0$FD86[i] == 0) {
    #FD86 = 0 then 0
    select_11_0$hormones_data11[i] = 0
  } else if (select_11_0$FD86[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_11_0$hormones_data11[i] = 1
  }
}

#preparation for final hormones_core variable
final_11_0 <- left_join(select_11_0, num_df, by = "ID")

#Placeholder values, used for testing
final_11_0$hormones_core11 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_11_0)) {
  if (is.na(final_11_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_11_0$hormones_data11[i])) {
      #correct NA
      final_11_0$hormones_core11[i] = NA
    } else if (final_11_0$hormones_data11[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_11_0$hormones_core11[i] = 1000
    } else {
      #Male should be NA no matter what
      final_11_0$hormones_core11[i] = NA
    }
  } else if (final_11_0$overall_menopause_exam[i] > 11) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_11_0$hormones_data11[i])) {
      #NA = NA
      final_11_0$hormones_core11[i] = NA
    } else if (final_11_0$hormones_data11[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_11_0$hormones_core11[i] = 2
    } else if (final_11_0$hormones_data11[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_11_0$hormones_core11[i] = 0
    }
  } else if (final_11_0$overall_menopause_exam[i] <= 11) {
    #no alteration
    final_11_0$hormones_core11[i] = final_11_0$hormones_data11[i]
  }
}

#replace NA
final_11_0 <- final_11_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 12
#FE99
select_12_0 <- exam12[,c("ID", "FE99")]

#Placeholder values, used for testing
select_12_0$hormones_data12 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_12_0)) {
  if (is.na(select_12_0$FE99[i])) {
    #Unknown = NA
    select_12_0$hormones_data12[i] = NA
  } else if (select_12_0$FE99[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #FE99 in (9997,9999,88,2000,7,8,9,992) then NA
    select_12_0$hormones_data12[i] = NA
  } else if (select_12_0$FE99[i] == 0) {
    #FE99 = 0 then 0
    select_12_0$hormones_data12[i] = 0
  } else if (select_12_0$FE99[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_12_0$hormones_data12[i] = 1
  }
}

#preparation for final hormones_core variable
final_12_0 <- left_join(select_12_0, num_df, by = "ID")

#Placeholder values, used for testing
final_12_0$hormones_core12 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_12_0)) {
  if (is.na(final_12_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_12_0$hormones_data12[i])) {
      #correct NA
      final_12_0$hormones_core12[i] = NA
    } else if (final_12_0$hormones_data12[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_12_0$hormones_core12[i] = 1000
    } else {
      #Male should be NA no matter what
      final_12_0$hormones_core12[i] = NA
    }
  } else if (final_12_0$overall_menopause_exam[i] > 12) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_12_0$hormones_data12[i])) {
      #NA = NA
      final_12_0$hormones_core12[i] = NA
    } else if (final_12_0$hormones_data12[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_12_0$hormones_core12[i] = 2
    } else if (final_12_0$hormones_data12[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_12_0$hormones_core12[i] = 0
    }
  } else if (final_12_0$overall_menopause_exam[i] <= 12) {
    #no alteration
    final_12_0$hormones_core12[i] = final_12_0$hormones_data12[i]
  }
}

#replace NA
final_12_0 <- final_12_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 13
#FF101
select_13_0 <- exam13[,c("ID", "FF101")]

#Placeholder values, used for testing
select_13_0$hormones_data13 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_13_0)) {
  if (is.na(select_13_0$FF101[i])) {
    #Unknown = NA
    select_13_0$hormones_data13[i] = NA
  } else if (select_13_0$FF101[i] %in% c(9997,9999,88,2000,7,8,9,992)) {
    #FF101 in (9997,9999,88,2000,7,8,9,992) then NA
    select_13_0$hormones_data13[i] = NA
  } else if (select_13_0$FF101[i] == 0) {
    #FF101 = 0 then 0
    select_13_0$hormones_data13[i] = 0
  } else if (select_13_0$FF101[i] %in% c(1,2,3)) {
    #else if extracted hormones datapoints in (1,2,3) then 1
    select_13_0$hormones_data13[i] = 1
  }
}

#preparation for final hormones_core variable
final_13_0 <- left_join(select_13_0, num_df, by = "ID")

#Placeholder values, used for testing
final_13_0$hormones_core13 = 9999

#for loop to determine final hormones_core variable
for (i in 1:nrow(final_13_0)) {
  if (is.na(final_13_0$overall_menopause_exam[i])) {
    #Only males should be NA - inconsistencies checked here
    if (is.na(final_13_0$hormones_data13[i])) {
      #correct NA
      final_13_0$hormones_core13[i] = NA
    } else if (final_13_0$hormones_data13[i] == 1) {
      #If there's male with menopause, this is an inconsistency
      final_13_0$hormones_core13[i] = 1000
    } else {
      #Male should be NA no matter what
      final_13_0$hormones_core13[i] = NA
    }
  } else if (final_13_0$overall_menopause_exam[i] > 13) {
    #second loop regarding individuals recorded as not yet in menopause
    if (is.na(final_13_0$hormones_data13[i])) {
      #NA = NA
      final_13_0$hormones_core13[i] = NA
    } else if (final_13_0$hormones_data13[i] == 1) {
      #if hormone = 1 then hormone_core() = 2 
      final_13_0$hormones_core13[i] = 2
    } else if (final_13_0$hormones_data13[i] == 0) {
      #hormones = 0 then hormones_core() = 0
      final_13_0$hormones_core13[i] = 0
    }
  } else if (final_13_0$overall_menopause_exam[i] <= 13) {
    #no alteration
    final_13_0$hormones_core13[i] = final_13_0$hormones_data13[i]
  }
}

#replace NA
final_13_0 <- final_13_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Hormones Logic Section 14-end ####



#Exam 14
#FG100
select_14_0 <- exam14[,c("ID", "FG100")]

#Placeholder values, used for testing
select_14_0$hormones_core14 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_14_0)) {
  if (is.na(select_14_0$FG100[i])) {
    #Unknown = NA
    select_14_0$hormones_core14[i] = NA
  } else if (select_14_0$FG100[i] == 88) {
    #FG100 = 88 then NA
    select_14_0$hormones_core14[i] = NA
  } else if (select_14_0$FG100[i] %in% c(1,2)) {
    #FG100 in (1,2) then 1
    select_14_0$hormones_core14[i] = 1
  } else if (select_14_0$FG100[i] %in% c(0,3)) {
    #FG100 in (0,3) then 0
    select_14_0$hormones_core14[i] = 0
  }
}

#replace NA
select_14_0 <- select_14_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 15
#FH99
select_15_0 <- exam15[,c("ID", "FH99")]

#Placeholder values, used for testing
select_15_0$hormones_core15 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_15_0)) {
  if (is.na(select_15_0$FH99[i])) {
    #Unknown = NA
    select_15_0$hormones_core15[i] = NA
  } else if (select_15_0$FH99[i] == 88) {
    #FH99 = 88 then NA
    select_15_0$hormones_core15[i] = NA
  } else if (select_15_0$FH99[i] %in% c(1,2)) {
    #FH99 in (1,2) then 1
    select_15_0$hormones_core15[i] = 1
  } else if (select_15_0$FH99[i] %in% c(0,3)) {
    #FH99 in (0,3) then 0
    select_15_0$hormones_core15[i] = 0
  }
}

#replace NA
select_15_0 <- select_15_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 16
#FI58
select_16_0 <- exam16[,c("ID", "FI58")]

#Placeholder values, used for testing
select_16_0$hormones_core16 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_16_0)) {
  if (is.na(select_16_0$FI58[i])) {
    #Unknown = NA
    select_16_0$hormones_core16[i] = NA
  } else if (select_16_0$FI58[i] == 88) {
    #FI58 = 88 then NA
    select_16_0$hormones_core16[i] = NA
  } else if (select_16_0$FI58[i] %in% c(4,5)) {
    #FI58 in (4,5) then 1
    select_16_0$hormones_core16[i] = 1
  } else if (select_16_0$FI58[i] == 3) {
    #FI58 = 3 then 0
    select_16_0$hormones_core16[i] = 0
  }
}

#replace NA
select_16_0 <- select_16_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 17
#FJ28
select_17_0 <- exam17[,c("ID", "FJ28")]

#Placeholder values, used for testing
select_17_0$hormones_core17 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_17_0)) {
  if (is.na(select_17_0$FJ28[i])) {
    #Unknown = NA
    select_17_0$hormones_core17[i] = NA
  } else if (select_17_0$FJ28[i] == 88) {
    #FJ28 = 88 then NA
    select_17_0$hormones_core17[i] = NA
  } else if (select_17_0$FJ28[i] %in% c(1,2)) {
    #FJ28 in (1,2) then 1
    select_17_0$hormones_core17[i] = 1
  } else if (select_17_0$FJ28[i] %in% c(0,3)) {
    #FJ28 in (0,3) then 0
    select_17_0$hormones_core17[i] = 0
  }
}

#replace NA
select_17_0 <- select_17_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 18
#FK112
select_18_0 <- exam18[,c("ID", "FK112")]

#Placeholder values, used for testing
select_18_0$hormones_core18 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_18_0)) {
  if (is.na(select_18_0$FK112[i])) {
    #Unknown = NA
    select_18_0$hormones_core18[i] = NA
  } else if (select_18_0$FK112[i] == 88) {
    #FK112 = 88 then NA
    select_18_0$hormones_core18[i] = NA
  } else if (select_18_0$FK112[i] %in% c(1,2)) {
    #FK112 in (1,2) then 1
    select_18_0$hormones_core18[i] = 1
  } else if (select_18_0$FK112[i] %in% c(0,3)) {
    #FK112 in (0,3) then 0
    select_18_0$hormones_core18[i] = 0
  }
}

#replace NA
select_18_0 <- select_18_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 19
#FL147, FL174, FL177
select_19_0 <- exam19[,c("ID", "FL147", "FL174", "FL177")]

#Placeholder values, used for testing
select_19_0$hormones_core19 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_19_0 <- select_19_0 %>% mutate_at(c(2:4), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_19_0)) {
  if (select_19_0$FL147[i] %in% c(1,2) | select_19_0$FL174[i] %in% c(1,2) | select_19_0$FL177[i] %in% c(1,2)) {
    #FL147 in (1,2) or FL174 in (1,2) or FL177 in (1,2) then 1
    select_19_0$hormones_core19[i] = 1
  } else if (select_19_0$FL147[i] == 0 & select_19_0$FL174[i] == 0 & select_19_0$FL177[i] == 0 ) {
    #FL147 = 0 and FL74  = 0 and FL177 = 0 then 0
    select_19_0$hormones_core19[i] = 0
  } else {
    #Else NA
    select_19_0$hormones_core19[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_19_0[c(2:4)] <- replace(select_19_0[c(2:4)], select_19_0[c(2:4)] == 10, NA)
#replace NA
select_19_0 <- select_19_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 20
#FM185, FM202, FM205, FM206
select_20_0 <- exam20[,c("ID", "FM185", "FM202", "FM205", "FM206")]

#Placeholder values, used for testing
select_20_0$hormones_core20 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_20_0 <- select_20_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_20_0)) {
  if (select_20_0$FM185[i] %in% c(1,2) | select_20_0$FM202[i] %in% c(1,2) 
      | select_20_0$FM205[i] %in% c(1,2) | select_20_0$FM206[i] %in% c(1,2)) {
    #FM185 in (1,2) or FM202 in (1,2) or FM205 in (1,2) or FM206 in (1,2) then 1
    select_20_0$hormones_core20[i] = 1
  } else if (select_20_0$FM185[i] %in% c(0,3) & select_20_0$FM202[i] == 0 
             & select_20_0$FM205[i] == 0 & select_20_0$FM206[i] == 0) {
    #FM185 in (0,3) and FM202 = 0 and FM205 = 0 and FM206 = 0 then 0
    select_20_0$hormones_core20[i] = 0
  } else if (select_20_0$FM185[i] %in% c(0,10,88) & select_20_0$FM202[i] %in% c(10,8,88) 
             & select_20_0$FM205[i] %in% c(10,8,88) & select_20_0$FM206[i] %in% c(10,8,88)) {
    #FM185 in (.,88) and FM202 in (.,8,88) and FM205 in (.,8,88) and FM206 in (.,8,88) then hormones_core26 = NA
    select_20_0$hormones_core20[i] = NA
  } else {
    #Else NA
    select_20_0$hormones_core20[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_20_0[c(2:5)] <- replace(select_20_0[c(2:5)], select_20_0[c(2:5)] == 10, NA)
#replace NA
select_20_0 <- select_20_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 21
#FN124, FN152, FN155, FN156
select_21_0 <- exam21[,c("ID", "FN124", "FN152", "FN155", "FN156")]

#Placeholder values, used for testing
select_21_0$hormones_core21 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_21_0 <- select_21_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_21_0)) {
  if (select_21_0$FN124[i] %in% c(1,2) | select_21_0$FN152[i] %in% c(1,2) 
      | select_21_0$FN155[i] %in% c(1,2) | select_21_0$FN156[i] %in% c(1,2)) {
    #FN124 in (1,2) or FN152 in (1,2) or FN155 in (1,2) or FN156 in (1,2) then 1
    select_21_0$hormones_core21[i] = 1
  } else if (select_21_0$FN124[i] %in% c(0,3) & select_21_0$FN152[i] == 0 
             & select_21_0$FN155[i] == 0 & select_21_0$FN156[i] == 0) {
    #FN124 in (0,3) and FN152 = 0 and FN155 = 0 and FN156 = 0 then 0
    select_21_0$hormones_core21[i] = 0
  } else {
    #Else NA
    select_21_0$hormones_core21[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_21_0[c(2:5)] <- replace(select_21_0[c(2:5)], select_21_0[c(2:5)] == 10, NA)
#replace NA
select_21_0 <- select_21_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 22
#FO131, FO151, FO155, FO156
select_22_0 <- exam22[,c("ID", "FO131", "FO151", "FO155", "FO156")]

#Placeholder values, used for testing
select_22_0$hormones_core22 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_22_0 <- select_22_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_22_0)) {
  if (select_22_0$FO131[i] %in% c(1,2) | select_22_0$FO151[i] %in% c(1,2) 
      | select_22_0$FO155[i] %in% c(1,2) | select_22_0$FO156[i] %in% c(1,2)) {
    #FO131 in (1,2) or FO151 in (1,2) or FO155 in (1,2) or FO156 in (1,2) then 1
    select_22_0$hormones_core22[i] = 1
  } else if (select_22_0$FO131[i] %in% c(0,3) & select_22_0$FO151[i] == 0 
             & select_22_0$FO155[i] == 0 & select_22_0$FO156[i] == 0) {
    #FO131 in (0,3) and FO151 = 0 and FO155 = 0 and FO156 = 0 then 0
    select_22_0$hormones_core22[i] = 0
  } else if (select_22_0$FO131[i] %in% c(0,10,88) & select_22_0$FO151[i] %in% c(10,8,88) 
             & select_22_0$FO155[i] %in% c(10,8,88) & select_22_0$FO156[i] %in% c(10,8,88)) {
    #FO131 in (.,88) and FO151 in (.,8,88) and FO155 in (.,8,88) and FO156 in (.,8,88) then hormones_core26 = NA
    select_22_0$hormones_core22[i] = NA
  } else {
    #Else NA
    select_22_0$hormones_core22[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_22_0[c(2:5)] <- replace(select_22_0[c(2:5)], select_22_0[c(2:5)] == 10, NA)
#replace NA
select_22_0 <- select_22_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 23
#FP085, FP121, FP122, FP126, FP127
select_23_0 <- exam23[,c("ID", "FP085", "FP121", "FP122", "FP126", "FP127")]

#Placeholder values, used for testing
select_23_0$hormones_core23 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_23_0 <- select_23_0 %>% mutate_at(c(2:6), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_23_0)) {
  if (select_23_0$FP085[i] %in% c(1,2) | select_23_0$FP121[i] == 1 
      | select_23_0$FP122[i] %in% c(1,2) | select_23_0$FP126[i] %in% c(1,2)
      | select_23_0$FP127[i] %in% c(1,2)) {
    #FP085 in (1,2) or FP121 = 1 or FP122 in (1,2) or FP126 in (1,2) or FP127 in (1,2) then 1
    select_23_0$hormones_core23[i] = 1
  } else if (select_23_0$FP085[i] %in% c(0,3) & select_23_0$FP121[i] == 0 
             & select_23_0$FP122[i] == 0 & select_23_0$FP126[i] == 0
             & select_23_0$FP127[i] == 0) {
    #FP085 in (0,3) and FP121 = 0 and FP122 = 0 and FP126 = 0 and FP127 = 0 then 0
    select_23_0$hormones_core23[i] = 0
  } else if (select_23_0$FP085[i] %in% c(0,10,88) & select_23_0$FP121[i] %in% c(10,8,88) 
             & select_23_0$FP122[i] %in% c(10,8,88) & select_23_0$FP126[i] %in% c(10,8,88)
             & select_23_0$FP127[i] %in% c(10,8,88)) {
    #FP085 in (.,88) and FP121 in (.,8,88) and FP122 in (.,8,88) and FP126 in (.,8,88) and FP127 in (.,8,88) then hormones_core26 = NA
    select_23_0$hormones_core23[i] = NA
  } else {
    #Else NA
    select_23_0$hormones_core23[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_23_0[c(2:6)] <- replace(select_23_0[c(2:6)], select_23_0[c(2:6)] == 10, NA)
#replace NA
select_23_0 <- select_23_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 24
#FQ170, FQ186, FQ190
select_24_0 <- exam24[,c("ID", "FQ170", "FQ186", "FQ190")]

#Placeholder values, used for testing
select_24_0$hormones_core24 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_24_0 <- select_24_0 %>% mutate_at(c(2:4), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_24_0)) {
  if (select_24_0$FQ170[i] %in% c(1,2) | select_24_0$FQ186[i] %in% c(1,2) 
      | select_24_0$FQ190[i] %in% c(1,2)) {
    #FQ170 in (1,2) or FQ186 in (1,2) or FQ190 in (1,2) then 1
    select_24_0$hormones_core24[i] = 1
  } else if (select_24_0$FQ170[i] %in% c(0,3) & select_24_0$FQ186[i] == 0 
             & select_24_0$FQ190[i] == 0) {
    #FQ170 in (0,3) and FQ186 = 0 and FQ190 = 0 then 0
    select_24_0$hormones_core24[i] = 0
  } else if (select_24_0$FQ170[i] %in% c(0,10,88) & select_24_0$FQ186[i] %in% c(10,8,88) 
             & select_24_0$FQ190[i] %in% c(10,8,88)) {
    #FQ170 in (.,88) and FQ186 in (.,8,88) and FQ190 in (.,8,88) then hormones_core26 = NA
    select_24_0$hormones_core24[i] = NA
  } else {
    #Else NA
    select_24_0$hormones_core24[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_24_0[c(2:4)] <- replace(select_24_0[c(2:4)], select_24_0[c(2:4)] == 10, NA)
#replace NA
select_24_0 <- select_24_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 25
#FR222, FR241, FR245, FR246
select_25_0 <- exam25[,c("ID", "FR222", "FR241", "FR245", "FR246")]

#Placeholder values, used for testing
select_25_0$hormones_core25 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_25_0 <- select_25_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_25_0)) {
  if (select_25_0$FR222[i] %in% c(1,2) | select_25_0$FR241[i] %in% c(1,2) 
      | select_25_0$FR245[i] %in% c(1,2) | select_25_0$FR246[i] %in% c(1,2)) {
    #FR222 in (1,2) or FR241 in (1,2) or FR245 in (1,2) or FR246 in (1,2) then 1
    select_25_0$hormones_core25[i] = 1
  } else if (select_25_0$FR222[i] %in% c(0,3) & select_25_0$FR241[i] %in% c(0,3) 
             & select_25_0$FR245[i] %in% c(0,3) & select_25_0$FR246[i] %in% c(0,3)) {
    #FR222 in (0,3) and FR241 = 0 and FR245 = 0 and FR246 = 0 then 0
    select_25_0$hormones_core25[i] = 0
  } else if (select_25_0$FR222[i] %in% c(0,10,88) & select_25_0$FR241[i] %in% c(10,8,88) 
             & select_25_0$FR245[i] %in% c(10,8,88) & select_25_0$FR246[i] %in% c(10,8,88)) {
    #FR222 in (.,88) and FR241 in (.,8,88) and FR245 in (.,8,88) and FR246 in (.,8,88) then hormones_core26 = NA
    select_25_0$hormones_core25[i] = NA
  } else {
    #Else NA
    select_25_0$hormones_core25[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_25_0[c(2:5)] <- replace(select_25_0[c(2:5)], select_25_0[c(2:5)] == 10, NA)
#replace NA
select_25_0 <- select_25_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 26
#FS290, FS314, FS318, FS319
select_26_0 <- exam26[,c("ID", "FS290", "FS314", "FS318", "FS319")]

#Placeholder values, used for testing
select_26_0$hormones_core26 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_26_0 <- select_26_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_26_0)) {
  if (select_26_0$FS290[i] %in% c(1,2) | select_26_0$FS314[i] %in% c(1,2) 
      | select_26_0$FS318[i] %in% c(1,2) | select_26_0$FS319[i] %in% c(1,2)) {
    #FS290 in (1,2) or FS314 in (1,2) or FS318 in (1,2) or FS319 in (1,2) then 1
    select_26_0$hormones_core26[i] = 1
  } else if (select_26_0$FS290[i] %in% c(0,3) & select_26_0$FS314[i] == 0 
             & select_26_0$FS318[i] == 0 & select_26_0$FS319[i] == 0) {
    #FS290 in (0,3) and FS314 = 0 and FS318 = 0 and FS319 = 0 then 0
    select_26_0$hormones_core26[i] = 0
  } else if (select_26_0$FS290[i] %in% c(0,10,88) & select_26_0$FS314[i] %in% c(10,8,88) 
             & select_26_0$FS318[i] %in% c(10,8,88) & select_26_0$FS319[i] %in% c(10,8,88)) {
    #FS290 in (.,88) and FS314 in (.,8,88) and FS318 in (.,8,88) and FS319 in (.,8,88) then hormones_core26 = NA
    select_26_0$hormones_core26[i] = NA
  } else {
    #Else NA
    select_26_0$hormones_core26[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_26_0[c(2:5)] <- replace(select_26_0[c(2:5)], select_26_0[c(2:5)] == 10, NA)
#replace NA
select_26_0 <- select_26_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 27
#FT297, FT323, FT327, FT328
select_27_0 <- exam27[,c("ID", "FT297", "FT323", "FT327", "FT328")]

#Placeholder values, used for testing
select_27_0$hormones_core27 = 9999

#Replacing NA with 10 avoids R treating those values differently
select_27_0 <- select_27_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#for loop to determine derived variable
for (i in 1:nrow(select_27_0)) {
  if (select_27_0$FT297[i] %in% c(1,2) | select_27_0$FT323[i] %in% c(1,2) 
      | select_27_0$FT327[i] %in% c(1,2) | select_27_0$FT328[i] %in% c(1,2)) {
    #FT297 in (1,2) or FT323 in (1,2) or FT327 in (1,2) or FT328 in (1,2) then 1
    select_27_0$hormones_core27[i] = 1
  } else if (select_27_0$FT297[i] %in% c(0,3) & select_27_0$FT323[i] == 0 
             & select_27_0$FT327[i] == 0 & select_27_0$FT328[i] == 0) {
    #FT297 in (0,3) and FT323 = 0 and FT327 = 0 and FT328 = 0 then 0
    select_27_0$hormones_core27[i] = 0
  } else if (select_27_0$FT297[i] %in% c(0,10,88) & select_27_0$FT323[i] %in% c(10,8,88) 
             & select_27_0$FT327[i] %in% c(10,8,88) & select_27_0$FT328[i] %in% c(10,8,88)) {
    #FT297 in (.,88) and FT323 in (.,8,88) and FT327 in (.,8,88) and FT328 in (.,8,88) then hormones_core27 = NA
    select_27_0$hormones_core27[i] = NA
  } else {
    #Else NA
    select_27_0$hormones_core27[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_27_0[c(2:5)] <- replace(select_27_0[c(2:5)], select_27_0[c(2:5)] == 10, NA)
#replace NA
select_27_0 <- select_27_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exam 28
#FU023, FU027, FU028
first_28_0 <- exam28[,c("ID", "FU023", "FU027", "FU028")]

med_found28 <- read.csv("hormones_atc_28.csv")

select_28_0 <- full_join(first_28_0, med_found28, by = "ID")

#Replacing NA with 10 avoids R treating those values differently
select_28_0 <- select_28_0 %>% mutate_at(c(2:5), ~replace_na(.,10))

#Placeholder values, used for testing
select_28_0$hormones_core28 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_28_0)) {
  if (select_28_0$FU023[i] %in% c(1,2) | select_28_0$FU027[i] %in% c(1,2) 
      | select_28_0$FU028[i] %in% c(1,2)) {
    #FU023 in (1,2) or FU027 in (1,2) or FU028 in (1,2) then 1
    select_28_0$hormones_core28[i] = 1
  } else if (select_28_0$atc_hormones_28[i] == 1) {
    #if ATC data (exam 28) = 1 then hormones_core28 = 1
    select_28_0$hormones_core28[i] = 1
  } else if (select_28_0$FU023[i] == 0 & select_28_0$FU027[i] == 0 
             & select_28_0$FU028[i] == 0) {
    #FU023 in (0,3) and FU027 = 0 and FU028 = 0 then 0
    select_28_0$hormones_core28[i] = 0
  } else if (select_28_0$FU023[i] %in% c(0,10,8,88) & select_28_0$FU027[i] %in% c(10,8,88) 
             & select_28_0$FU028[i] %in% c(10,8,88)) {
    #FU023 in (.,88) and FU027 in (.,8,88) and FU028 in (.,8,88) then hormones_core26 = NA
    select_28_0$hormones_core28[i] = NA
  } else {
    #Else NA
    select_28_0$hormones_core28[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
select_28_0[c(2:5)] <- replace(select_28_0[c(2:5)], select_28_0[c(2:5)] == 10, NA)
#replace NA
select_28_0 <- select_28_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#Exams 29-32
#since the new variable in 29-32 is the same as the old one, just rename the column

select_29_0 <- med_found29
select_30_0 <- med_found30
select_31_0 <- med_found31
select_32_0 <- med_found32
colnames(select_29_0) <- c("ID", "hormones_core29")
colnames(select_30_0) <- c("ID", "hormones_core30")
colnames(select_31_0) <- c("ID", "hormones_core31")
colnames(select_32_0) <- c("ID", "hormones_core32")

select_29_0 <- select_29_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
select_30_0 <- select_30_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
select_31_0 <- select_31_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
select_32_0 <- select_32_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Hormones Logic Merge ####
#merge datasets
horm_list <- list(final_02_0, final_05_0, final_07_0, final_08_0, final_09_0, final_10_0, 
                  final_11_0, final_12_0, final_13_0, select_14_0, select_15_0, select_16_0,
                  select_17_0, select_18_0, select_19_0, select_20_0, select_21_0, select_22_0,
                  select_23_0, select_24_0, select_25_0, select_26_0, select_27_0, select_28_0, 
                  select_29_0, select_30_0, select_31_0, select_32_0)
horm_joined <- horm_list %>% reduce(full_join, by = "ID")

#subset for only ID and menopause columns
horm_only <- subset(horm_joined, select = c(ID, hormones_core2, hormones_core5, hormones_core7, hormones_core8, 
                                            hormones_core9, hormones_core10, hormones_core11, hormones_core12, 
                                            hormones_core13, hormones_core14, hormones_core15, hormones_core16,
                                            hormones_core17, hormones_core18, hormones_core19, hormones_core20,
                                            hormones_core21, hormones_core22, hormones_core23, hormones_core24,
                                            hormones_core25, hormones_core26, hormones_core27, hormones_core28,
                                            hormones_core29, hormones_core30, hormones_core31, hormones_core32))

#write hormones data to file
#write.csv(horm_only, file = "Hormones_Derived_Variables_Only.csv", row.names = FALSE)

#create subset spreadsheet of just the columns we want
short_subset <- new_joined[c(1, 23:45)]
short_subset$sex <- num_df$sex

#merge with above subset spreadsheet
complete_merge <- full_join(short_subset, horm_only, by = "ID")

complete_merge <- complete_merge %>% rename("id" = "ID")
complete_merge$framid <- complete_merge$id
complete_merge$idtype <- 0

#changing position of column
complete_final <- complete_merge %>% relocate(idtype, .before = id)
complete_final <- complete_final %>% relocate(framid, .after = id)
complete_final <- complete_final %>% relocate(sex, .after = framid)

#write to file
write.csv(complete_final, file = "Hormones_Menopause_Gen1.csv", row.names = FALSE)

