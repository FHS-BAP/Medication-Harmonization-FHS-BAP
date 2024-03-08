# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 aspirin derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: February 2023
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 2/Omni 1 aspirin usage.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
# e_exam_ex01_1_0079_v1.sas7bdat (Gen 2 Exam 1)
# e_exam_ex02_1_0080_v1.sas7bdat (Gen 2 Exam 2)
# e_exam_ex03_1_0081.sas7bdat (Gen 2 Exam 3)
# e_exam_ex04_1_0082.sas7bdat (Gen 2 Exam 4)
# e_exam_ex05_1_0083.sas7bdat (Gen 2 Exam 5)
# e_exam_ex06_1_0084.sas7bdat (Gen 2 Exam 6)
# e_exam_ex07_1_0085_v1.sas7bdat (Gen 2 Exam 7)
# e_exam_ex08_1_0005.sas7bdat (Gen 2 Exam 8)
# e_exam_ex01_7_0020.sas7bdat (Omni 1 Exam 1)
# e_exam_ex02_7_0003.sas7bdat (Omni 1 Exam 2)
# e_exam_ex03_7_0426.sas7bdat (Omni 1 Exam 3)
# e_exam_ex09_1b_0844.sas7bdat (Gen 2 Exam 9 and Omni 1 Exam 9)
# e_exam_ex10_1b_1409.sas7bdat (Gen 2 Exam 10 and Omni 1 Exam 5)
# 
# 
# Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality

#### Gen 2 Exam 2 ####
#B69
exam_02_1 <- read_sas("e_exam_ex02_1_0080_v1.sas7bdat")
select_02_1 <- exam_02_1[,c("ID", "B69")]
#add IDTYPE column
select_02_1 <- select_02_1 %>% add_column(IDTYPE = "1", .after = 1)

#Placeholder values, used for testing
select_02_1$aspirin_usage_core2 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_02_1)) {
  if (is.na(select_02_1$B69[i])) {
    #Unknown = NA
    select_02_1$aspirin_usage_core2[i] = NA
  } else if (select_02_1$B69[i] >= 1) {
    #If it’s a positive number it’s a yes
    select_02_1$aspirin_usage_core2[i] = 1
  } else {
    #If the aspirin value is 0 it’s no
    select_02_1$aspirin_usage_core2[i] = 0
  }
}

#assign framid
select_02_1$framid <- (80000 + select_02_1$ID)
#remove unneeded column
select_02_1 <- select_02_1 %>% select(-B69)
#rename columns
colnames(select_02_1) <- c("id", "idtype", "aspirin_use_core2", "framid")
#replace NA
select_02_1 <- select_02_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 3 ####
#C7
exam_03_1 <- read_sas("e_exam_ex03_1_0081.sas7bdat")
select_03_1 <- exam_03_1[,c("ID", "C7")]
#add IDTYPE column
select_03_1 <- select_03_1 %>% add_column(IDTYPE = "1", .after = 1)

#Placeholder values, used for testing
select_03_1$aspirin_usage_core3 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_03_1)) {
  if (is.na(select_03_1$C7[i])) {
    #Unknown = NA
    select_03_1$aspirin_usage_core3[i] = NA
  } else if (select_03_1$C7[i] >= 1) {
    #If it’s a positive number it’s a yes
    select_03_1$aspirin_usage_core3[i] = 1
  } else {
    #If the aspirin value is 0 it’s no
    select_03_1$aspirin_usage_core3[i] = 0
  }
}

#assign framid
select_03_1$framid <- (80000 + select_03_1$ID)
#remove unneeded column
select_03_1 <- select_03_1 %>% select(-C7)
#rename columns
colnames(select_03_1) <- c("id", "idtype", "aspirin_use_core3", "framid")
#replace NA
select_03_1 <- select_03_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 4 ####
#D008
exam_04_1 <- read_sas("e_exam_ex04_1_0082.sas7bdat")
select_04_1 <- exam_04_1[,c("ID", "D008")]
#add IDTYPE column
select_04_1 <- select_04_1 %>% add_column(IDTYPE = "1", .after = 1)

#Placeholder values, used for testing
select_04_1$aspirin_usage_core4 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_04_1)) {
  if (is.na(select_04_1$D008[i])) {
    #Unknown = NA
    select_04_1$aspirin_usage_core4[i] = NA
  } else if (select_04_1$D008[i] >= 1) {
    #If it’s a positive number it’s a yes
    select_04_1$aspirin_usage_core4[i] = 1
  } else {
    #If the aspirin value is 0 it’s no
    select_04_1$aspirin_usage_core4[i] = 0
  }
}

#assign framid
select_04_1$framid <- (80000 + select_04_1$ID)
#remove unneeded column
select_04_1 <- select_04_1 %>% select(-D008)
#rename columns
colnames(select_04_1) <- c("id", "idtype", "aspirin_use_core4", "framid")
#replace NA
select_04_1 <- select_04_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 5 ####
#E041, E218, E219
exam_05_1 <- read_sas("e_exam_ex05_1_0083.sas7bdat")
select_05_1 <- exam_05_1[,c("ID", "IDTYPE", "E041", "E218", "E219")]

#Placeholder values, used for testing
select_05_1$aspirin_usage_core5 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_05_1)) {
  if (is.na(select_05_1$E041[i]) | is.na(select_05_1$E218[i]) | is.na(select_05_1$E219[i])) {
    #If NA in any column then aspirin_usage_core5 = NA (This has to go first for logic)
    select_05_1$aspirin_usage_core5[i] = NA
  } else if (select_05_1$E041[i] >= 1 & select_05_1$E041[i] <= 7 
             & select_05_1$E219[i] >= 1 & select_05_1$E219[i] <= 2) {
    #If E219 is 1 or 2 and E041 is 1-7 then new variable is 1 
    select_05_1$aspirin_usage_core5[i] = 1
  } else if (select_05_1$E041[i] == 0 | select_05_1$E218[i] == 0 | select_05_1$E219[i] == 0) {
    #If E041 is 0 or E218 is 0 or E219 is 0 then new variable is 0 
    select_05_1$aspirin_usage_core5[i] = 0
  } else {
    #Else NA
    select_05_1$aspirin_usage_core5[i] = NA
  }
}

#assign framid
select_05_1$framid <- (80000 + select_05_1$ID)
#remove unneeded columns
select_05_1 <- subset(select_05_1, select = -c(E041, E218, E219))
#rename columns
colnames(select_05_1) <- c("id", "idtype", "aspirin_use_core5", "framid")
#replace NA
select_05_1 <- select_05_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 6 ####
#F180-F183
exam_06_1 <- read_sas("e_exam_ex06_1_0084.sas7bdat")
select_06_1 <- exam_06_1[,c("ID", "IDTYPE", "F180", "F181", "F182", "F183")]

#Placeholder values, used for testing
select_06_1$aspirin_usage_core6 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_06_1)) {
  if (is.na(select_06_1$F181[i]) | is.na(select_06_1$F182[i]) | is.na(select_06_1$F183[i])) {
    #If NA in any column then aspirin_usage_core6 = NA (This has to go first for logic)
    select_06_1$aspirin_usage_core6[i] = NA
  } else if (select_06_1$F181[i] == 1 & select_06_1$F182[i] == 1 
             & select_06_1$F183[i] >= 81 & select_06_1$F183[i] <= 325) {
    #If F181 = 1 and F182 = 1 and F183 = 81-325 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] == 2 & select_06_1$F182[i] == 1 
             & select_06_1$F183[i] >= 81 & select_06_1$F183[i] <= 160) {
    #If F181 = 2 and F182 = 1 and F183 = 81-160 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] >= 3 & select_06_1$F181[i] <= 4 
             & select_06_1$F182[i] == 1 & select_06_1$F183[i] == 81 
             & select_06_1$F183[i] <= 163) {
    #If F181 = 3-4 and F182 = 1 and F183 = 81 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] >= 7 & select_06_1$F181[i] <= 14
             & select_06_1$F182[i] == 2 & select_06_1$F183[i] >= 81 
             & select_06_1$F183[i] <= 160) {
    #If F181 = 7-14 and F182 = 2 and F183 = 81-160 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] >= 3 & select_06_1$F181[i] <= 6
             & select_06_1$F182[i] == 2 & select_06_1$F183[i] >= 81 
             & select_06_1$F183[i] <= 325) {
    #If F181 = 3-6 and F182 = 2 and F183 = 81-325 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] >= 15 & select_06_1$F181[i] <= 20 
             & select_06_1$F182[i] == 2 & select_06_1$F183[i] >= 81) {
    #If F181 = 15-20 and F182 = 2 and F183 = 81 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] >= 7 & select_06_1$F181[i] <= 10 
             & select_06_1$F182[i] == 2 & select_06_1$F183[i] == 325) {
    #If F181 = 7-10 and F182 = 2 and F183 = 325 then aspirin_usage_core6 = 1
    select_06_1$aspirin_usage_core6[i] = 1
  } else if (select_06_1$F181[i] >= 10 & select_06_1$F181[i] <= 20 
             & select_06_1$F182[i] == 1 & select_06_1$F183[i] == 325) {
    #If F181 = 10-20 and F182 = 1 and F183 = 325 then aspirin_usage_core6 = 3
    select_06_1$aspirin_usage_core6[i] = 3
  } else if (select_06_1$F181[i] >= 6 & select_06_1$F181[i] <= 20 
             & select_06_1$F182[i] == 1 & select_06_1$F183[i] == 500) {
    #If F181 = 6-20 and F181 = 1 and F183 = 500 then aspirin_usage_core6 = 3
    select_06_1$aspirin_usage_core6[i] = 3
  } else if (select_06_1$F181[i] == 0 | select_06_1$F182[i] == 0
             | select_06_1$F182[i] == 3 | select_06_1$F182[i] == 4
             | select_06_1$F183[i] == 0) {
    #If F181 = (0, NA) or F182 = (0, 3, 4, NA) or F183 = (0, NA) then aspirin_usage_core6 = NA
    select_06_1$aspirin_usage_core6[i] = NA
  } else {
    #Else aspirin_usage_core6 = 2
    select_06_1$aspirin_usage_core6[i] = 2
  }
}

#assign framid
select_06_1$framid <- (80000 + select_06_1$ID)
#rename columns
colnames(select_06_1) <- c("id", "idtype", "aspirin_use_core6", 
                           "aspirin_quantity_core6", "aspirin_freq_core6",
                           "aspirin_dose_core6", "aspirin_purpose_core6", "framid")
#replace NA
select_06_1 <- select_06_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 7 ####
#G037-G040
exam_07_1 <- read_sas("e_exam_ex07_1_0085_v1.sas7bdat")
select_07_1 <- exam_07_1[,c("ID", "IDTYPE", "G037", "G038", "G039", "G040")]

#Placeholder values, used for testing
select_07_1$aspirin_usage_core7 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_07_1)) {
  if (is.na(select_07_1$G038[i]) | is.na(select_07_1$G039[i]) | is.na(select_07_1$G040[i])) {
    #If NA in any column then aspirin_usage_core7 = NA (This has to go first for logic)
    select_07_1$aspirin_usage_core7[i] = NA
  } else if (select_07_1$G038[i] == 1 & select_07_1$G039[i] == 1 
             & select_07_1$G040[i] >= 81 & select_07_1$G040[i] <= 325) {
    #If G038 = 1 and G039 = 1 and G040 = 81-325 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] == 2 & select_07_1$G039[i] == 1 
             & select_07_1$G040[i] >= 81 & select_07_1$G040[i] <= 160) {
    #If G038 = 2 and G039 = 1 and G040 = 81-160 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] >= 3 & select_07_1$G038[i] <= 4 
             & select_07_1$G039[i] == 1 & select_07_1$G040[i] == 81 
             & select_07_1$G040[i] <= 163) {
    #If G038 = 3-4 and G039 = 1 and G040 = 81 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] >= 7 & select_07_1$G038[i] <= 14
             & select_07_1$G039[i] == 2 & select_07_1$G040[i] >= 81 
             & select_07_1$G040[i] <= 160) {
    #If G038 = 7-14 and G039 = 2 and G040 = 81-160 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] >= 3 & select_07_1$G038[i] <= 6
             & select_07_1$G039[i] == 2 & select_07_1$G040[i] >= 81 
             & select_07_1$G040[i] <= 325) {
    #If G038 = 3-6 and G039 = 2 and G040 = 81-325 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] >= 15 & select_07_1$G038[i] <= 28 
             & select_07_1$G039[i] == 2 & select_07_1$G040[i] >= 81) {
    #If G038 = 15-28 and G039 = 2 and G040 = 81 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] >= 7 & select_07_1$G038[i] <= 10 
             & select_07_1$G039[i] == 2 & select_07_1$G040[i] == 325) {
    #If G038 = 7-10 and G039 = 2 and G040 = 325 then aspirin_usage_core7 = 1
    select_07_1$aspirin_usage_core7[i] = 1
  } else if (select_07_1$G038[i] >= 10 & select_07_1$G038[i] <= 91 
             & select_07_1$G039[i] == 1 & select_07_1$G040[i] == 325) {
    #If G038 = 10-91 and G039 = 1 and G040 = 325 then aspirin_usage_core7 = 3
    select_07_1$aspirin_usage_core7[i] = 3
  } else if (select_07_1$G038[i] >= 6 & select_07_1$G038[i] <= 91 
             & select_07_1$G039[i] == 1 & select_07_1$G040[i] == 500) {
    #If G038 = 6-91 and G038 = 1 and G040 = 500 then aspirin_usage_core7 = 3
    select_07_1$aspirin_usage_core7[i] = 3
  } else if (select_07_1$G038[i] == 0 | select_07_1$G039[i] == 0
             | select_07_1$G039[i] == 3 | select_07_1$G039[i] == 4
             | select_07_1$G040[i] == 0) {
    #If G038 = (0, NA) or G039 = (0, 3, 4, NA) or G040 = (0, NA) then aspirin_usage_core7 = NA
    select_07_1$aspirin_usage_core7[i] = NA
  } else {
    #Else aspirin_usage_core7 = 2
    select_07_1$aspirin_usage_core7[i] = 2
  }
}

#assign framid
select_07_1$framid <- (80000 + select_07_1$ID)
#rename columns
colnames(select_07_1) <- c("id", "idtype", "aspirin_use_core7", 
                           "aspirin_quantity_core7", "aspirin_freq_core7",
                           "aspirin_dose_core7", "aspirin_purpose_core7", "framid")
#replace NA
select_07_1 <- select_07_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 8 ####
#H010-H013
exam_08_1 <- read_sas("e_exam_ex08_1_0005.sas7bdat")
select_08_1 <- exam_08_1[,c("ID", "IDTYPE", "H010", "H011", "H012", "H013")]

#Placeholder values, used for testing
select_08_1$aspirin_usage_core8 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_08_1)) {
  if (is.na(select_08_1$H011[i]) | is.na(select_08_1$H012[i]) | is.na(select_08_1$H013[i])) {
    #If NA in any column then aspirin_usage_core8 = NA (This has to go first for logic)
    select_08_1$aspirin_usage_core8[i] = NA
  } else if (select_08_1$H011[i] == 1 & select_08_1$H012[i] == 1 
             & select_08_1$H013[i] >= 81 & select_08_1$H013[i] <= 325) {
    #If H011 = 1 and H012 = 1 and H013 = 81-325 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] == 2 & select_08_1$H012[i] == 1 
             & select_08_1$H013[i] >= 81 & select_08_1$H013[i] <= 160) {
    #If H011 = 2 and H012 = 1 and H013 = 81-160 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] >= 3 & select_08_1$H011[i] <= 4 
             & select_08_1$H012[i] == 1 & select_08_1$H013[i] == 81 
             & select_08_1$H013[i] <= 163) {
    #If H011 = 3-4 and H012 = 1 and H013 = 81 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] >= 7 & select_08_1$H011[i] <= 15
             & select_08_1$H012[i] == 2 & select_08_1$H013[i] >= 81 
             & select_08_1$H013[i] <= 160) {
    #If H011 = 7-15 and H012 = 2 and H013 = 81-160 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] >= 3 & select_08_1$H011[i] <= 6
             & select_08_1$H012[i] == 2 & select_08_1$H013[i] >= 81 
             & select_08_1$H013[i] <= 325) {
    #If H011 = 3-6 and H012 = 2 and H013 = 81-325 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] == 15 & select_08_1$H012[i] == 2 & select_08_1$H013[i] >= 81) {
    #If H011 = 15 and H012 = 2 and H013 = 81 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] >= 7 & select_08_1$H011[i] <= 10 
             & select_08_1$H012[i] == 2 & select_08_1$H013[i] == 325) {
    #If H011 = 7-10 and H012 = 2 and H013 = 325 then aspirin_usage_core8 = 1
    select_08_1$aspirin_usage_core8[i] = 1
  } else if (select_08_1$H011[i] >= 10 & select_08_1$H011[i] <= 15 
             & select_08_1$H012[i] == 1 & select_08_1$H013[i] == 325) {
    #If H011 = 10-15 and H012 = 1 and H013 = 325 then aspirin_usage_core8 = 3
    select_08_1$aspirin_usage_core8[i] = 3
  } else if (select_08_1$H011[i] >= 6 & select_08_1$H011[i] <= 15 
             & select_08_1$H012[i] == 1 & select_08_1$H013[i] == 500) {
    #If H011 = 6-15 and H011 = 1 and H013 = 500 then aspirin_usage_core8 = 3
    select_08_1$aspirin_usage_core8[i] = 3
  } else if (select_08_1$H011[i] == 0 | select_08_1$H012[i] == 0
             | select_08_1$H012[i] == 3 | select_08_1$H012[i] == 4
             | select_08_1$H013[i] == 0) {
    #If H011 = (0, NA) or H012 = (0, 3, 4, NA) or H013 = (0, NA) then aspirin_usage_core8 = NA
    select_08_1$aspirin_usage_core8[i] = NA
  } else {
    #Else aspirin_usage_core8 = 2
    select_08_1$aspirin_usage_core8[i] = 2
  }
}

#assign framid
select_08_1$framid <- (80000 + select_08_1$ID)
#rename columns
colnames(select_08_1) <- c("id", "idtype", "aspirin_use_core8", 
                           "aspirin_quantity_core8", "aspirin_freq_core8",
                           "aspirin_dose_core8", "aspirin_purpose_core8", "framid")
#replace NA
select_08_1 <- select_08_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Omni 1 Exam 1 ####
#E041, E218, E219
exam_01_7 <- read_sas("e_exam_ex01_7_0020.sas7bdat")
select_01_7 <- exam_01_7[,c("id", "idtype", "e041", "e218", "e219")]

#Placeholder values, used for testing
select_01_7$aspirin_usage_core1 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_01_7)) {
  if (is.na(select_01_7$e041[i]) | is.na(select_01_7$e218[i]) | is.na(select_01_7$e219[i])) {
    #If NA in any column then aspirin_usage_core1 = NA (This has to go first for logic)
    select_01_7$aspirin_usage_core1[i] = NA
  } else if (select_01_7$e041[i] >= 1 & select_01_7$e041[i] <= 7 
             & select_01_7$e219[i] >= 1 & select_01_7$e219[i] <= 2) {
    #If E219 is 1 or 2 and E041 is 1-7 then new variable is 1 
    select_01_7$aspirin_usage_core1[i] = 1
  } else if (select_01_7$e041[i] == 0 | select_01_7$e218[i] == 0 | select_01_7$e219[i] == 0) {
    #If E041 is 0 or E218 is 0 or E219 is 0 then new variable is 0 
    select_01_7$aspirin_usage_core1[i] = 0
  } else {
    #Else NA
    select_01_7$aspirin_usage_core1[i] = NA
  }
}

#assign framid
select_01_7$framid <- (700000 + select_01_7$id)
#remove unneeded columns
select_01_7 <- subset(select_01_7, select = -c(e041, e218, e219))
#rename columns
colnames(select_01_7) <- c("id", "idtype", "aspirin_use_core5", "framid")
#replace NA
select_01_7 <- select_01_7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
usage_all_exam_5 <- rbind(select_05_1, select_01_7)


#### Omni 1 Exam 2 ####
#G037-G040
exam_02_7 <- read_sas("e_exam_ex02_7_0003.sas7bdat")
select_02_7 <- exam_02_7[,c("id", "idtype", "g037", "g038", "g039", "g040")]

#Placeholder values, used for testing
select_02_7$aspirin_usage_core2 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_02_7)) {
  if (is.na(select_02_7$g038[i]) | is.na(select_02_7$g039[i]) | is.na(select_02_7$g040[i])) {
    #If NA in any column then aspirin_usage_core2 = NA (This has to go first for logic)
    select_02_7$aspirin_usage_core2[i] = NA
  } else if (select_02_7$g038[i] == 1 & select_02_7$g039[i] == 1 
             & select_02_7$g040[i] >= 81 & select_02_7$g040[i] <= 325) {
    #If G038 = 1 and G039 = 1 and G040 = 81-325 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] == 2 & select_02_7$g039[i] == 1 
             & select_02_7$g040[i] >= 81 & select_02_7$g040[i] <= 160) {
    #If G038 = 2 and G039 = 1 and G040 = 81-160 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] >= 3 & select_02_7$g038[i] <= 4 
             & select_02_7$g039[i] == 1 & select_02_7$g040[i] == 81 
             & select_02_7$g040[i] <= 163) {
    #If G038 = 3-4 and G039 = 1 and G040 = 81 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] >= 7 & select_02_7$g038[i] <= 14
             & select_02_7$g039[i] == 2 & select_02_7$g040[i] >= 81 
             & select_02_7$g040[i] <= 160) {
    #If G038 = 7-14 and G039 = 2 and G040 = 81-160 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] >= 3 & select_02_7$g038[i] <= 6
             & select_02_7$g039[i] == 2 & select_02_7$g040[i] >= 81 
             & select_02_7$g040[i] <= 325) {
    #If G038 = 3-6 and G039 = 2 and G040 = 81-325 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] >= 15 & select_02_7$g038[i] <= 28 
             & select_02_7$g039[i] == 2 & select_02_7$g040[i] >= 81) {
    #If G038 = 15-28 and G039 = 2 and G040 = 81 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] >= 7 & select_02_7$g038[i] <= 10 
             & select_02_7$g039[i] == 2 & select_02_7$g040[i] == 325) {
    #If G038 = 7-10 and G039 = 2 and G040 = 325 then aspirin_usage_core2 = 1
    select_02_7$aspirin_usage_core2[i] = 1
  } else if (select_02_7$g038[i] >= 10 & select_02_7$g038[i] <= 91 
             & select_02_7$g039[i] == 1 & select_02_7$g040[i] == 325) {
    #If G038 = 10-91 and G039 = 1 and G040 = 325 then aspirin_usage_core2 = 3
    select_02_7$aspirin_usage_core2[i] = 3
  } else if (select_02_7$g038[i] >= 6 & select_02_7$g038[i] <= 91 
             & select_02_7$g039[i] == 1 & select_02_7$g040[i] == 500) {
    #If G038 = 6-91 and G038 = 1 and G040 = 500 then aspirin_usage_core2 = 3
    select_02_7$aspirin_usage_core2[i] = 3
  } else if (select_02_7$g038[i] == 0 | select_02_7$g039[i] == 0
             | select_02_7$g039[i] == 3 | select_02_7$g039[i] == 4
             | select_02_7$g040[i] == 0) {
    #If G038 = (0, NA) or G039 = (0, 3, 4, NA) or G040 = (0, NA) then aspirin_usage_core2 = NA
    select_02_7$aspirin_usage_core2[i] = NA
  } else {
    #Else aspirin_usage_core2 = 2
    select_02_7$aspirin_usage_core2[i] = 2
  }
}

#assign framid
select_02_7$framid <- (700000 + select_02_7$id)
#rename columns
colnames(select_02_7) <- c("id", "idtype", "aspirin_use_core7", 
                           "aspirin_quantity_core7", "aspirin_freq_core7",
                           "aspirin_dose_core7", "aspirin_purpose_core7", "framid")
#replace NA
select_02_7 <- select_02_7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
usage_all_exam_7 <- rbind(select_07_1, select_02_7)


#### Omni 1 Exam 3 ####
#H010-H013
exam_03_7 <- read_sas("e_exam_ex03_7_0426.sas7bdat")
select_03_7 <- exam_03_7[,c("id", "idtype", "h010", "h011", "h012", "h013")]

#Placeholder values, used for testing
select_03_7$aspirin_usage_core3 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_03_7)) {
  if (is.na(select_03_7$h011[i]) | is.na(select_03_7$h012[i]) | is.na(select_03_7$h013[i])) {
    #If NA in any column then aspirin_usage_core3 = NA (This has to go first for logic)
    select_03_7$aspirin_usage_core3[i] = NA
  } else if (select_03_7$h011[i] == 1 & select_03_7$h012[i] == 1 
             & select_03_7$h013[i] >= 81 & select_03_7$h013[i] <= 325) {
    #If H011 = 1 and H012 = 1 and H013 = 81-325 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] == 2 & select_03_7$h012[i] == 1 
             & select_03_7$h013[i] >= 81 & select_03_7$h013[i] <= 160) {
    #If H011 = 2 and H012 = 1 and H013 = 81-160 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] >= 3 & select_03_7$h011[i] <= 4 
             & select_03_7$h012[i] == 1 & select_03_7$h013[i] == 81 
             & select_03_7$h013[i] <= 163) {
    #If H011 = 3-4 and H012 = 1 and H013 = 81 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] >= 7 & select_03_7$h011[i] <= 15
             & select_03_7$h012[i] == 2 & select_03_7$h013[i] >= 81 
             & select_03_7$h013[i] <= 160) {
    #If H011 = 7-15 and H012 = 2 and H013 = 81-160 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] >= 3 & select_03_7$h011[i] <= 6
             & select_03_7$h012[i] == 2 & select_03_7$h013[i] >= 81 
             & select_03_7$h013[i] <= 325) {
    #If H011 = 3-6 and H012 = 2 and H013 = 81-325 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] == 15 & select_03_7$h012[i] == 2 & select_03_7$h013[i] >= 81) {
    #If H011 = 15 and H012 = 2 and H013 = 81 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] >= 7 & select_03_7$h011[i] <= 10 
             & select_03_7$h012[i] == 2 & select_03_7$h013[i] == 325) {
    #If H011 = 7-10 and H012 = 2 and H013 = 325 then aspirin_usage_core3 = 1
    select_03_7$aspirin_usage_core3[i] = 1
  } else if (select_03_7$h011[i] >= 10 & select_03_7$h011[i] <= 15 
             & select_03_7$h012[i] == 1 & select_03_7$h013[i] == 325) {
    #If H011 = 10-15 and H012 = 1 and H013 = 325 then aspirin_usage_core3 = 3
    select_03_7$aspirin_usage_core3[i] = 3
  } else if (select_03_7$h011[i] >= 6 & select_03_7$h011[i] <= 15 
             & select_03_7$h012[i] == 1 & select_03_7$h013[i] == 500) {
    #If H011 = 6-15 and H011 = 1 and H013 = 500 then aspirin_usage_core3 = 3
    select_03_7$aspirin_usage_core3[i] = 3
  } else if (select_03_7$h011[i] == 0 | select_03_7$h012[i] == 0
             | select_03_7$h012[i] == 3 | select_03_7$h012[i] == 4
             | select_03_7$h013[i] == 0) {
    #If H011 = (0, NA) or H012 = (0, 3, 4, NA) or H013 = (0, NA) then aspirin_usage_core3 = NA
    select_03_7$aspirin_usage_core3[i] = NA
  } else {
    #Else aspirin_usage_core3 = 2
    select_03_7$aspirin_usage_core3[i] = 2
  }
}

#assign framid
select_03_7$framid <- (700000 + select_03_7$id)
#rename columns
colnames(select_03_7) <- c("id", "idtype", "aspirin_use_core8", 
                           "aspirin_quantity_core8", "aspirin_freq_core8",
                           "aspirin_dose_core8", "aspirin_purpose_core8", "framid")
#replace NA
select_03_7 <- select_03_7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
usage_all_exam_8 <- rbind(select_08_1, select_03_7)


#### Gen 2 Exam 9 Omni 1 Exam 9 ####
#ex09_1b (Offspring (GEN 2) Exam 9 and Omni 1 Exam 9)
#J010-J013
exam_09_1b <- read_sas("e_exam_ex09_1b_0844.sas7bdat")
select_09_1b <- exam_09_1b[,c("id", "idtype", "j010", "j011", "j012", "j013")]

#Placeholder values, used for testing
select_09_1b$aspirin_usage_core9 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_09_1b)) {
  if (is.na(select_09_1b$j011[i]) | is.na(select_09_1b$j012[i]) | is.na(select_09_1b$j013[i])) {
    #If NA in any column then aspirin_usage_core9 = NA (This has to go first for logic)
    select_09_1b$aspirin_usage_core9[i] = NA
  } else if (select_09_1b$j011[i] == 1 & select_09_1b$j012[i] == 1 
             & select_09_1b$j013[i] >= 81 & select_09_1b$j013[i] <= 325) {
    #If J011 = 1 and J012 = 1 and J013 = 81-325 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] == 2 & select_09_1b$j012[i] == 1 
             & select_09_1b$j013[i] >= 81 & select_09_1b$j013[i] <= 160) {
    #If J011 = 2 and J012 = 1 and J013 = 81-160 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] >= 3 & select_09_1b$j011[i] <= 4 
             & select_09_1b$j012[i] == 1 & select_09_1b$j013[i] == 81 
             & select_09_1b$j013[i] <= 163) {
    #If J011 = 3-4 and J012 = 1 and J013 = 81 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] >= 7 & select_09_1b$j011[i] <= 14
             & select_09_1b$j012[i] == 2 & select_09_1b$j013[i] >= 81 
             & select_09_1b$j013[i] <= 160) {
    #If J011 = 7-14 and J012 = 2 and J013 = 81-160 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] >= 3 & select_09_1b$j011[i] <= 6
             & select_09_1b$j012[i] == 2 & select_09_1b$j013[i] >= 81 
             & select_09_1b$j013[i] <= 325) {
    #If J011 = 3-6 and J012 = 2 and J013 = 81-325 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] >= 15 & select_09_1b$j011[i] <= 20 
             & select_09_1b$j012[i] == 2 & select_09_1b$j013[i] >= 81) {
    #If J011 = 15-20 and J012 = 2 and J013 = 81 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] >= 7 & select_09_1b$j011[i] <= 10 
             & select_09_1b$j012[i] == 2 & select_09_1b$j013[i] == 325) {
    #If J011 = 7-10 and J012 = 2 and J013 = 325 then aspirin_usage_core9 = 1
    select_09_1b$aspirin_usage_core9[i] = 1
  } else if (select_09_1b$j011[i] >= 10 & select_09_1b$j011[i] <= 20 
             & select_09_1b$j012[i] == 1 & select_09_1b$j013[i] == 325) {
    #If J011 = 10-20 and J012 = 1 and J013 = 325 then aspirin_usage_core9 = 3
    select_09_1b$aspirin_usage_core9[i] = 3
  } else if (select_09_1b$j011[i] >= 6 & select_09_1b$j011[i] <= 20 
             & select_09_1b$j012[i] == 1 & select_09_1b$j013[i] == 500) {
    #If J011 = 6-20 and J011 = 1 and J013 = 500 then aspirin_usage_core9 = 3
    select_09_1b$aspirin_usage_core9[i] = 3
  } else if (select_09_1b$j011[i] >= 5 & select_09_1b$j011[i] <= 20 
             & select_09_1b$j012[i] == 1 & select_09_1b$j013[i] >= 600 
             & select_09_1b$j013[i] <= 650) {
    #If J011 = 5 - 20 and J012 = 1 and J013 = 600-650 then aspirin_usage_core9 = 3
    select_09_1b$aspirin_usage_core9[i] = 3
  } else if (select_09_1b$j011[i] == 0 | select_09_1b$j012[i] == 0
             | select_09_1b$j012[i] == 3 | select_09_1b$j012[i] == 4
             | select_09_1b$j013[i] == 0) {
    #If J011 = (0, NA) or J012 = (0, 3, 4, NA) or J013 = (0, NA) then aspirin_usage_core9 = NA
    select_09_1b$aspirin_usage_core9[i] = NA
  } else {
    #Else aspirin_usage_core9 = 2
    select_09_1b$aspirin_usage_core9[i] = 2
  }
}

#split into Gen 2 Exam 9 and Omni 1 Exam 9
select_09_1 <- subset(select_09_1b, idtype == 1)
select_09_7 <- subset(select_09_1b, idtype == 7)

#assign framid
select_09_1$framid <- (80000 + select_09_1$id)
#rename columns
colnames(select_09_1) <- c("id", "idtype", "aspirin_use_core9", 
                           "aspirin_quantity_core9", "aspirin_freq_core9",
                           "aspirin_dose_core9", "aspirin_purpose_core9", "framid")
#replace NA
select_09_1 <- select_09_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#assign framid
select_09_7$framid <- (700000 + select_09_7$id)
#rename columns
colnames(select_09_7) <- c("id", "idtype", "aspirin_use_core9", 
                           "aspirin_quantity_core9", "aspirin_freq_core9",
                           "aspirin_dose_core9", "aspirin_purpose_core9", "framid")
#replace NA
select_09_7 <- select_09_7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
usage_all_exam_9 <- rbind(select_09_1, select_09_7)


#### Gen 2 Exam 10 Omni 1 Exam 5 ####
#ex10_1b (Offspring (GEN 2) Exam 10 and Omni 1 Exam 5)
#K0211-K0214
exam_10_1b <- read_sas("e_exam_ex10_1b_1409.sas7bdat")
select_10_1b <- exam_10_1b[,c("id", "idtype", "K0211", "K0212", "K0213", "K0214", "K0215")]

#redo ambiguous K0214
for (i in 1:nrow(select_10_1b)) {
  if (is.na(select_10_1b$K0214[i])) {
    #do nothing
  } else if (select_10_1b$K0214[i] == 888) {
    select_10_1b$K0214[i] = select_10_1b$K0215[i]
  }
}

#remove K0215
select_10_1b <- select(select_10_1b, -K0215)

#Placeholder values, used for testing
select_10_1b$aspirin_usage_core10 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_10_1b)) {
  if (is.na(select_10_1b$K0212[i]) | is.na(select_10_1b$K0213[i]) | is.na(select_10_1b$K0214[i])) {
    #If NA in any column then aspirin_usage_core10 = NA (This has to go first for logic)
    select_10_1b$aspirin_usage_core10[i] = NA
  } else if (select_10_1b$K0212[i] == 1 & select_10_1b$K0213[i] == 1 
             & select_10_1b$K0214[i] >= 81 & select_10_1b$K0214[i] <= 325) {
    #If K0212 = 1 and K0213 = 1 and K0214 = 81-325 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] == 2 & select_10_1b$K0213[i] == 1 
             & select_10_1b$K0214[i] >= 81 & select_10_1b$K0214[i] <= 160) {
    #If K0212 = 2 and K0213 = 1 and K0214 = 81-160 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] >= 3 & select_10_1b$K0212[i] <= 4 
             & select_10_1b$K0213[i] == 1 & select_10_1b$K0214[i] == 81 
             & select_10_1b$K0214[i] <= 163) {
    #If K0212 = 3-4 and K0213 = 1 and K0214 = 81 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] >= 7 & select_10_1b$K0212[i] <= 14
             & select_10_1b$K0213[i] == 2 & select_10_1b$K0214[i] >= 81 
             & select_10_1b$K0214[i] <= 160) {
    #If K0212 = 7-14 and K0213 = 2 and K0214 = 81-160 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] >= 3 & select_10_1b$K0212[i] <= 6
             & select_10_1b$K0213[i] == 2 & select_10_1b$K0214[i] >= 81 
             & select_10_1b$K0214[i] <= 325) {
    #If K0212 = 3-6 and K0213 = 2 and K0214 = 81-325 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] >= 15 & select_10_1b$K0212[i] <= 20 
             & select_10_1b$K0213[i] == 2 & select_10_1b$K0214[i] >= 81) {
    #If K0212 = 15-20 and K0213 = 2 and K0214 = 81 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] >= 7 & select_10_1b$K0212[i] <= 10 
             & select_10_1b$K0213[i] == 2 & select_10_1b$K0214[i] == 325) {
    #If K0212 = 7-10 and K0213 = 2 and K0214 = 325 then aspirin_usage_core10 = 1
    select_10_1b$aspirin_usage_core10[i] = 1
  } else if (select_10_1b$K0212[i] >= 10 & select_10_1b$K0212[i] <= 20 
             & select_10_1b$K0213[i] == 1 & select_10_1b$K0214[i] == 325) {
    #If K0212 = 10-20 and K0213 = 1 and K0214 = 325 then aspirin_usage_core10 = 3
    select_10_1b$aspirin_usage_core10[i] = 3
  } else if (select_10_1b$K0212[i] >= 6 & select_10_1b$K0212[i] <= 20 
             & select_10_1b$K0213[i] == 1 & select_10_1b$K0214[i] == 500) {
    #If K0212 = 6-20 and K0212 = 1 and K0214 = 500 then aspirin_usage_core10 = 3
    select_10_1b$aspirin_usage_core10[i] = 3
  } else if (select_10_1b$K0212[i] >= 5 & select_10_1b$K0212[i] <= 20 
             & select_10_1b$K0213[i] == 1 & select_10_1b$K0214[i] >= 600 
             & select_10_1b$K0214[i] <= 650) {
    #If K0212 = 5 - 20 and K0213 = 1 and K0214 = 600-650 then aspirin_usage_core10 = 3
    select_10_1b$aspirin_usage_core10[i] = 3
  } else if (select_10_1b$K0212[i] == 0 | select_10_1b$K0213[i] == 0
             | select_10_1b$K0213[i] == 3 | select_10_1b$K0213[i] == 4
             | select_10_1b$K0214[i] == 0) {
    #If K0212 = (0, NA) or K0213 = (0, 3, 4, NA) or K0214 = (0, NA) then aspirin_usage_core10 = NA
    select_10_1b$aspirin_usage_core10[i] = NA
  } else {
    #Else aspirin_usage_core10 = 2
    select_10_1b$aspirin_usage_core10[i] = 2
  }
}

#split into Gen 2 Exam 10 and Omni 1 Exam 5
select_10_1 <- subset(select_10_1b, idtype == 1)
select_05_7 <- subset(select_10_1b, idtype == 7)

#assign framid
select_10_1$framid <- (80000 + select_10_1$id)
#rename columns
colnames(select_10_1) <- c("id", "idtype", "aspirin_use_core10", 
                           "aspirin_quantity_core10", "aspirin_freq_core10",
                           "aspirin_dose_core10", "aspirin_purpose_core10", "framid")
#replace NA
select_10_1 <- select_10_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#assign framid
select_05_7$framid <- (700000 + select_05_7$id)
#rename columns
colnames(select_05_7) <- c("id", "idtype", "aspirin_use_core10", 
                           "aspirin_quantity_core10", "aspirin_freq_core10",
                           "aspirin_dose_core10", "aspirin_purpose_core10", "framid")
#replace NA
select_05_7 <- select_05_7 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
usage_all_exam_10 <- rbind(select_10_1, select_05_7)


#### Final Merge ####
#merge datasets
gen2_list <- list(select_02_1, select_03_1, select_04_1, usage_all_exam_5, 
                  select_06_1, usage_all_exam_7, usage_all_exam_8, usage_all_exam_9, usage_all_exam_10)
gen2_joined <- gen2_list %>% reduce(full_join, by = c("idtype", "id", "framid"))
#relocate framid column
gen2_relocated <- gen2_joined %>% relocate(framid, .after = idtype)
#change identifying columns to numeric type
gen2_relocated[c("idtype", "id", "framid")] <- sapply(gen2_relocated[c("idtype", "id", "framid")], as.numeric)
#reorder rows
gen2_final <- gen2_relocated %>% arrange(framid)

#write final csv
write.csv(gen2_final, file = "aspirin_gen2_full_spreadsheet.csv", row.names = FALSE)

