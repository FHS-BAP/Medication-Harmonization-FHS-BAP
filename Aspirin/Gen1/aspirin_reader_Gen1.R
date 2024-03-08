# ******************************************************************************************************************************************
# Introduction to Gen 1 aspirin derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: February 2024
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 1 aspirin usage.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
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
# e_exam_ex29_0_0210_v2.sas7bdat (Gen 1 Exam 29)
# e_exam_ex30_0_0274.sas7bdat (Gen 1 Exam 30)
# e_exam_ex31_0_0738.sas7bdat (Gen 1 Exam 31)
# e_exam_ex32_0_0939.sas7bdat (Gen 1 Exam 32)
# 
# 
# Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality


#### Exam 12 ####
#FE100
exam_12_0 <- read_sas("e_exam_ex12_0_0060_v1.sas7bdat")
select_12_0 <- exam_12_0[,c("ID", "FE100")]

#Placeholder values, used for testing
select_12_0$aspirin_derived_core12 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_12_0)) {
  if (is.na(select_12_0$FE100[i])) {
    #Unknown = NA
    select_12_0$aspirin_derived_core12[i] = NA
  } else if (select_12_0$FE100[i] >= 2) {
    #2 or above) is 1
    select_12_0$aspirin_derived_core12[i] = 1
  } else {
    #(0-1) is 0
    select_12_0$aspirin_derived_core12[i] = 0
  }
}

#remove unneeded column
select_12_0 <- select_12_0 %>% select(-FE100)
#rename columns
colnames(select_12_0) <- c("id", "aspirin_use_core12")
#replace NA
select_12_0 <- select_12_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 13 ####
#FF102
exam_13_0 <- read_sas("e_exam_ex13_0_0061_v1.sas7bdat")
select_13_0 <- exam_13_0[,c("ID", "FF102")]

#Placeholder values, used for testing
select_13_0$aspirin_derived_core13 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_13_0)) {
  if (is.na(select_13_0$FF102[i])) {
    #Unknown = NA
    select_13_0$aspirin_derived_core13[i] = NA
  } else if (select_13_0$FF102[i] >= 2) {
    #2 or above) is 1
    select_13_0$aspirin_derived_core13[i] = 1
  } else {
    #(0-1) is 0
    select_13_0$aspirin_derived_core13[i] = 0
  }
}

#remove unneeded column
select_13_0 <- select_13_0 %>% select(-FF102)
#rename columns
colnames(select_13_0) <- c("id", "aspirin_use_core13")
#replace NA
select_13_0 <- select_13_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 17 ####
#FJ37
exam_17_0 <- read_sas("e_exam_ex17_0_0065.sas7bdat")
select_17_0 <- exam_17_0[,c("ID", "FJ37")]

#Placeholder values, used for testing
select_17_0$aspirin_derived_core17 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_17_0)) {
  if (is.na(select_17_0$FJ37[i])) {
    #Unknown = NA
    select_17_0$aspirin_derived_core17[i] = NA
  } else if (select_17_0$FJ37[i] >= 2) {
    #2 or above) is 1
    select_17_0$aspirin_derived_core17[i] = 1
  } else {
    #(0-1) is 0
    select_17_0$aspirin_derived_core17[i] = 0
  }
}

#remove unneeded column
select_17_0 <- select_17_0 %>% select(-FJ37)
#rename columns
colnames(select_17_0) <- c("id", "aspirin_use_core17")
#replace NA
select_17_0 <- select_17_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 18 ####
#FK130
exam_18_0 <- read_sas("e_exam_ex18_0_0066.sas7bdat")
select_18_0 <- exam_18_0[,c("ID", "FK130")]

#Placeholder values, used for testing
select_18_0$aspirin_derived_core18 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_18_0)) {
  if (is.na(select_18_0$FK130[i])) {
    #Unknown = NA
    select_18_0$aspirin_derived_core18[i] = NA
  } else if (select_18_0$FK130[i] >= 2) {
    #2 or above) is 1
    select_18_0$aspirin_derived_core18[i] = 1
  } else {
    #(0-1) is 0
    select_18_0$aspirin_derived_core18[i] = 0
  }
}

#remove unneeded column
select_18_0 <- select_18_0 %>% select(-FK130)
#rename columns
colnames(select_18_0) <- c("id", "aspirin_use_core18")
#replace NA
select_18_0 <- select_18_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 19 ####
#FL162
exam_19_0 <- read_sas("e_exam_ex19_0_0067.sas7bdat")
select_19_0 <- exam_19_0[,c("ID", "FL162")]

#Placeholder values, used for testing
select_19_0$aspirin_derived_core19 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_19_0)) {
  if (is.na(select_19_0$FL162[i])) {
    #Unknown = NA
    select_19_0$aspirin_derived_core19[i] = NA
  } else if (select_19_0$FL162[i] >= 1) {
    #If it’s a positive value it’s 1
    select_19_0$aspirin_derived_core19[i] = 1
  } else {
    #If the aspirin value is 0 it’s 0
    select_19_0$aspirin_derived_core19[i] = 0
  }
}

#remove unneeded column
select_19_0 <- select_19_0 %>% select(-FL162)
#rename columns
colnames(select_19_0) <- c("id", "aspirin_use_core19")
#replace NA
select_19_0 <- select_19_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 20 ####
#FM155
exam_20_0 <- read_sas("e_exam_ex20_0_0068.sas7bdat")
select_20_0 <- exam_20_0[,c("ID", "FM155")]

#Placeholder values, used for testing
select_20_0$aspirin_derived_core20 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_20_0)) {
  if (is.na(select_20_0$FM155[i])) {
    #Unknown = NA
    select_20_0$aspirin_derived_core20[i] = NA
  } else if (select_20_0$FM155[i] >= 1) {
    #If it’s a positive value it’s 1
    select_20_0$aspirin_derived_core20[i] = 1
  } else {
    #If the aspirin value is 0 it’s 0
    select_20_0$aspirin_derived_core20[i] = 0
  }
}

#remove unneeded column
select_20_0 <- select_20_0 %>% select(-FM155)
#rename columns
colnames(select_20_0) <- c("id", "aspirin_use_core20")
#replace NA
select_20_0 <- select_20_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 21 ####
#FN93
exam_21_0 <- read_sas("e_exam_ex21_0_0069.sas7bdat")
select_21_0 <- exam_21_0[,c("ID", "FN93")]

#Placeholder values, used for testing
select_21_0$aspirin_derived_core21 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_21_0)) {
  if (is.na(select_21_0$FN93[i])) {
    #Unknown = NA
    select_21_0$aspirin_derived_core21[i] = NA
  } else if (select_21_0$FN93[i] >= 1) {
    #If it’s a positive value it’s 1
    select_21_0$aspirin_derived_core21[i] = 1
  } else {
    #If the aspirin value is 0 it’s 0
    select_21_0$aspirin_derived_core21[i] = 0
  }
}

#remove unneeded column
select_21_0 <- select_21_0 %>% select(-FN93)
#rename columns
colnames(select_21_0) <- c("id", "aspirin_use_core21")
#replace NA
select_21_0 <- select_21_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 22 ####
#FO091, FO092
exam_22_0 <- read_sas("e_exam_ex22_0_0070.sas7bdat")
select_22_0 <- exam_22_0[,c("ID", "FO091", "FO092")]

#Placeholder values, used for testing
select_22_0$aspirin_derived_core22 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_22_0)) {
  if (is.na(select_22_0$FO091[i]) | is.na(select_22_0$FO092[i])) {
    #Unknown = NA
    select_22_0$aspirin_derived_core22[i] = NA
  } else if (select_22_0$FO091[i] == 0 | select_22_0$FO092[i] == 0) {
    #If FO091 or FO092 is 0, code as 0
    select_22_0$aspirin_derived_core22[i] = 0
  } else if (select_22_0$FO091[i] >= 1 & select_22_0$FO092[i] >= 1 
             & select_22_0$FO092[i] <= 2) {
    #If FO091 = 1 or more and FO092 = (1 or 2), code as 1
    select_22_0$aspirin_derived_core22[i] = 1
  } else if (select_22_0$FO091[i] >= 4 & select_22_0$FO092[i] == 3) {
    #If FO091 = 4 or more and FO092 = (3), code as 1
    select_22_0$aspirin_derived_core22[i] = 1
  } else if (select_22_0$FO092[i] == 4) {
    #If FO091 = any value and FO092 = (4), code as 0
    select_22_0$aspirin_derived_core22[i] = 0
  } else {
    #Else NA
    select_22_0$aspirin_derived_core22[i] = NA
  }
}

#rename columns
colnames(select_22_0) <- c("id", "aspirin_quantity_core22", "aspirin_freq_core22", "aspirin_use_core22")
#replace NA
select_22_0 <- select_22_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
#relocate use_core column
select_22_0 <- select_22_0 %>% relocate(aspirin_use_core22, .after = id)

#### Exam 23 ####
#FP044, FP045, FP046
exam_23_0 <- read_sas("e_exam_ex23_0_0071.sas7bdat")
select_23_0 <- exam_23_0[,c("ID", "FP044", "FP045", "FP046")]

#Placeholder values, used for testing
select_23_0$aspirin_derived_core23 = 9999
select_23_0$aspirin_extra_core23 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_23_0)) {
  if (is.na(select_23_0$FP044[i]) | is.na(select_23_0$FP045[i]) | is.na(select_23_0$FP046[i])) {
    #If NA in any column then aspirin_derived_core23 = NA (This has to go first for logic)
    select_23_0$aspirin_derived_core23[i] = NA
  } else if (select_23_0$FP044[i] == 1 & select_23_0$FP045[i] == 1 
             & select_23_0$FP046[i] >= 80 & select_23_0$FP046[i] <= 325) {
    #If FP044 = 1 and FP045 = 1 and FP046 = 80-325 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] == 2 & select_23_0$FP045[i] == 1 
             & select_23_0$FP046[i] >= 80 & select_23_0$FP046[i] <= 160) {
    #If FP044 = 2 and FP045 = 1 and FP046 = 80-160 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 3 & select_23_0$FP044[i] <= 4 
             & select_23_0$FP045[i] == 1 & select_23_0$FP046[i] == 80) {
    #If FP044 = 3-4 and FP045 = 1 and FP046 = 80 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 7 & select_23_0$FP044[i] <= 14
             & select_23_0$FP045[i] == 2 & select_23_0$FP046[i] >= 80 
             & select_23_0$FP046[i] <= 160) {
    #If FP044 = 7-14 and FP045 = 2 and FP046 = 80-160 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 3 & select_23_0$FP044[i] <= 6
             & select_23_0$FP045[i] == 2 & select_23_0$FP046[i] >= 80 
             & select_23_0$FP046[i] <= 325) {
    #If FP044 = 3-6 and FP045 = 2 and FP046 = 80-325 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 15 & select_23_0$FP044[i] <= 16 
             & select_23_0$FP045[i] == 2 & select_23_0$FP046[i] >= 80) {
    #If FP044 = 15-16 and FP045 = 2 and FP046 = 80 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 7 & select_23_0$FP044[i] <= 9 
             & select_23_0$FP045[i] == 2 & select_23_0$FP046[i] == 325) {
    #If FP044 = 7-9 and FP045 = 2 and FP046 = 325 then aspirin_derived_core23 = 1
    select_23_0$aspirin_derived_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 10 & select_23_0$FP044[i] <= 16 
             & select_23_0$FP045[i] == 1 & select_23_0$FP046[i] == 325) {
    #If FP044 = 10-16 and FP045 = 1 and FP046 = 325 then aspirin_derived_core23 = 3
    select_23_0$aspirin_derived_core23[i] = 3
  } else if (select_23_0$FP044[i] >= 6 & select_23_0$FP044[i] <= 16 
             & select_23_0$FP045[i] == 1 & select_23_0$FP046[i] == 500) {
    #If FP044 = 6-16 and FP044 = 1 and FP046 = 500 then aspirin_derived_core23 = 3
    select_23_0$aspirin_derived_core23[i] = 3
  } else if (select_23_0$FP044[i] == 0 | select_23_0$FP045[i] == 0
             | select_23_0$FP045[i] == 3 | select_23_0$FP045[i] == 4
             | select_23_0$FP046[i] == 0) {
    #If FP044 = (0, NA) or FP045 = (0, 3, 4, NA) or FP046 = (0, NA) then aspirin_derived_core23 = NA
    select_23_0$aspirin_derived_core23[i] = NA
  } else {
    #Else aspirin_derived_core23 = 2
    select_23_0$aspirin_derived_core23[i] = 2
  }
}

for (i in 1:nrow(select_23_0)) {
  if (is.na(select_23_0$FP044[i]) | is.na(select_23_0$FP045[i])) {
    #Unknown = NA
    select_23_0$aspirin_extra_core23[i] = NA
  } else if (select_23_0$FP044[i] == 0 | select_23_0$FP045[i] == 0) {
    #If FP044 or FP045 is 0, code as 0
    select_23_0$aspirin_extra_core23[i] = 0
  } else if (select_23_0$FP044[i] >= 1 & select_23_0$FP045[i] >= 1 
             & select_23_0$FP045[i] <= 2) {
    #If FP044 = 1 or more and FP045 = (1 or 2), code as 1
    select_23_0$aspirin_extra_core23[i] = 1
  } else if (select_23_0$FP044[i] >= 4 & select_23_0$FP045[i] == 3) {
    #If FP044 = 4 or more and FP045 = (3), code as 1
    select_23_0$aspirin_extra_core23[i] = 1
  } else if (select_23_0$FP045[i] == 4) {
    #If FP044 = any value and FP045 = (4), code as 0
    select_23_0$aspirin_extra_core23[i] = 0
  } else {
    #Else NA
    select_23_0$aspirin_extra_core23[i] = NA
  }
}

#rename columns
colnames(select_23_0) <- c("id", "aspirin_quantity_core23", "aspirin_freq_core23", 
                           "aspirin_dose_core23", "aspirin_purpose_core23", "aspirin_use_core23")
#replace NA
select_23_0 <- select_23_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
#relocate use_core column
select_23_0 <- select_23_0 %>% relocate(aspirin_use_core23, .after = id)


#### Exam 24 ####
#FQ128-FQ131
exam_24_0 <- read_sas("e_exam_ex24_0_0072.sas7bdat")
select_24_0 <- exam_24_0[,c("ID", "FQ128", "FQ129", "FQ130", "FQ131")]

#Placeholder values, used for testing
select_24_0$aspirin_derived_core24 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_24_0)) {
  if (is.na(select_24_0$FQ129[i]) | is.na(select_24_0$FQ130[i]) | is.na(select_24_0$FQ131[i])) {
    #If NA in any column then aspirin_derived_core24 = NA (This has to go first for logic)
    select_24_0$aspirin_derived_core24[i] = NA
  } else if (select_24_0$FQ129[i] == 1 & select_24_0$FQ130[i] == 1 
             & select_24_0$FQ131[i] >= 81 & select_24_0$FQ131[i] <= 325) {
    #If FQ129 = 1 and FQ130 = 1 and FQ131 = 81-325 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] == 2 & select_24_0$FQ130[i] == 1 
             & select_24_0$FQ131[i] >= 81 & select_24_0$FQ131[i] <= 160) {
    #If FQ129 = 2 and FQ130 = 1 and FQ131 = 81-160 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] >= 3 & select_24_0$FQ129[i] <= 4 
             & select_24_0$FQ130[i] == 1 & select_24_0$FQ131[i] == 81) {
    #If FQ129 = 3-4 and FQ130 = 1 and FQ131 = 81 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] >= 7 & select_24_0$FQ129[i] <= 14
             & select_24_0$FQ130[i] == 2 & select_24_0$FQ131[i] >= 81 
             & select_24_0$FQ131[i] <= 160) {
    #If FQ129 = 7-14 and FQ130 = 2 and FQ131 = 81-160 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] >= 3 & select_24_0$FQ129[i] <= 6
             & select_24_0$FQ130[i] == 2 & select_24_0$FQ131[i] >= 81 
             & select_24_0$FQ131[i] <= 325) {
    #If FQ129 = 3-6 and FQ130 = 2 and FQ131 = 81-325 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] == 15 & select_24_0$FQ130[i] == 2 
             & select_24_0$FQ131[i] >= 81) {
    #If FQ129 = 15 and FQ130 = 2 and FQ131 = 81 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] >= 7 & select_24_0$FQ129[i] <= 9 
             & select_24_0$FQ130[i] == 2 & select_24_0$FQ131[i] == 325) {
    #If FQ129 = 7-9 and FQ130 = 2 and FQ131 = 325 then aspirin_derived_core24 = 1
    select_24_0$aspirin_derived_core24[i] = 1
  } else if (select_24_0$FQ129[i] >= 10 & select_24_0$FQ129[i] <= 15 
             & select_24_0$FQ130[i] == 1 & select_24_0$FQ131[i] == 325) {
    #If FQ129 = 10-15 and FQ130 = 1 and FQ131 = 325 then aspirin_derived_core24 = 3
    select_24_0$aspirin_derived_core24[i] = 3
  } else if (select_24_0$FQ129[i] >= 6 & select_24_0$FQ129[i] <= 15 
             & select_24_0$FQ130[i] == 1 & select_24_0$FQ131[i] == 500) {
    #If FQ129 = 6-15 and FQ129 = 1 and FQ131 = 500 then aspirin_derived_core24 = 3
    select_24_0$aspirin_derived_core24[i] = 3
  } else if (select_24_0$FQ129[i] == 0 | select_24_0$FQ130[i] == 0
             | select_24_0$FQ130[i] == 3 | select_24_0$FQ130[i] == 4
             | select_24_0$FQ131[i] == 0) {
    #If FQ129 = (0, NA) or FQ130 = (0, 3, 4, NA) or FQ131 = (0, NA) then aspirin_derived_core24 = NA
    select_24_0$aspirin_derived_core24[i] = NA
  } else {
    #Else aspirin_derived_core24 = 2
    select_24_0$aspirin_derived_core24[i] = 2
  }
}

#rename columns
colnames(select_24_0) <- c("id", "aspirin_use_core24", "aspirin_quantity_core24", 
                           "aspirin_freq_core24", "aspirin_dose_core24", "aspirin_purpose_core24")
#replace NA
select_24_0 <- select_24_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 25 ####
#FR180-FR183
exam_25_0 <- read_sas("e_exam_ex25_0_0073.sas7bdat")
select_25_0 <- exam_25_0[,c("ID", "FR180", "FR181", "FR182", "FR183")]

#Placeholder values, used for testing
select_25_0$aspirin_derived_core25 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_25_0)) {
  if (is.na(select_25_0$FR181[i]) | is.na(select_25_0$FR182[i]) | is.na(select_25_0$FR183[i])) {
    #If NA in any column then aspirin_derived_core25 = NA (This has to go first for logic)
    select_25_0$aspirin_derived_core25[i] = NA
  } else if (select_25_0$FR181[i] == 1 & select_25_0$FR182[i] == 1 
             & select_25_0$FR183[i] >= 81 & select_25_0$FR183[i] <= 325) {
    #If FR181 = 1 and FR182 = 1 and FR183 = 81-325 then aspirin_derived_core25 = 1
    select_25_0$aspirin_derived_core25[i] = 1
  } else if (select_25_0$FR181[i] == 2 & select_25_0$FR182[i] == 1 
             & select_25_0$FR183[i] >= 81 & select_25_0$FR183[i] <= 160) {
    #If FR181 = 2 and FR182 = 1 and FR183 = 81-160 then aspirin_derived_core25 = 1
    select_25_0$aspirin_derived_core25[i] = 1
  } else if (select_25_0$FR181[i] >= 3 & select_25_0$FR181[i] <= 4 
             & select_25_0$FR182[i] == 1 & select_25_0$FR183[i] == 81) {
    #If FR181 = 3-4 and FR182 = 1 and FR183 = 81 then aspirin_derived_core25 = 1
    select_25_0$aspirin_derived_core25[i] = 1
  } else if (select_25_0$FR181[i] >= 3 & select_25_0$FR181[i] <= 4
             & select_25_0$FR182[i] == 2 & select_25_0$FR183[i] >= 81 
             & select_25_0$FR183[i] <= 325) {
    #If FR181 = 3-4 and FR182 = 2 and FR183 = 81-325 then aspirin_derived_core25 = 1
    select_25_0$aspirin_derived_core25[i] = 1
  } else if (select_25_0$FR181[i] == 0 | select_25_0$FR182[i] == 0
             | select_25_0$FR182[i] == 3 | select_25_0$FR182[i] == 4
             | select_25_0$FR183[i] == 0) {
    #If FR181 = (0, NA) or FR182 = (0, 3, 4, NA) or FR183 = (0, NA) then aspirin_derived_core25 = NA
    select_25_0$aspirin_derived_core25[i] = NA
  } else {
    #Else aspirin_derived_core25 = 2
    select_25_0$aspirin_derived_core25[i] = 2
  }
}

#rename columns
colnames(select_25_0) <- c("id", "aspirin_use_core25", "aspirin_quantity_core25", 
                           "aspirin_freq_core25", "aspirin_dose_core25", "aspirin_purpose_core25")
#replace NA
select_25_0 <- select_25_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 26 ####
#FS265-FS268
exam_26_0 <- read_sas("e_exam_ex26_0_0074.sas7bdat")
select_26_0 <- exam_26_0[,c("ID", "FS265", "FS266", "FS267", "FS268")]

#Placeholder values, used for testing
select_26_0$aspirin_derived_core26 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_26_0)) {
  if (is.na(select_26_0$FS266[i]) | is.na(select_26_0$FS267[i]) | is.na(select_26_0$FS268[i])) {
    #If NA in any column then aspirin_derived_core26 = NA (This has to go first for logic)
    select_26_0$aspirin_derived_core26[i] = NA
  } else if (select_26_0$FS266[i] == 1 & select_26_0$FS267[i] == 1 
             & select_26_0$FS268[i] >= 81 & select_26_0$FS268[i] <= 325) {
    #If FS266 = 1 and FS267 = 1 and FS268 = 81-325 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] == 2 & select_26_0$FS267[i] == 1 
             & select_26_0$FS268[i] >= 81 & select_26_0$FS268[i] <= 160) {
    #If FS266 = 2 and FS267 = 1 and FS268 = 81-160 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] >= 3 & select_26_0$FS266[i] <= 4 
             & select_26_0$FS267[i] == 1 & select_26_0$FS268[i] == 81) {
    #If FS266 = 3-4 and FS267 = 1 and FS268 = 81 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] >= 7 & select_26_0$FS266[i] <= 14
             & select_26_0$FS267[i] == 2 & select_26_0$FS268[i] >= 81 
             & select_26_0$FS268[i] <= 160) {
    #If FS266 = 7-14 and FS267 = 2 and FS268 = 81-160 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] >= 3 & select_26_0$FS266[i] <= 6
             & select_26_0$FS267[i] == 2 & select_26_0$FS268[i] >= 81 
             & select_26_0$FS268[i] <= 325) {
    #If FS266 = 3-6 and FS267 = 2 and FS268 = 81-325 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] == 15 & select_26_0$FS267[i] == 2 
             & select_26_0$FS268[i] >= 81) {
    #If FS266 = 15 and FS267 = 2 and FS268 = 81 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] >= 7 & select_26_0$FS266[i] <= 9 
             & select_26_0$FS267[i] == 2 & select_26_0$FS268[i] == 325) {
    #If FS266 = 7-9 and FS267 = 2 and FS268 = 325 then aspirin_derived_core26 = 1
    select_26_0$aspirin_derived_core26[i] = 1
  } else if (select_26_0$FS266[i] >= 10 & select_26_0$FS266[i] <= 15 
             & select_26_0$FS267[i] == 1 & select_26_0$FS268[i] == 325) {
    #If FS266 = 10-15 and FS267 = 1 and FS268 = 325 then aspirin_derived_core26 = 3
    select_26_0$aspirin_derived_core26[i] = 3
  } else if (select_26_0$FS266[i] >= 6 & select_26_0$FS266[i] <= 15 
             & select_26_0$FS267[i] == 1 & select_26_0$FS268[i] == 500) {
    #If FS266 = 6-15 and FS266 = 1 and FS268 = 500 then aspirin_derived_core26 = 3
    select_26_0$aspirin_derived_core26[i] = 3
  } else if (select_26_0$FS266[i] == 0 | select_26_0$FS267[i] == 0
             | select_26_0$FS267[i] == 3 | select_26_0$FS267[i] == 4
             | select_26_0$FS268[i] == 0) {
    #If FS266 = (0, NA) or FS267 = (0, 3, 4, NA) or FS268 = (0, NA) then aspirin_derived_core26 = NA
    select_26_0$aspirin_derived_core26[i] = NA
  } else {
    #Else aspirin_derived_core26 = 2
    select_26_0$aspirin_derived_core26[i] = 2
  }
}

#rename columns
colnames(select_26_0) <- c("id", "aspirin_use_core26", "aspirin_quantity_core26", 
                           "aspirin_freq_core26", "aspirin_dose_core26", "aspirin_purpose_core26")
#replace NA
select_26_0 <- select_26_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 27 ####
#FT272-FT275
exam_27_0 <- read_sas("e_exam_ex27_0_0075.sas7bdat")
select_27_0 <- exam_27_0[,c("ID", "FT272", "FT273", "FT274", "FT275")]

#Placeholder values, used for testing
select_27_0$aspirin_derived_core27 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_27_0)) {
  if (is.na(select_27_0$FT273[i]) | is.na(select_27_0$FT274[i]) | is.na(select_27_0$FT275[i])) {
    #If NA in any column then aspirin_derived_core27 = NA (This has to go first for logic)
    select_27_0$aspirin_derived_core27[i] = NA
  } else if (select_27_0$FT273[i] == 1 & select_27_0$FT274[i] == 1 
             & select_27_0$FT275[i] >= 81 & select_27_0$FT275[i] <= 325) {
    #If FT273 = 1 and FT274 = 1 and FT275 = 81-325 then aspirin_derived_core27 = 1
    select_27_0$aspirin_derived_core27[i] = 1
  } else if (select_27_0$FT273[i] == 2 & select_27_0$FT274[i] == 1 
             & select_27_0$FT275[i] >= 81 & select_27_0$FT275[i] <= 160) {
    #If FT273 = 2 and FT274 = 1 and FT275 = 81-160 then aspirin_derived_core27 = 1
    select_27_0$aspirin_derived_core27[i] = 1
  } else if (select_27_0$FT273[i] >= 3 & select_27_0$FT273[i] <= 4 
             & select_27_0$FT274[i] == 1 & select_27_0$FT275[i] == 81) {
    #If FT273 = 3-4 and FT274 = 1 and FT275 = 81 then aspirin_derived_core27 = 1
    select_27_0$aspirin_derived_core27[i] = 1
  } else if (select_27_0$FT273[i] >= 3 & select_27_0$FT273[i] <= 4
             & select_27_0$FT274[i] == 2 & select_27_0$FT275[i] >= 81 
             & select_27_0$FT275[i] <= 325) {
    #If FT273 = 3-4 and FT274 = 2 and FT275 = 81-325 then aspirin_derived_core27 = 1
    select_27_0$aspirin_derived_core27[i] = 1
  } else if (select_27_0$FT273[i] == 0 | select_27_0$FT274[i] == 0
             | select_27_0$FT274[i] == 3 | select_27_0$FT274[i] == 4
             | select_27_0$FT275[i] == 0) {
    #If FT273 = (0, NA) or FT274 = (0, 3, 4, NA) or FT275 = (0, NA) then aspirin_derived_core27 = NA
    select_27_0$aspirin_derived_core27[i] = NA
  } else {
    #Else aspirin_derived_core27 = 2
    select_27_0$aspirin_derived_core27[i] = 2
  }
}

#rename columns
colnames(select_27_0) <- c("id", "aspirin_use_core27", "aspirin_quantity_core27", 
                           "aspirin_freq_core27", "aspirin_dose_core27", "aspirin_purpose_core27")
#replace NA
select_27_0 <- select_27_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 28 ####
#FU010-FU013
exam_28_0 <- read_sas("e_exam_ex28_0_0256.sas7bdat")
select_28_0 <- exam_28_0[,c("ID", "FU010", "FU011", "FU012", "FU013")]

#Placeholder values, used for testing
select_28_0$aspirin_derived_core28 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_28_0)) {
  if (is.na(select_28_0$FU011[i]) | is.na(select_28_0$FU012[i]) | is.na(select_28_0$FU013[i])) {
    #If NA in any column then aspirin_derived_core28 = NA (This has to go first for logic)
    select_28_0$aspirin_derived_core28[i] = NA
  } else if (select_28_0$FU011[i] == 1 & select_28_0$FU012[i] == 1 
             & select_28_0$FU013[i] >= 81 & select_28_0$FU013[i] <= 325) {
    #If FU011 = 1 and FU012 = 1 and FU013 = 81-325 then aspirin_derived_core28 = 1
    select_28_0$aspirin_derived_core28[i] = 1
  } else if (select_28_0$FU011[i] == 2 & select_28_0$FU012[i] == 1 
             & select_28_0$FU013[i] >= 81 & select_28_0$FU013[i] <= 160) {
    #If FU011 = 2 and FU012 = 1 and FU013 = 81-160 then aspirin_derived_core28 = 1
    select_28_0$aspirin_derived_core28[i] = 1
  } else if (select_28_0$FU011[i] >= 3 & select_28_0$FU011[i] <= 4 
             & select_28_0$FU012[i] == 1 & select_28_0$FU013[i] == 81) {
    #If FU011 = 3-4 and FU012 = 1 and FU013 = 81 then aspirin_derived_core28 = 1
    select_28_0$aspirin_derived_core28[i] = 1
  } else if (select_28_0$FU011[i] >= 3 & select_28_0$FU011[i] <= 4
             & select_28_0$FU012[i] == 2 & select_28_0$FU013[i] >= 81 
             & select_28_0$FU013[i] <= 325) {
    #If FU011 = 3-4 and FU012 = 2 and FU013 = 81-325 then aspirin_derived_core28 = 1
    select_28_0$aspirin_derived_core28[i] = 1
  } else if (select_28_0$FU011[i] == 0 | select_28_0$FU012[i] == 0
             | select_28_0$FU012[i] == 3 | select_28_0$FU012[i] == 4
             | select_28_0$FU013[i] == 0) {
    #If FU011 = (0, NA) or FU012 = (0, 3, 4, NA) or FU013 = (0, NA) then aspirin_derived_core28 = NA
    select_28_0$aspirin_derived_core28[i] = NA
  } else {
    #Else aspirin_derived_core28 = 2
    select_28_0$aspirin_derived_core28[i] = 2
  }
}

#rename columns
colnames(select_28_0) <- c("id", "aspirin_use_core28", "aspirin_quantity_core28", 
                           "aspirin_freq_core28", "aspirin_dose_core28", "aspirin_purpose_core28")
#replace NA
select_28_0 <- select_28_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 29 ####
#FV011-FV014
exam_29_0 <- read_sas("e_exam_ex29_0_0210_v2.sas7bdat")
select_29_0 <- exam_29_0[,c("id", "fv011", "fv012", "fv013", "fv014")]

#Placeholder values, used for testing
select_29_0$aspirin_derived_core29 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_29_0)) {
  if (is.na(select_29_0$fv012[i]) | is.na(select_29_0$fv013[i]) | is.na(select_29_0$fv014[i])) {
    #If NA in any column then aspirin_derived_core29 = NA (This has to go first for logic)
    select_29_0$aspirin_derived_core29[i] = NA
  } else if (select_29_0$fv012[i] == 1 & select_29_0$fv013[i] == 1 
             & select_29_0$fv014[i] >= 81 & select_29_0$fv014[i] <= 325) {
    #If FV012 = 1 and FV013 = 1 and FV014 = 81-325 then aspirin_derived_core29 = 1
    select_29_0$aspirin_derived_core29[i] = 1
  } else if (select_29_0$fv012[i] == 2 & select_29_0$fv013[i] == 1 
             & select_29_0$fv014[i] >= 81 & select_29_0$fv014[i] <= 160) {
    #If FV012 = 2 and FV013 = 1 and FV014 = 81-160 then aspirin_derived_core29 = 1
    select_29_0$aspirin_derived_core29[i] = 1
  } else if (select_29_0$fv012[i] >= 3 & select_29_0$fv012[i] <= 4 
             & select_29_0$fv013[i] == 1 & select_29_0$fv014[i] == 81) {
    #If FV012 = 3-4 and FV013 = 1 and FV014 = 81 then aspirin_derived_core29 = 1
    select_29_0$aspirin_derived_core29[i] = 1
  } else if (select_29_0$fv012[i] >= 3 & select_29_0$fv012[i] <= 4
             & select_29_0$fv013[i] == 2 & select_29_0$fv014[i] >= 81 
             & select_29_0$fv014[i] <= 325) {
    #If FV012 = 3-4 and FV013 = 2 and FV014 = 81-325 then aspirin_derived_core29 = 1
    select_29_0$aspirin_derived_core29[i] = 1
  } else if (select_29_0$fv012[i] == 0 | select_29_0$fv013[i] == 0
             | select_29_0$fv013[i] == 3 | select_29_0$fv013[i] == 4
             | select_29_0$fv014[i] == 0) {
    #If FV012 = (0, NA) or FV013 = (0, 3, 4, NA) or FV014 = (0, NA) then aspirin_derived_core29 = NA
    select_29_0$aspirin_derived_core29[i] = NA
  } else {
    #Else aspirin_derived_core29 = 2
    select_29_0$aspirin_derived_core29[i] = 2
  }
}

#rename columns
colnames(select_29_0) <- c("id", "aspirin_use_core29", "aspirin_quantity_core29", 
                           "aspirin_freq_core29", "aspirin_dose_core29", "aspirin_purpose_core29")
#replace NA
select_29_0 <- select_29_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 30 ####
#FW009-FW012
exam_30_0 <- read_sas("e_exam_ex30_0_0274.sas7bdat")
select_30_0 <- exam_30_0[,c("id", "fw009", "fw010", "fw011", "fw012")]

#Placeholder values, used for testing
select_30_0$aspirin_derived_core30 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_30_0)) {
  if (is.na(select_30_0$fw010[i]) | is.na(select_30_0$fw011[i]) | is.na(select_30_0$fw012[i])) {
    #If NA in any column then aspirin_derived_core30 = NA (This has to go first for logic)
    select_30_0$aspirin_derived_core30[i] = NA
  } else if (select_30_0$fw010[i] == 1 & select_30_0$fw011[i] == 1 
             & select_30_0$fw012[i] >= 81 & select_30_0$fw012[i] <= 325) {
    #If FW010 = 1 and FW011 = 1 and FW012 = 81-325 then aspirin_derived_core30 = 1
    select_30_0$aspirin_derived_core30[i] = 1
  } else if (select_30_0$fw010[i] == 2 & select_30_0$fw011[i] == 1 
             & select_30_0$fw012[i] >= 81 & select_30_0$fw012[i] <= 160) {
    #If FW010 = 2 and FW011 = 1 and FW012 = 81-160 then aspirin_derived_core30 = 1
    select_30_0$aspirin_derived_core30[i] = 1
  } else if (select_30_0$fw010[i] >= 3 & select_30_0$fw010[i] <= 4 
             & select_30_0$fw011[i] == 1 & select_30_0$fw012[i] == 81) {
    #If FW010 = 3-4 and FW011 = 1 and FW012 = 81 then aspirin_derived_core30 = 1
    select_30_0$aspirin_derived_core30[i] = 1
  } else if (select_30_0$fw010[i] >= 3 & select_30_0$fw010[i] <= 4
             & select_30_0$fw011[i] == 2 & select_30_0$fw012[i] >= 81 
             & select_30_0$fw012[i] <= 325) {
    #If FW010 = 3-4 and FW011 = 2 and FW012 = 81-325 then aspirin_derived_core30 = 1
    select_30_0$aspirin_derived_core30[i] = 1
  } else if (select_30_0$fw010[i] == 0 | select_30_0$fw011[i] == 0
             | select_30_0$fw011[i] == 3 | select_30_0$fw011[i] == 4
             | select_30_0$fw012[i] == 0) {
    #If FW010 = (0, NA) or FW011 = (0, 3, 4, NA) or FW012 = (0, NA) then aspirin_derived_core30 = NA
    select_30_0$aspirin_derived_core30[i] = NA
  } else {
    #Else aspirin_derived_core30 = 2
    select_30_0$aspirin_derived_core30[i] = 2
  }
}


#rename columns
colnames(select_30_0) <- c("id", "aspirin_use_core30", "aspirin_quantity_core30", 
                           "aspirin_freq_core30", "aspirin_dose_core30", "aspirin_purpose_core30")
#replace NA
select_30_0 <- select_30_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 31 ####
#FX009-FX012
exam_31_0 <- read_sas("e_exam_ex31_0_0738.sas7bdat")
select_31_0 <- exam_31_0[,c("id", "FX009", "FX010", "FX011", "FX012")]

#Placeholder values, used for testing
select_31_0$aspirin_derived_core31 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_31_0)) {
  if (is.na(select_31_0$FX010[i]) | is.na(select_31_0$FX011[i]) | is.na(select_31_0$FX012[i])) {
    #If NA in any column then aspirin_derived_core31 = NA (This has to go first for logic)
    select_31_0$aspirin_derived_core31[i] = NA
  } else if (select_31_0$FX010[i] == 1 & select_31_0$FX011[i] == 1 
             & select_31_0$FX012[i] >= 81 & select_31_0$FX012[i] <= 325) {
    #If FX010 = 1 and FX011 = 1 and FX012 = 81-325 then aspirin_derived_core31 = 1
    select_31_0$aspirin_derived_core31[i] = 1
  } else if (select_31_0$FX010[i] == 2 & select_31_0$FX011[i] == 1 
             & select_31_0$FX012[i] >= 81 & select_31_0$FX012[i] <= 160) {
    #If FX010 = 2 and FX011 = 1 and FX012 = 81-160 then aspirin_derived_core31 = 1
    select_31_0$aspirin_derived_core31[i] = 1
  } else if (select_31_0$FX010[i] == 0 | select_31_0$FX011[i] == 0
             | select_31_0$FX011[i] == 3 | select_31_0$FX011[i] == 4
             | select_31_0$FX012[i] == 0) {
    #If FX010 = (0, NA) or FX011 = (0, 3, 4, NA) or FX012 = (0, NA) then aspirin_derived_core31 = NA
    select_31_0$aspirin_derived_core31[i] = NA
  } else {
    #Else aspirin_derived_core31 = 2
    select_31_0$aspirin_derived_core31[i] = 2
  }
}

#rename columns
colnames(select_31_0) <- c("id", "aspirin_use_core31", "aspirin_quantity_core31", 
                           "aspirin_freq_core31", "aspirin_dose_core31", "aspirin_purpose_core31")
#replace NA
select_31_0 <- select_31_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 32 ####
#FY009-FY012
exam_32_0 <- read_sas("e_exam_ex32_0_0939.sas7bdat")
select_32_0 <- exam_32_0[,c("id", "fy009", "fy010", "fy011", "fy012")]

#Placeholder values, used for testing
select_32_0$aspirin_derived_core32 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_32_0)) {
  if (is.na(select_32_0$fy010[i]) | is.na(select_32_0$fy011[i]) | is.na(select_32_0$fy012[i])) {
    #If NA in any column then aspirin_derived_core32 = NA (This has to go first for logic)
    select_32_0$aspirin_derived_core32[i] = NA
  } else if (select_32_0$fy010[i] == 1 & select_32_0$fy011[i] == 1 
             & select_32_0$fy012[i] >= 81 & select_32_0$fy012[i] <= 325) {
    #If FY010 = 1 and FY011 = 1 and FY012 = 81-325 then aspirin_derived_core32 = 1
    select_32_0$aspirin_derived_core32[i] = 1
  } else if (select_32_0$fy010[i] == 2 & select_32_0$fy011[i] == 1 
             & select_32_0$fy012[i] >= 81 & select_32_0$fy012[i] <= 160) {
    #If FY010 = 2 and FY011 = 1 and FY012 = 81-160 then aspirin_derived_core32 = 1
    select_32_0$aspirin_derived_core32[i] = 1
  } else if (select_32_0$fy010[i] == 0 | select_32_0$fy011[i] == 0
             | select_32_0$fy011[i] == 3 | select_32_0$fy011[i] == 4
             | select_32_0$fy012[i] == 0) {
    #If FY010 = (0, NA) or FY011 = (0, 3, 4, NA) or FY012 = (0, NA) then aspirin_derived_core32 = NA
    select_32_0$aspirin_derived_core32[i] = NA
  } else {
    #Else aspirin_derived_core32 = 2
    select_32_0$aspirin_derived_core32[i] = 2
  }
}

#rename columns
colnames(select_32_0) <- c("id", "aspirin_use_core32", "aspirin_quantity_core32", 
                           "aspirin_freq_core32", "aspirin_dose_core32", "aspirin_purpose_core32")
#replace NA
select_32_0 <- select_32_0 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Final Merge ####
#merge datasets
gen1_list <- list(select_12_0, select_13_0, select_17_0, select_18_0, select_19_0,
                  select_20_0, select_21_0, select_22_0, select_23_0, select_24_0,
                  select_25_0, select_26_0, select_27_0, select_28_0, select_29_0,
                  select_30_0, select_31_0, select_32_0)
gen1_joined <- gen1_list %>% reduce(full_join, by = "id")
#change identifying columns to numeric type
gen1_joined$id <- as.numeric(gen1_joined$id)
#reorder rows
gen1_reordered <- gen1_joined %>% arrange(id)
#add further id columns
gen1_final <- gen1_reordered %>% mutate(idtype = 0, .before = id)
gen1_final <- gen1_final %>% mutate(framid = id, .after = id)
#write final csv
write.csv(gen1_final, file = "aspirin_gen1_full_spreadsheet.csv", row.names = FALSE)


