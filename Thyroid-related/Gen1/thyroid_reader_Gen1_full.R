# ******************************************************************************************************************************************
# Introduction to Gen1 thyroid derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: February 2024
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 1 thyroid function.
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
# vr_meds_ex28_0_0441.sas7bdat (ATC Info Exam 28)
# vr_meds_ex31_0_0763.sas7bdat (ATC Info Exams 29/30/31)
# 
# 3)  Individual FHS exam questionnaires:
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
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code. ;
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files

#Reading ATC list from excel spreadsheet - used for Exams 28 and 29
thyro_xlsx <- read_excel("ATC4_Thyroid.xlsx", sheet = "H03_Thyroid_All")
thyro_atc_list <- thyro_xlsx$`ATC Code`

#Exam 1-7
#Exam 7: MF531, MF532
exam_07 <- read_sas("e_exam_ex07_0_0076_v1.sas7bdat")
select_07 <- exam_07[,c("ID", "MF531", "MF532")]
#Placeholder values, used for testing
select_07$thyroid_core07 = 9999
select_07$thyroid_med_core07 = 9999

for (i in 1:nrow(select_07)) {
  if (is.na(select_07$MF531[i])) {
    #If MF531 = NA then thyroid_core07 = NA
    select_07$thyroid_core07[i] = NA
  } else if (select_07$MF531[i] == 992 | select_07$MF531[i] == 0) {
    #If MF531 = 992 or MF532 = 0 then thyroid_core07 = 0
    select_07$thyroid_core07[i] = 0
  } else {
    #Else thyroid_core07 = 1
    select_07$thyroid_core07[i] = 1
  }
}

for (i in 1:nrow(select_07)) {
  if (is.na(select_07$MF532[i])) {
    #If MF532 = NA then thyroid_med_core07 = NA
    select_07$thyroid_med_core07[i] = NA
  } else if (select_07$MF532[i] == 992 | select_07$MF532[i] == 0 | 
             select_07$MF532[i] == 2 | select_07$MF532[i] == 4 | 
             select_07$MF532[i] == 5) {
    #If MF532 = 1000, 0000, 0002, 0004, or 0005 then thyroid_med_core07 = 0
    select_07$thyroid_med_core07[i] = 0
  } else {
    #Else thyroid_med_core07 = 1
    select_07$thyroid_med_core07[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_07 <- select_07 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 8: FA123
exam_08 <- read_sas("e_exam_ex08_0_0077.sas7bdat")
select_08 <- exam_08[,c("ID", "FA123")]
#Placeholder values, used for testing
select_08$thyroid_med_core08 = 9999

for (i in 1:nrow(select_08)) {
  if (is.na(select_08$FA123[i]) | select_08$FA123[i] == 9) {
    #If FA123 = NA then thyroid_med_core08 = NA
    select_08$thyroid_med_core08[i] = NA
  } else if (select_08$FA123[i] == 0) {
    #If FA123 = 0 then thyroid_med_core08 = 0
    select_08$thyroid_med_core08[i] = 0
  } else {
    #Else thyroid_med_core08 = 1
    select_08$thyroid_med_core08[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_08 <- select_08 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 9: FB71, FB72
exam_09 <- read_sas("e_exam_ex09_0_0078.sas7bdat")
select_09 <- exam_09[,c("ID", "FB71", "FB72")]
select_09 <- select_09 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_09$thyroid_med_core09 = 9999

for (i in 1:nrow(select_09)) {
  if (select_09$FB71[i] == 0 & select_09$FB72[i] == 0) {
    #If both are 0 then thyroid_med_core09 = 0
    select_09$thyroid_med_core09[i] = 0
  } else if (select_09$FB71[i] == 1 | select_09$FB71[i] == 2 | 
             select_09$FB72[i] == 1 | select_09$FB72[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core09 = 1
    select_09$thyroid_med_core09[i] = 1
  } else if (select_09$FB71[i] == 0 & select_09$FB72[i] == 9) {
    #Go with definite 0 in FB71
    select_09$thyroid_med_core09[i] = 0
  } else if (select_09$FB71[i] == 9 & select_09$FB72[i] == 0) {
    #Go with definite 0 in FB72
    select_09$thyroid_med_core09[i] = 0
  } else {
    #Else thyroid_med_core09 = NA
    select_09$thyroid_med_core09[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_09 <- select_09 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 10: FC88, FC89
exam_10 <- read_sas("e_exam_ex10_0_0058.sas7bdat")
select_10 <- exam_10[,c("ID", "FC88", "FC89")]
select_10 <- select_10 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_10$thyroid_med_core10 = 9999

for (i in 1:nrow(select_10)) {
  if (select_10$FC88[i] == 0 & select_10$FC89[i] == 0) {
    #If both are 0 then thyroid_med_core10 = 0
    select_10$thyroid_med_core10[i] = 0
  } else if (select_10$FC88[i] == 1 | select_10$FC88[i] == 2 | 
             select_10$FC89[i] == 1 | select_10$FC89[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core10 = 1
    select_10$thyroid_med_core10[i] = 1
  } else if (select_10$FC88[i] == 0 & select_10$FC89[i] == 9) {
    #Go with definite 0 in FC88
    select_10$thyroid_med_core10[i] = 0
  } else if (select_10$FC88[i] == 9 & select_10$FC89[i] == 0) {
    #Go with definite 0 in FC89
    select_10$thyroid_med_core10[i] = 0
  } else {
    #Else thyroid_med_core10 = NA
    select_10$thyroid_med_core10[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_10 <- select_10 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 11: FD80, FD81
exam_11 <- read_sas("e_exam_ex11_0_0059.sas7bdat")
select_11 <- exam_11[,c("ID", "FD80", "FD81")]
select_11 <- select_11 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_11$thyroid_med_core11 = 9999

for (i in 1:nrow(select_11)) {
  if (select_11$FD80[i] == 0 & select_11$FD81[i] == 0) {
    #If both are 0 then thyroid_med_core11 = 0
    select_11$thyroid_med_core11[i] = 0
  } else if (select_11$FD80[i] == 1 | select_11$FD80[i] == 2 | 
             select_11$FD81[i] == 1 | select_11$FD81[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core11 = 1
    select_11$thyroid_med_core11[i] = 1
  } else if (select_11$FD80[i] == 0 & select_11$FD81[i] == 9) {
    #Go with definite 0 in FD80
    select_11$thyroid_med_core11[i] = 0
  } else if (select_11$FD80[i] == 9 & select_11$FD81[i] == 0) {
    #Go with definite 0 in FD81
    select_11$thyroid_med_core11[i] = 0
  } else {
    #Else thyroid_med_core11 = NA
    select_11$thyroid_med_core11[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_11 <- select_11 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 12: FE91
exam_12 <- read_sas("e_exam_ex12_0_0060_v1.sas7bdat")
select_12 <- exam_12[,c("ID", "FE91")]
#Placeholder values, used for testing
select_12$thyroid_med_core12 = 9999

for (i in 1:nrow(select_12)) {
  if (is.na(select_12$FE91[i])) {
    #If FE91 = NA then thyroid_med_core12 = NA
    select_12$thyroid_med_core12[i] = NA
  } else if (select_12$FE91[i] == 0) {
    #If FE91 = 0 then thyroid_med_core12 = 0
    select_12$thyroid_med_core12[i] = 0
  } else {
    #Else thyroid_med_core12 = 1
    select_12$thyroid_med_core12[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_12 <- select_12 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 13: FF93
exam_13 <- read_sas("e_exam_ex13_0_0061_v1.sas7bdat")
select_13 <- exam_13[,c("ID", "FF93")]
#Placeholder values, used for testing
select_13$thyroid_med_core13 = 9999

for (i in 1:nrow(select_13)) {
  if (is.na(select_13$FF93[i])) {
    #If FF93 = NA then thyroid_med_core13 = NA
    select_13$thyroid_med_core13[i] = NA
  } else if (select_13$FF93[i] == 0) {
    #If FF93 = 0 then thyroid_med_core13 = 0
    select_13$thyroid_med_core13[i] = 0
  } else {
    #Else thyroid_med_core13 = 1
    select_13$thyroid_med_core13[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_13 <- select_13 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 14: FG92
exam_14 <- read_sas("e_exam_ex14_0_0062.sas7bdat")
select_14 <- exam_14[,c("ID", "FG92")]
#Placeholder values, used for testing
select_14$thyroid_med_core14 = 9999

for (i in 1:nrow(select_14)) {
  if (is.na(select_14$FG92[i])) {
    #If FG92 = NA then thyroid_med_core14 = NA
    select_14$thyroid_med_core14[i] = NA
  } else if (select_14$FG92[i] == 0) {
    #If FG92 = 0 then thyroid_med_core14 = 0
    select_14$thyroid_med_core14[i] = 0
  } else {
    #Else thyroid_med_core14 = 1
    select_14$thyroid_med_core14[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_14 <- select_14 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 15: FH92
exam_15 <- read_sas("e_exam_ex15_0_0063.sas7bdat")
select_15 <- exam_15[,c("ID", "FH92")]
#Placeholder values, used for testing
select_15$thyroid_med_core15 = 9999

for (i in 1:nrow(select_15)) {
  if (is.na(select_15$FH92[i])) {
    #If FH92 = NA then thyroid_med_core15 = NA
    select_15$thyroid_med_core15[i] = NA
  } else if (select_15$FH92[i] == 0) {
    #If FH92 = 0 then thyroid_med_core15 = 0
    select_15$thyroid_med_core15[i] = 0
  } else {
    #Else thyroid_med_core15 = 1
    select_15$thyroid_med_core15[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_15 <- select_15 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 16: FI51
exam_16 <- read_sas("e_exam_ex16_0_0064.sas7bdat")
select_16 <- exam_16[,c("ID", "FI51")]
#Placeholder values, used for testing
select_16$thyroid_med_core16 = 9999

for (i in 1:nrow(select_16)) {
  if (is.na(select_16$FI51[i])) {
    #If FI51 = NA then thyroid_med_core16 = NA
    select_16$thyroid_med_core16[i] = NA
  } else if (select_16$FI51[i] == 0) {
    #If FI51 = 0 then thyroid_med_core16 = 0
    select_16$thyroid_med_core16[i] = 0
  } else {
    #Else thyroid_med_core16 = 1
    select_16$thyroid_med_core16[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_16 <- select_16 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 17: FJ25, FJ134
exam_17 <- read_sas("e_exam_ex17_0_0065.sas7bdat")
select_17 <- exam_17[,c("ID", "FJ25", "FJ134")]
#Replacing NA with 9 avoids R treating those values differently
select_17 <- select_17 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_17$thyroid_med_core17 = 9999

for (i in 1:nrow(select_17)) {
  if (select_17$FJ25[i] == 0 & select_17$FJ134[i] == 0) {
    #If both are 0 then thyroid_med_core17 = 0
    select_17$thyroid_med_core17[i] = 0
  } else if (select_17$FJ25[i] == 1 | select_17$FJ25[i] == 2 | 
             select_17$FJ134[i] == 1) {
    #If there’s a 1 or 2 in first or 1 in second then thyroid_med_core17 = 1
    select_17$thyroid_med_core17[i] = 1
  } else if (select_17$FJ25[i] == 0 & select_17$FJ134[i] == 9) {
    #Go with definite 0 in FJ25
    select_17$thyroid_med_core17[i] = 0
  } else if (select_17$FJ25[i] == 9 & select_17$FJ134[i] == 0) {
    #Go with definite 0 in FJ134
    select_17$thyroid_med_core17[i] = 0
  } else if (select_17$FJ25[i] == 0 & select_17$FJ134[i] == 2) {
    #Go with definite 0 in FJ25
    select_17$thyroid_med_core17[i] = 0
  } else if (select_17$FJ25[i] == 3 & select_17$FJ134[i] == 0) {
    #Go with definite 0 in FJ134
    select_17$thyroid_med_core17[i] = 0
  } else {
    #Else thyroid_med_core17 = NA
    select_17$thyroid_med_core17[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_17 <- select_17 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 18: FK106, FK215, and look into FK107
exam_18 <- read_sas("e_exam_ex18_0_0066.sas7bdat")
select_18 <- exam_18[,c("ID", "FK106", "FK215", "FK107")]
#Replacing NA with 9 avoids R treating those values differently
select_18$FK106 <- select_18$FK106 %>% replace(is.na(.), 9)
select_18$FK215 <- select_18$FK215 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_18$thyroid_med_core18 = 9999

for (i in 1:nrow(select_18)) {
  if (select_18$FK106[i] == 0 & select_18$FK215[i] == 0) {
    #If both are 0 then thyroid_med_core18 = 0
    select_18$thyroid_med_core18[i] = 0
  } else if (select_18$FK106[i] == 1 | select_18$FK106[i] == 2 | 
             select_18$FK215[i] == 1) {
    #If there’s a 1 or 2 in first or 1 in second then thyroid_med_core18 = 1
    select_18$thyroid_med_core18[i] = 1
  } else if (select_18$FK106[i] == 0 & select_18$FK215[i] == 9) {
    #Go with definite 0 in FK106
    select_18$thyroid_med_core18[i] = 0
  } else if (select_18$FK106[i] == 9 & select_18$FK215[i] == 0) {
    #Go with definite 0 in FK215
    select_18$thyroid_med_core18[i] = 0
  } else if (select_18$FK106[i] == 0 & select_18$FK215[i] == 2) {
    #Go with definite 0 in FK106
    select_18$thyroid_med_core18[i] = 0
  } else if (select_18$FK106[i] == 3 & select_18$FK215[i] == 0) {
    #Go with definite 0 in FK215
    select_18$thyroid_med_core18[i] = 0
  } else {
    #Else thyroid_med_core18 = NA
    select_18$thyroid_med_core18[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_18 <- select_18 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 19: FL142, FL143
exam_19 <- read_sas("e_exam_ex19_0_0067.sas7bdat")
select_19 <- exam_19[,c("ID", "FL142", "FL143")]
#Replacing NA with 9 avoids R treating those values differently
select_19 <- select_19 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_19$thyroid_med_core19 = 9999

for (i in 1:nrow(select_19)) {
  if (select_19$FL142[i] == 0 & select_19$FL143[i] == 0) {
    #If both are 0 then thyroid_med_core19 = 0
    select_19$thyroid_med_core19[i] = 0
  } else if (select_19$FL142[i] == 1 | select_19$FL142[i] == 2 | 
             select_19$FL143[i] == 1 | select_19$FL143[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core19 = 1
    select_19$thyroid_med_core19[i] = 1
  } else if (select_19$FL142[i] == 0 & select_19$FL143[i] == 9) {
    #Go with definite 0 in FL142
    select_19$thyroid_med_core19[i] = 0
  } else if (select_19$FL142[i] == 9 & select_19$FL143[i] == 0) {
    #Go with definite 0 in FL143
    select_19$thyroid_med_core19[i] = 0
  } else if (select_19$FL142[i] == 0 & select_19$FL143[i] == 3) {
    #Go with definite 0 in FL142
    select_19$thyroid_med_core19[i] = 0
  } else if (select_19$FL142[i] == 3 & select_19$FL143[i] == 0) {
    #Go with definite 0 in FL143
    select_19$thyroid_med_core19[i] = 0
  } else {
    #Else thyroid_med_core19 = NA
    select_19$thyroid_med_core19[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_19 <- select_19 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 20: FM180, FM181, FM230, FM231
exam_20 <- read_sas("e_exam_ex20_0_0068.sas7bdat")
select_20 <- exam_20[,c("ID", "FM180", "FM181", "FM230", "FM231")]
#Replacing NA with 9 avoids R treating those values differently
select_20 <- select_20 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_20$thyroid_med_core20 = 9999
select_20$thyroid_core20 = 9999

for (i in 1:nrow(select_20)) {
  if (select_20$FM180[i] == 0 & select_20$FM181[i] == 0) {
    #If both are 0 then thyroid_med_core20 = 0
    select_20$thyroid_med_core20[i] = 0
  } else if (select_20$FM180[i] == 1 | select_20$FM180[i] == 2 | 
             select_20$FM181[i] == 1 | select_20$FM181[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core20 = 1
    select_20$thyroid_med_core20[i] = 1
  } else if (select_20$FM180[i] == 0 & select_20$FM181[i] == 9) {
    #Go with definite 0 in FM180
    select_20$thyroid_med_core20[i] = 0
  } else if (select_20$FM180[i] == 9 & select_20$FM181[i] == 0) {
    #Go with definite 0 in FM181
    select_20$thyroid_med_core20[i] = 0
  } else if (select_20$FM180[i] == 0 & select_20$FM181[i] == 3) {
    #Go with definite 0 in FM180
    select_20$thyroid_med_core20[i] = 0
  } else if (select_20$FM180[i] == 3 & select_20$FM181[i] == 0) {
    #Go with definite 0 in FM181
    select_20$thyroid_med_core20[i] = 0
  } else {
    #Else thyroid_med_core20 = NA
    select_20$thyroid_med_core20[i] = NA
  }
}

for (i in 1:nrow(select_20)) {
  if (select_20$FM230[i] == 0 & select_20$FM231[i] == 0) {
    #If both are 0 then thyroid_core20 = 0
    select_20$thyroid_core20[i] = 0
  } else if (select_20$FM230[i] == 1 | select_20$FM231[i] == 1) {
    #If 1 in either then thyroid_core20 = 1
    select_20$thyroid_core20[i] = 1
  } else if (select_20$FM230[i] == 0 & select_20$FM231[i] == 9) {
    #Go with definite 0 in FM230
    select_20$thyroid_core20[i] = 0
  } else if (select_20$FM230[i] == 9 & select_20$FM231[i] == 0) {
    #Go with definite 0 in FM231
    select_20$thyroid_core20[i] = 0
  } else {
    #Else thyroid_core20 = NA
    select_20$thyroid_core20[i] = NA
  }
}
#replacement of missing values for upcoming merge
select_20 <- select_20 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 21: FN119, FN120, FN184
exam_21 <- read_sas("e_exam_ex21_0_0069.sas7bdat")
select_21 <- exam_21[,c("ID", "FN119", "FN120", "FN184")]
#Replacing NA with 9 avoids R treating those values differently
select_21$FN119 <- select_21$FN119 %>% replace(is.na(.), 9)
select_21$FN120 <- select_21$FN120 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_21$thyroid_med_core21 = 9999
select_21$thyroid_core21 = 9999

for (i in 1:nrow(select_21)) {
  if (select_21$FN119[i] == 0 & select_21$FN120[i] == 0) {
    #If both are 0 then thyroid_med_core21 = 0
    select_21$thyroid_med_core21[i] = 0
  } else if (select_21$FN119[i] == 1 | select_21$FN119[i] == 2 | 
             select_21$FN120[i] == 1 | select_21$FN120[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core21 = 1
    select_21$thyroid_med_core21[i] = 1
  } else if (select_21$FN119[i] == 0 & select_21$FN120[i] == 9) {
    #Go with definite 0 in FN119
    select_21$thyroid_med_core21[i] = 0
  } else if (select_21$FN119[i] == 9 & select_21$FN120[i] == 0) {
    #Go with definite 0 in FN120
    select_21$thyroid_med_core21[i] = 0
  } else if (select_21$FN119[i] == 0 & select_21$FN120[i] == 3) {
    #Go with definite 0 in FN119
    select_21$thyroid_med_core21[i] = 0
  } else if (select_21$FN119[i] == 3 & select_21$FN120[i] == 0) {
    #Go with definite 0 in FN120
    select_21$thyroid_med_core21[i] = 0
  } else {
    #Else thyroid_med_core21 = NA
    select_21$thyroid_med_core21[i] = NA
  }
}

for (i in 1:nrow(select_21)) {
  if (is.na(select_21$FN184[i])) {
    #If FN184 = NA then thyroid_core21 = NA
    select_21$thyroid_core21[i] = NA
  } else if (select_21$FN184[i] == 0) {
    #If FN184 = 0 then thyroid_core21 = 0
    select_21$thyroid_core21[i] = 0
  } else {
    #Else thyroid_core21 = 1
    select_21$thyroid_core21[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_21 <- select_21 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 22: FO126, FO127, FO162
exam_22 <- read_sas("e_exam_ex22_0_0070.sas7bdat")
#Replacing NA with 9 avoids R treating those values differently
select_22 <- exam_22[,c("ID", "FO126", "FO127", "FO162")]
#Placeholder values, used for testing
select_22$FO126 <- select_22$FO126 %>% replace(is.na(.), 9)
select_22$FO127 <- select_22$FO127 %>% replace(is.na(.), 9)

select_22$thyroid_med_core22 = 9999
select_22$thyroid_core22 = 9999

for (i in 1:nrow(select_22)) {
  if (select_22$FO126[i] == 0 & select_22$FO127[i] == 0) {
    #If both are 0 then thyroid_med_core22 = 0
    select_22$thyroid_med_core22[i] = 0
  } else if (select_22$FO126[i] == 1 | select_22$FO126[i] == 2 | 
             select_22$FO127[i] == 1 | select_22$FO127[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core22 = 1
    select_22$thyroid_med_core22[i] = 1
  } else if (select_22$FO126[i] == 0 & select_22$FO127[i] == 9) {
    #Go with definite 0 in FO126
    select_22$thyroid_med_core22[i] = 0
  } else if (select_22$FO126[i] == 9 & select_22$FO127[i] == 0) {
    #Go with definite 0 in FO127
    select_22$thyroid_med_core22[i] = 0
  } else if (select_22$FO126[i] == 0 & select_22$FO127[i] == 3) {
    #Go with definite 0 in FO126
    select_22$thyroid_med_core22[i] = 0
  } else if (select_22$FO126[i] == 3 & select_22$FO127[i] == 0) {
    #Go with definite 0 in FO127
    select_22$thyroid_med_core22[i] = 0
  } else {
    #Else thyroid_med_core22 = NA
    select_22$thyroid_med_core22[i] = NA
  }
}

for (i in 1:nrow(select_22)) {
  if (is.na(select_22$FO162[i])) {
    #If FO162 = NA then thyroid_core22 = NA
    select_22$thyroid_core22[i] = NA
  } else if (select_22$FO162[i] == 0) {
    #If FO162 = 0 then thyroid_core22 = 0
    select_22$thyroid_core22[i] = 0
  } else {
    #Else thyroid_core22 = 1
    select_22$thyroid_core22[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_22 <- select_22 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 23: FP080, FP081, FP102
exam_23 <- read_sas("e_exam_ex23_0_0071.sas7bdat")
select_23 <- exam_23[,c("ID", "FP080", "FP081", "FP102")]
#Replacing NA with 9 avoids R treating those values differently
select_23$FP080 <- select_23$FP080 %>% replace(is.na(.), 9)
select_23$FP081 <- select_23$FP081 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_23$thyroid_med_core23 = 9999
select_23$thyroid_core23 = 9999

for (i in 1:nrow(select_23)) {
  if (select_23$FP080[i] == 0 & select_23$FP081[i] == 0) {
    #If both are 0 then thyroid_med_core23 = 0
    select_23$thyroid_med_core23[i] = 0
  } else if (select_23$FP080[i] == 1 | select_23$FP080[i] == 2 | 
             select_23$FP081[i] == 1 | select_23$FP081[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core23 = 1
    select_23$thyroid_med_core23[i] = 1
  } else if (select_23$FP080[i] == 0 & select_23$FP081[i] == 9) {
    #Go with definite 0 in FP080
    select_23$thyroid_med_core23[i] = 0
  } else if (select_23$FP080[i] == 9 & select_23$FP081[i] == 0) {
    #Go with definite 0 in FP081
    select_23$thyroid_med_core23[i] = 0
  } else if (select_23$FP080[i] == 0 & select_23$FP081[i] == 3) {
    #Go with definite 0 in FP080
    select_23$thyroid_med_core23[i] = 0
  } else if (select_23$FP080[i] == 3 & select_23$FP081[i] == 0) {
    #Go with definite 0 in FP081
    select_23$thyroid_med_core23[i] = 0
  } else {
    #Else thyroid_med_core23 = NA
    select_23$thyroid_med_core23[i] = NA
  }
}

for (i in 1:nrow(select_23)) {
  if (is.na(select_23$FP102[i])) {
    #If FP102 = NA then thyroid_core23 = NA
    select_23$thyroid_core23[i] = NA
  } else if (select_23$FP102[i] == 0) {
    #If FP102 = 0 then thyroid_core23 = 0
    select_23$thyroid_core23[i] = 0
  } else if (select_23$FP102[i] == 2) {
    #If FP102 = 2 then thyroid_core23 = NA
    select_23$thyroid_core23[i] = NA
  } else {
    #Else thyroid_core23 = 1
    select_23$thyroid_core23[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_23 <- select_23 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 24: FQ165, FQ166, FQ193
exam_24 <- read_sas("e_exam_ex24_0_0072.sas7bdat")
select_24 <- exam_24[,c("ID", "FQ165", "FQ166", "FQ193")]
#Replacing NA with 9 avoids R treating those values differently
select_24$FQ165 <- select_24$FQ165 %>% replace(is.na(.), 9)
select_24$FQ166 <- select_24$FQ166 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_24$thyroid_med_core24 = 9999
select_24$thyroid_core24 = 9999

for (i in 1:nrow(select_24)) {
  if (select_24$FQ165[i] == 0 & select_24$FQ166[i] == 0) {
    #If both are 0 then thyroid_med_core24 = 0
    select_24$thyroid_med_core24[i] = 0
  } else if (select_24$FQ165[i] == 1 | select_24$FQ165[i] == 2 | 
             select_24$FQ166[i] == 1 | select_24$FQ166[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core24 = 1
    select_24$thyroid_med_core24[i] = 1
  } else if (select_24$FQ165[i] == 0 & select_24$FQ166[i] == 9) {
    #Go with definite 0 in FQ165
    select_24$thyroid_med_core24[i] = 0
  } else if (select_24$FQ165[i] == 9 & select_24$FQ166[i] == 0) {
    #Go with definite 0 in FQ166
    select_24$thyroid_med_core24[i] = 0
  } else if (select_24$FQ165[i] == 0 & select_24$FQ166[i] == 3) {
    #Go with definite 0 in FQ165
    select_24$thyroid_med_core24[i] = 0
  } else if (select_24$FQ165[i] == 3 & select_24$FQ166[i] == 0) {
    #Go with definite 0 in FQ166
    select_24$thyroid_med_core24[i] = 0
  } else {
    #Else thyroid_med_core24 = NA
    select_24$thyroid_med_core24[i] = NA
  }
}

for (i in 1:nrow(select_24)) {
  if (is.na(select_24$FQ193[i])) {
    #If FQ193 = NA then thyroid_core24 = NA
    select_24$thyroid_core24[i] = NA
  } else if (select_24$FQ193[i] == 0) {
    #If FQ193 = 0 then thyroid_core24 = 0
    select_24$thyroid_core24[i] = 0
  } else {
    #Else thyroid_core24 = 1
    select_24$thyroid_core24[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_24 <- select_24 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 25: FR217, FR218, FR251
exam_25 <- read_sas("e_exam_ex25_0_0073.sas7bdat")
select_25 <- exam_25[,c("ID", "FR217", "FR218", "FR251")]
#Replacing NA with 9 avoids R treating those values differently
select_25$FR217 <- select_25$FR217 %>% replace(is.na(.), 9)
select_25$FR218 <- select_25$FR218 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_25$thyroid_med_core25 = 9999
select_25$thyroid_core25 = 9999

for (i in 1:nrow(select_25)) {
  if (select_25$FR217[i] == 0 & select_25$FR218[i] == 0) {
    #If both are 0 then thyroid_med_core25 = 0
    select_25$thyroid_med_core25[i] = 0
  } else if (select_25$FR217[i] == 1 | select_25$FR217[i] == 2 | 
             select_25$FR218[i] == 1 | select_25$FR218[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core25 = 1
    select_25$thyroid_med_core25[i] = 1
  } else if (select_25$FR217[i] == 0 & select_25$FR218[i] == 9) {
    #Go with definite 0 in FR217
    select_25$thyroid_med_core25[i] = 0
  } else if (select_25$FR217[i] == 9 & select_25$FR218[i] == 0) {
    #Go with definite 0 in FR218
    select_25$thyroid_med_core25[i] = 0
  } else if (select_25$FR217[i] == 0 & select_25$FR218[i] == 3) {
    #Go with definite 0 in FR217
    select_25$thyroid_med_core25[i] = 0
  } else if (select_25$FR217[i] == 3 & select_25$FR218[i] == 0) {
    #Go with definite 0 in FR218
    select_25$thyroid_med_core25[i] = 0
  } else {
    #Else thyroid_med_core25 = NA
    select_25$thyroid_med_core25[i] = NA
  }
}

for (i in 1:nrow(select_25)) {
  if (is.na(select_25$FR251[i])) {
    #If FR251 = NA then thyroid_core25 = NA
    select_25$thyroid_core25[i] = NA
  } else if (select_25$FR251[i] == 0) {
    #If FR251 = 0 then thyroid_core25 = 0
    select_25$thyroid_core25[i] = 0
  } else {
    #Else thyroid_core25 = 1
    select_25$thyroid_core25[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_25 <- select_25 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 26: FS276, FS277, FS324
exam_26 <- read_sas("e_exam_ex26_0_0074.sas7bdat")
select_26 <- exam_26[,c("ID", "FS276", "FS277", "FS324")]
#Replacing NA with 9 avoids R treating those values differently
select_26$FS276 <- select_26$FS276 %>% replace(is.na(.), 9)
select_26$FS277 <- select_26$FS277 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_26$thyroid_med_core26 = 9999
select_26$thyroid_core26 = 9999

for (i in 1:nrow(select_26)) {
  if (select_26$FS276[i] == 0 & select_26$FS277[i] == 0) {
    #If both are 0 then thyroid_med_core26 = 0
    select_26$thyroid_med_core26[i] = 0
  } else if (select_26$FS276[i] == 1 | select_26$FS276[i] == 2 | 
             select_26$FS277[i] == 1 | select_26$FS277[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core26 = 1
    select_26$thyroid_med_core26[i] = 1
  } else if (select_26$FS276[i] == 0 & select_26$FS277[i] == 9) {
    #Go with definite 0 in FS276
    select_26$thyroid_med_core26[i] = 0
  } else if (select_26$FS276[i] == 9 & select_26$FS277[i] == 0) {
    #Go with definite 0 in FS277
    select_26$thyroid_med_core26[i] = 0
  } else if (select_26$FS276[i] == 0 & select_26$FS277[i] == 3) {
    #Go with definite 0 in FS276
    select_26$thyroid_med_core26[i] = 0
  } else if (select_26$FS276[i] == 3 & select_26$FS277[i] == 0) {
    #Go with definite 0 in FS277
    select_26$thyroid_med_core26[i] = 0
  } else {
    #Else thyroid_med_core26 = NA
    select_26$thyroid_med_core26[i] = NA
  }
}

for (i in 1:nrow(select_26)) {
  if (is.na(select_26$FS324[i])) {
    #If FS324 = NA then thyroid_core26 = NA
    select_26$thyroid_core26[i] = NA
  } else if (select_26$FS324[i] == 0) {
    #If FS324 = 0 then thyroid_core26 = 0
    select_26$thyroid_core26[i] = 0
  } else {
    #Else thyroid_core26 = 1
    select_26$thyroid_core26[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_26 <- select_26 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 27: FT283, FT284, FT333
exam_27 <- read_sas("e_exam_ex27_0_0075.sas7bdat")
select_27 <- exam_27[,c("ID", "FT283", "FT284", "FT333")]
#Replacing NA with 9 avoids R treating those values differently
select_27$FT283 <- select_27$FT283 %>% replace(is.na(.), 9)
select_27$FT284 <- select_27$FT284 %>% replace(is.na(.), 9)
#Placeholder values, used for testing
select_27$thyroid_med_core27 = 9999
select_27$thyroid_core27 = 9999

for (i in 1:nrow(select_27)) {
  if (select_27$FT283[i] == 0 & select_27$FT284[i] == 0) {
    #If both are 0 then thyroid_med_core27 = 0
    select_27$thyroid_med_core27[i] = 0
  } else if (select_27$FT283[i] == 1 | select_27$FT283[i] == 2 | 
             select_27$FT284[i] == 1 | select_27$FT284[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core27 = 1
    select_27$thyroid_med_core27[i] = 1
  } else if (select_27$FT283[i] == 0 & select_27$FT284[i] == 9) {
    #Go with definite 0 in FT283
    select_27$thyroid_med_core27[i] = 0
  } else if (select_27$FT283[i] == 9 & select_27$FT284[i] == 0) {
    #Go with definite 0 in FT284
    select_27$thyroid_med_core27[i] = 0
  } else if (select_27$FT283[i] == 0 & select_27$FT284[i] == 3) {
    #Go with definite 0 in FT283
    select_27$thyroid_med_core27[i] = 0
  } else if (select_27$FT283[i] == 3 & select_27$FT284[i] == 0) {
    #Go with definite 0 in FT284
    select_27$thyroid_med_core27[i] = 0
  } else {
    #Else thyroid_med_core27 = NA
    select_27$thyroid_med_core27[i] = NA
  }
}

for (i in 1:nrow(select_27)) {
  if (is.na(select_27$FT333[i])) {
    #If FT333 = NA then thyroid_core27 = NA
    select_27$thyroid_core27[i] = NA
  } else if (select_27$FT333[i] == 0) {
    #If FT333 = 0 then thyroid_core27 = 0
    select_27$thyroid_core27[i] = 0
  } else {
    #Else thyroid_core27 = 1
    select_27$thyroid_core27[i] = 1
  }
}
#replacement of missing values for upcoming merge
select_27 <- select_27 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 28: FU034
exam_28 <- read_sas("e_exam_ex28_0_0256.sas7bdat")
select_28 <- exam_28[,c("ID", "FU034")]
#Placeholder values, used for testing
select_28$thyroid_core28 = 9999

#medications from exam 28
meds_28 <- read_sas("vr_meds_ex28_0_0441.sas7bdat")
cod_28 <- meds_28[,c("ID", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]


for (i in 1:nrow(select_28)) {
  if (is.na(select_28$FU034[i])) {
    #If FU034 = NA then thyroid_core28 = NA
    select_28$thyroid_core28[i] = NA
  } else if (select_28$FU034[i] == 0) {
    #If FU034 = 0 then thyroid_core28 = 0
    select_28$thyroid_core28[i] = 0
  } else {
    #Else thyroid_core28 = 1
    select_28$thyroid_core28[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered28 <- cod_28 %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_ID_28 <- unique(cod_28$ID)
med_found28 <- data.frame(atc_ID_28)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found28['thyroid_med_core28'] <- as.integer(med_found28$atc_ID_28 %in% thyro_filtered28$ID)
#prepare for merge
colnames(med_found28)[1] <- "ID"

#merge
all_28 <- merge(select_28, med_found28, all = TRUE)

#replacement of missing values for upcoming merge
all_28 <- all_28 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#Exam 29: FV025
exam_29 <- read_sas("e_exam_ex29_0_0210_v2.sas7bdat")
select_29 <- exam_29[,c("id", "fv025")]
#Placeholder values, used for testing
select_29$thyroid_core29 = 9999

#medications from exam 29
#split 29/30/31 medications file
meds_29_30_31 <- read_sas("vr_meds_ex31_0_0763.sas7bdat")
meds_29 <- subset(meds_29_30_31, exam == 29)
cod_29 <- meds_29[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]


for (i in 1:nrow(select_29)) {
  if (is.na(select_29$fv025[i])) {
    #If fv025 = NA then thyroid_core29 = NA
    select_29$thyroid_core29[i] = NA
  } else if (select_29$fv025[i] == 0) {
    #If fv025 = 0 then thyroid_core29 = 0
    select_29$thyroid_core29[i] = 0
  } else {
    #Else thyroid_core29 = 1
    select_29$thyroid_core29[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered29 <- cod_29 %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_ID_29 <- unique(cod_29$id)
med_found29 <- data.frame(atc_ID_29)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found29['thyroid_med_core29'] <- as.integer(med_found29$atc_ID_29 %in% thyro_filtered29$id)
#prepare for merge
colnames(select_29)[1] <- "ID"
colnames(med_found29)[1] <- "ID"

#merge
all_29 <- merge(select_29, med_found29, all = TRUE)

#replacement of missing values for upcoming merge
all_29 <- all_29 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#list all exam data frames
gen1_select_list <- list(select_07, select_08, select_09, select_10, select_11, select_12, select_13,
                         select_14, select_15, select_16, select_17, select_18, select_19, select_20,
                         select_21, select_22, select_23, select_24, select_25, select_26, select_27,
                         all_28, all_29)
#join all exam data frames together
gen1_joined <- gen1_select_list %>% reduce(full_join, by = "ID")
#remove unneeded columns of original questions
gen1_final <- select(gen1_joined, -c(MF531, MF532, FA123, FB71, FB72, FC88, FC89, FD80, FD81, FE91,
                                     FF93, FG92, FH92, FI51, FJ25, FJ134, FK106, FK215, FK107, FL142,
                                     FL143, FM180, FM181, FM230, FM231, FN119, FN120, FN184, FO126,
                                     FO127, FO162, FP080, FP081, FP102, FQ165, FQ166, FQ193, FR217,
                                     FR218, FR251, FS276, FS277, FS324, FT283, FT284, FT333, FU034,
                                     fv025))
#add further id columns
gen1_final <- gen1_final %>% mutate(IDTYPE = 0, .before = ID)
gen1_final <- gen1_final %>% mutate(FRAMID = ID, .after = ID)
#Write final dataset to file
write.csv(gen1_final, file = "Thyroid_Data_Gen_1_Full_Spreadsheet.csv", row.names = FALSE)

