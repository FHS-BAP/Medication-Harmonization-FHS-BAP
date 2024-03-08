# ******************************************************************************************************************************************
# Introduction to Gen 2 thyroid derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: February 2024
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 2 thyroid function.
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
# vr_meds_ex03_7_0535.sas7bdat (Omni 1 Exam 3)
# vr_meds_ex08_1_0280_v1.sas7bdat (Gen 2 Exam 8)
# vr_meds_ex09_1b_0879.sas7bdat (Gen 2 Exam 9 and Omni 1 Exam 9)
# vr_meds_ex10_1b_1198.sas7bdat (Gen 2 Exam 10 and Omni 1 Exam 5)
# 
# 3)  Individual FHS exam questionnaires:
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
library(readxl) #reading excel files

#Reading ATC list from excel spreadsheet
thyro_xlsx <- read_excel("ATC4_Thyroid.xlsx", sheet = "H03_Thyroid_All")
thyro_atc_list <- thyro_xlsx$`ATC Code`

#### Gen 2 Exam 1 ####
#A67, A81, A82
exam_01_1 <- read_sas("e_exam_ex01_1_0079_v1.sas7bdat")
select_01_1 <- exam_01_1[,c("ID", "A67", "A81", "A82")]
#create idtype and framid columns
select_01_1$IDTYPE <- 1
select_01_1$FRAMID <- (80000 + select_01_1$ID)

#Replacing NA with 9 avoids R treating those values differently
select_01_1$A81 <- select_01_1$A81 %>% replace(is.na(.), 9)
select_01_1$A82 <- select_01_1$A82 %>% replace(is.na(.), 9)

#Placeholder values, used for testing
select_01_1$thyroid_med_core1 = 9999
select_01_1$thyroid_core1 = 9999

#for loop to determine thyroid_med_core1
for (i in 1:nrow(select_01_1)) {
  if (select_01_1$A81[i] == 3 & select_01_1$A82[i] == 6) {
    #If both are 0 then thyroid_med_core1 = 0
    select_01_1$thyroid_med_core1[i] = 0
  } else if (select_01_1$A81[i] == 4 | select_01_1$A81[i] == 5 | 
             select_01_1$A82[i] == 7 ) {
    #If there’s a yes in either then thyroid_med_core1 = 1
    select_01_1$thyroid_med_core1[i] = 1
  } else if (select_01_1$A81[i] == 3 & select_01_1$A82[i] == 9) {
    #Go with definite no in A81
    select_01_1$thyroid_med_core1[i] = 0
  } else if (select_01_1$A81[i] == 9 & select_01_1$A82[i] == 6) {
    #Go with definite no in A82
    select_01_1$thyroid_med_core1[i] = 0
  } else {
    #Else thyroid_med_core1 = NA
    select_01_1$thyroid_med_core1[i] = NA
  }
}

#for loop to determine thyroid_core1
for (i in 1:nrow(select_01_1)) {
  if (is.na(select_01_1$A67[i])) {
    #If A67 = NA then thyroid_core1 = NA
    select_01_1$thyroid_core1[i] = NA
  } else if (select_01_1$A67[i] == 3 & select_01_1$thyroid_med_core1[i] == 1) {
    #If A67 = 3 and thyroid_med_core1 = 1 then thyroid_core1 = 1
    select_01_1$thyroid_core1[i] = 1
  } else {
    #Else thyroid_core1 = 0
    select_01_1$thyroid_core1[i] = 0
  }
}

#remove unneeded columns of original questions
select_01_1 <- select(select_01_1, -c(A67, A81, A82))

#replacement of missing values with .
select_01_1 <- select_01_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#set column names for merging
colnames(select_01_1)[1:3] <- c("id", "idtype", "framid")


#### Gen 2 Exam 2 ####
#B62, B195
exam_02_1 <- read_sas("e_exam_ex02_1_0080_v1.sas7bdat")
select_02_1 <- exam_02_1[,c("ID", "B62", "B195")]
#create idtype and framid columns
select_02_1$IDTYPE <- 1
select_02_1$FRAMID <- (80000 + select_02_1$ID)

#recode maybe answers as NA
select_02_1$B195 <- replace(select_02_1$B195, select_02_1$B195 == 2, NA)

#Replacing NA with 9 avoids R treating those values differently
select_02_1$B62 <- select_02_1$B62 %>% replace(is.na(.), 9)

#Placeholder values, used for testing
select_02_1$thyroid_med_core2 = 9999
select_02_1$thyroid_core2 = 9999

#for loop to determine thyroid_med_core2
for (i in 1:nrow(select_02_1)) {
  if (is.na(select_02_1$B62[i])) {
    #If B62 = NA then thyroid_med_core2 = NA
    select_02_1$thyroid_med_core2[i] = NA
  } else if (select_02_1$B62[i] == 0) {
    #If B62 = 0 then thyroid_med_core2 = 0
    select_02_1$thyroid_med_core2[i] = 0
  } else {
    #Else thyroid_med_core2 = 1
    select_02_1$thyroid_med_core2[i] = 1
  }
}

#thyroid_core2 here is simply the same as B195
select_02_1$thyroid_core2 <- select_02_1$B195

#remove unneeded columns of original questions
select_02_1 <- select(select_02_1, -c(B62, B195))

#replacement of missing values with .
select_02_1 <- select_02_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#set column names for merging
colnames(select_02_1)[1:3] <- c("id", "idtype", "framid")


#### Gen 2 Exam 3 ####
#C28
exam_03_1 <- read_sas("e_exam_ex03_1_0081.sas7bdat")
select_03_1 <- exam_03_1[,c("ID", "C28")]
#create idtype and framid columns
select_03_1$IDTYPE <- 1
select_03_1$FRAMID <- (80000 + select_03_1$ID)

#recode maybe answers as NA
select_03_1$C28 <- replace(select_03_1$C28, select_03_1$C28 == 3, NA)

#Placeholder values, used for testing
select_03_1$thyroid_med_core3 = 9999

#for loop to determine thyroid_med_core3
for (i in 1:nrow(select_03_1)) {
  if (is.na(select_03_1$C28[i])) {
    #If C28 = NA then thyroid_med_core3 = NA
    select_03_1$thyroid_med_core3[i] = NA
  } else if (select_03_1$C28[i] == 0) {
    #If C28 = 0 then thyroid_med_core3 = 0
    select_03_1$thyroid_med_core3[i] = 0
  } else {
    #Else thyroid_med_core3 = 1
    select_03_1$thyroid_med_core3[i] = 1
  }
}

#remove unneeded columns of original questions
select_03_1 <- select(select_03_1, -C28)

#replacement of missing values with .
select_03_1 <- select_03_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#set column names for merging
colnames(select_03_1)[1:3] <- c("id", "idtype", "framid")


#### Gen 2 Exam 4 ####
#D033, D034, D092
exam_04_1 <- read_sas("e_exam_ex04_1_0082.sas7bdat")
select_04_1 <- exam_04_1[,c("ID", "D033", "D034", "D092")]
#create idtype and framid columns
select_04_1$IDTYPE <- 1
select_04_1$FRAMID <- (80000 + select_04_1$ID)

#Replacing NA with 9 avoids R treating those values differently
select_04_1$D033 <- select_04_1$D033 %>% replace(is.na(.), 9)
select_04_1$D034 <- select_04_1$D034 %>% replace(is.na(.), 9)

#Placeholder values, used for testing
select_04_1$thyroid_med_core4 = 9999
select_04_1$thyroid_core4 = 9999

#for loop to determine thyroid_med_core4
for (i in 1:nrow(select_04_1)) {
  if (select_04_1$D033[i] == 0 & select_04_1$D034[i] == 0) {
    #If both are 0 then thyroid_med_core4 = 0
    select_04_1$thyroid_med_core4[i] = 0
  } else if (select_04_1$D033[i] == 1 | select_04_1$D033[i] == 2 | 
             select_04_1$D034[i] == 1 | select_04_1$D034[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core4 = 1
    select_04_1$thyroid_med_core4[i] = 1
  } else if (select_04_1$D033[i] == 0 & select_04_1$D034[i] == 9) {
    #Go with definite 0 in D033
    select_04_1$thyroid_med_core4[i] = 0
  } else if (select_04_1$D033[i] == 9 & select_04_1$D034[i] == 0) {
    #Go with definite 0 in D034
    select_04_1$thyroid_med_core4[i] = 0
  } else if (select_04_1$D033[i] == 0 & select_04_1$D034[i] == 3) {
    #Go with definite 0 in D033
    select_04_1$thyroid_med_core4[i] = 0
  } else if (select_04_1$D033[i] == 3 & select_04_1$D034[i] == 0) {
    #Go with definite 0 in D034
    select_04_1$thyroid_med_core4[i] = 0
  } else {
    #Else thyroid_med_core4 = NA
    select_04_1$thyroid_med_core4[i] = NA
  }
}

#for loop to determine thyroid_core4
for (i in 1:nrow(select_04_1)) {
  if (is.na(select_04_1$D092[i])) {
    #If D092 = NA then thyroid_core4 = NA
    select_04_1$thyroid_core4[i] = NA
  } else if (select_04_1$D092[i] == 0) {
    #If D092 = 0 then thyroid_core4 = 0
    select_04_1$thyroid_core4[i] = 0
  } else {
    #Else thyroid_core4 = 1
    select_04_1$thyroid_core4[i] = 1
  }
}

#remove unneeded columns of original questions
select_04_1 <- select(select_04_1, -c(D033, D034, D092))

#replacement of missing values with .
select_04_1 <- select_04_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#set column names for merging
colnames(select_04_1)[1:3] <- c("id", "idtype", "framid")


#### Gen 2 Exam 5/Omni 1 Exam 1 ####
#E252, E253, E297
#Gen 2 Exam 5
exam_05_1 <- read_sas("e_exam_ex05_1_0083.sas7bdat")
select_05_1 <- exam_05_1[,c("ID", "IDTYPE", "E252", "E253", "E297")]
#create framid column
select_05_1$FRAMID <- (80000 + select_05_1$ID)

#Omni 1 Exam 1
exam_01_7 <- read_sas("e_exam_ex01_7_0020.sas7bdat")
select_01_7 <- exam_01_7[,c("id", "idtype", "e252", "e253", "e297")]
#create framid column
select_01_7$framid <- (700000 + select_01_7$id)

#Combine Exam 1 sections into one data frame
colnames(select_05_1) <- colnames(select_01_7)
select_05_1b <- do.call("rbind", list(select_05_1, select_01_7))

#Replacing NA with 9 avoids R treating those values differently
select_05_1b$e252 <- select_05_1b$e252 %>% replace(is.na(.), 9)
select_05_1b$e253 <- select_05_1b$e253 %>% replace(is.na(.), 9)

#Placeholder values, used for testing
select_05_1b$thyroid_med_core5 = 9999
select_05_1b$thyroid_core5 = 9999

#for loop to determine thyroid_med_core5
for (i in 1:nrow(select_05_1b)) {
  if (select_05_1b$e252[i] == 0 & select_05_1b$e253[i] == 0) {
    #If both are 0 then thyroid_med_core5 = 0
    select_05_1b$thyroid_med_core5[i] = 0
  } else if (select_05_1b$e252[i] == 1 | select_05_1b$e252[i] == 2 | 
             select_05_1b$e253[i] == 1 | select_05_1b$e253[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core5 = 1
    select_05_1b$thyroid_med_core5[i] = 1
  } else if (select_05_1b$e252[i] == 0 & select_05_1b$e253[i] == 9) {
    #Go with definite 0 in e252
    select_05_1b$thyroid_med_core5[i] = 0
  } else if (select_05_1b$e252[i] == 9 & select_05_1b$e253[i] == 0) {
    #Go with definite 0 in e253
    select_05_1b$thyroid_med_core5[i] = 0
  } else if (select_05_1b$e252[i] == 0 & select_05_1b$e253[i] == 3) {
    #Go with definite 0 in e252
    select_05_1b$thyroid_med_core5[i] = 0
  } else if (select_05_1b$e252[i] == 3 & select_05_1b$e253[i] == 0) {
    #Go with definite 0 in e253
    select_05_1b$thyroid_med_core5[i] = 0
  } else {
    #Else thyroid_med_core5 = NA
    select_05_1b$thyroid_med_core5[i] = NA
  }
}

#for loop to determine thyroid_core5
for (i in 1:nrow(select_05_1b)) {
  if (is.na(select_05_1b$e297[i])) {
    #If e297 = NA then thyroid_core5 = NA
    select_05_1b$thyroid_core5[i] = NA
  } else if (select_05_1b$e297[i] == 0) {
    #If e297 = 0 then thyroid_core5 = 0
    select_05_1b$thyroid_core5[i] = 0
  } else {
    #Else thyroid_core5 = 1
    select_05_1b$thyroid_core5[i] = 1
  }
}

#remove unneeded columns of original questions
select_05_1b <- select(select_05_1b, -c(e252, e253, e297))

#replacement of missing values with .
select_05_1b <- select_05_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 6 ####
#F216, F217, F263
exam_06_1 <- read_sas("e_exam_ex06_1_0084.sas7bdat")
select_06_1 <- exam_06_1[,c("ID", "IDTYPE", "F216", "F217", "F263")]
#create framid column
select_06_1$FRAMID <- (80000 + select_06_1$ID)

#Replacing NA with 9 avoids R treating those values differently
select_06_1$F216 <- select_06_1$F216 %>% replace(is.na(.), 9)
select_06_1$F217 <- select_06_1$F217 %>% replace(is.na(.), 9)

#Placeholder values, used for testing
select_06_1$thyroid_med_core6 = 9999
select_06_1$thyroid_core6 = 9999

#for loop to determine thyroid_med_core6
for (i in 1:nrow(select_06_1)) {
  if (select_06_1$F216[i] == 0 & select_06_1$F217[i] == 0) {
    #If both are 0 then thyroid_med_core6 = 0
    select_06_1$thyroid_med_core6[i] = 0
  } else if (select_06_1$F216[i] == 1 | select_06_1$F216[i] == 2 | 
             select_06_1$F217[i] == 1 | select_06_1$F217[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core6 = 1
    select_06_1$thyroid_med_core6[i] = 1
  } else if (select_06_1$F216[i] == 0 & select_06_1$F217[i] == 9) {
    #Go with definite 0 in F216
    select_06_1$thyroid_med_core6[i] = 0
  } else if (select_06_1$F216[i] == 9 & select_06_1$F217[i] == 0) {
    #Go with definite 0 in F217
    select_06_1$thyroid_med_core6[i] = 0
  } else if (select_06_1$F216[i] == 0 & select_06_1$F217[i] == 3) {
    #Go with definite 0 in F216
    select_06_1$thyroid_med_core6[i] = 0
  } else if (select_06_1$F216[i] == 3 & select_06_1$F217[i] == 0) {
    #Go with definite 0 in F217
    select_06_1$thyroid_med_core6[i] = 0
  } else {
    #Else thyroid_med_core6 = NA
    select_06_1$thyroid_med_core6[i] = NA
  }
}

#for loop to determine thyroid_core6
for (i in 1:nrow(select_06_1)) {
  if (is.na(select_06_1$F263[i])) {
    #If F263 = NA then thyroid_core6 = NA
    select_06_1$thyroid_core6[i] = NA
  } else if (select_06_1$F263[i] == 0) {
    #If F263 = 0 then thyroid_core6 = 0
    select_06_1$thyroid_core6[i] = 0
  } else {
    #Else thyroid_core6 = 1
    select_06_1$thyroid_core6[i] = 1
  }
}

#remove unneeded columns of original questions
select_06_1 <- select(select_06_1, -c(F216, F217, F263))

#replacement of missing values with .
select_06_1 <- select_06_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#set column names for merging
colnames(select_06_1)[1:3] <- c("id", "idtype", "framid")


#### Gen 2 Exam 7/Omni 1 Exam 2 ####
#G048, G049, G099
#Gen 2 Exam 7
exam_07_1 <- read_sas("e_exam_ex07_1_0085_v1.sas7bdat")
select_07_1 <- exam_07_1[,c("ID", "IDTYPE", "G048", "G049", "G099")]
#create framid column
select_07_1$FRAMID <- (80000 + select_07_1$ID)

#Omni 1 Exam 2
exam_02_7 <- read_sas("e_exam_ex02_7_0003.sas7bdat")
select_02_7 <- exam_02_7[,c("id", "idtype", "g048", "g049", "g099")]
#create framid column
select_02_7$framid <- (700000 + select_02_7$id)

#Combine Exam 1 sections into one data frame
colnames(select_07_1) <- colnames(select_02_7)
select_07_1b <- do.call("rbind", list(select_07_1, select_02_7))

#Combine Exam 1 sections into one data frame
select_07_1b$g048 <- select_07_1b$g048 %>% replace(is.na(.), 9)
select_07_1b$g049 <- select_07_1b$g049 %>% replace(is.na(.), 9)

#Replacing NA with 9 avoids R treating those values differently
select_07_1b$thyroid_med_core7 = 9999
select_07_1b$thyroid_core7 = 9999

#for loop to determine thyroid_med_core7
for (i in 1:nrow(select_07_1b)) {
  if (select_07_1b$g048[i] == 0 & select_07_1b$g049[i] == 0) {
    #If both are 0 then thyroid_med_core7 = 0
    select_07_1b$thyroid_med_core7[i] = 0
  } else if (select_07_1b$g048[i] == 1 | select_07_1b$g048[i] == 2 | 
             select_07_1b$g049[i] == 1 | select_07_1b$g049[i] == 2) {
    #If there’s a 1 or 2 in either then thyroid_med_core7 = 1
    select_07_1b$thyroid_med_core7[i] = 1
  } else if (select_07_1b$g048[i] == 0 & select_07_1b$g049[i] == 9) {
    #Go with definite 0 in g048
    select_07_1b$thyroid_med_core7[i] = 0
  } else if (select_07_1b$g048[i] == 9 & select_07_1b$g049[i] == 0) {
    #Go with definite 0 in g049
    select_07_1b$thyroid_med_core7[i] = 0
  } else if (select_07_1b$g048[i] == 0 & select_07_1b$g049[i] == 3) {
    #Go with definite 0 in g048
    select_07_1b$thyroid_med_core7[i] = 0
  } else if (select_07_1b$g048[i] == 3 & select_07_1b$g049[i] == 0) {
    #Go with definite 0 in g049
    select_07_1b$thyroid_med_core7[i] = 0
  } else {
    #Else thyroid_med_core7 = NA
    select_07_1b$thyroid_med_core7[i] = NA
  }
}

#for loop to determine thyroid_core7
for (i in 1:nrow(select_07_1b)) {
  if (is.na(select_07_1b$g099[i])) {
    #If g099 = NA then thyroid_core7 = NA
    select_07_1b$thyroid_core7[i] = NA
  } else if (select_07_1b$g099[i] == 0) {
    #If g099 = 0 then thyroid_core7 = 0
    select_07_1b$thyroid_core7[i] = 0
  } else {
    #Else thyroid_core7 = 1
    select_07_1b$thyroid_core7[i] = 1
  }
}

#remove unneeded columns of original questions
select_07_1b <- select(select_07_1b, -c(g048, g049, g099))

#replacement of missing values with .
select_07_1b <- select_07_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 8/Omni 1 Exam 3 ####
#H351
#Gen 2 Exam 8
exam_08_1 <- read_sas("e_exam_ex08_1_0005.sas7bdat")
select_08_1 <- exam_08_1[,c("ID", "IDTYPE", "H351")]
#create framid column
select_08_1$FRAMID <- (80000 + select_08_1$ID)

#Omni 1 Exam 3
exam_03_7 <- read_sas("e_exam_ex03_7_0426.sas7bdat")
select_03_7 <- exam_03_7[,c("id", "idtype", "h351")]
#create framid column
select_03_7$framid <- (700000 + select_03_7$id)

#Combine Exam 1 sections into one data frame
colnames(select_08_1) <- colnames(select_03_7)
select_08_1b <- do.call("rbind", list(select_08_1, select_03_7))

#recode maybe answers as NA
select_08_1b$h351 <- replace(select_08_1b$h351, select_08_1b$h351 == 2, NA)

#medications from Gen 2 Exam 8
meds_08_1 <- read_sas("vr_meds_ex08_1_0280_v1.sas7bdat")
cod_08_1 <- meds_08_1[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
cod_08_1$framid <- (80000 + cod_08_1$id)

#medications from Omni 1 Exam 3
meds_03_7 <- read_sas("vr_meds_ex03_7_0535.sas7bdat")
cod_03_7 <- meds_03_7[,c("ID", "IDTYPE", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
cod_03_7$FRAMID <- (700000 + cod_03_7$ID)

#Combine medication sections into one data frame
colnames(cod_03_7) <- colnames(cod_08_1)
cod_08_1b <- do.call("rbind", list(cod_08_1, cod_03_7))

#Placeholder values, used for testing
select_08_1b$thyroid_core8 = 9999

#for loop to determine thyroid_core8
for (i in 1:nrow(select_08_1b)) {
  if (is.na(select_08_1b$h351[i])) {
    #If h351 = NA then thyroid_core8 = NA
    select_08_1b$thyroid_core8[i] = NA
  } else if (select_08_1b$h351[i] == 0) {
    #If h351 = 0 then thyroid_core8 = 0
    select_08_1b$thyroid_core8[i] = 0
  } else {
    #Else thyroid_core8 = 1
    select_08_1b$thyroid_core8[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_framid_08_1b <- unique(cod_08_1b$framid)
med_found_08_1b <- data.frame(atc_framid_08_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_08_1b['thyroid_med_core8'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% thyro_filtered08_1b$framid)
#prepare for merge
colnames(med_found_08_1b)[1] <- "framid"

#merge
all_08_1b <- merge(select_08_1b, med_found_08_1b, all = TRUE)

#remove unneeded columns of original questions
all_08_1b <- select(all_08_1b, -h351)

#replacement of missing values with .
all_08_1b <- all_08_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 9/Omni 1 Exam 9 ####
#J410
#Gen 2 Exam 9/Omni 1 Exam 9
exam_09_1b <- read_sas("e_exam_ex09_1b_0844.sas7bdat")
select_09_1b <- exam_09_1b[,c("id", "idtype", "j410")]
#create framid column
select_09_1b$framid <- with(select_09_1b, ifelse(idtype == 1, 80000 + id, 
                                                 ifelse(idtype == 7, 700000 + id, id)))

#recode maybe answers as NA
select_09_1b$j410 <- replace(select_09_1b$j410, select_09_1b$j410 == 2, NA)

#medications from Gen 2 Exam 9
meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
cod_09_1b <- meds_09_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#create framid column
cod_09_1b$framid <- with(cod_09_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Placeholder values, used for testing
select_09_1b$thyroid_core9 = 9999

#for loop to determine thyroid_core9
for (i in 1:nrow(select_09_1b)) {
  if (is.na(select_09_1b$j410[i])) {
    #If j410 = NA then thyroid_core9 = NA
    select_09_1b$thyroid_core9[i] = NA
  } else if (select_09_1b$j410[i] == 0) {
    #If j410 = 0 then thyroid_core9 = 0
    select_09_1b$thyroid_core9[i] = 0
  } else {
    #Else thyroid_core9 = 1
    select_09_1b$thyroid_core9[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_framid_09_1b <- unique(cod_09_1b$framid)
med_found_09_1b <- data.frame(atc_framid_09_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_09_1b['thyroid_med_core9'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% thyro_filtered09_1b$framid)
#prepare for merge
colnames(med_found_09_1b)[1] <- "framid"

#merge
all_09_1b <- merge(select_09_1b, med_found_09_1b, all = TRUE)

#remove unneeded columns of original questions
all_09_1b <- select(all_09_1b, -j410)

#replacement of missing values with .
all_09_1b <- all_09_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#### Gen 2 Exam 10/Omni 1 Exam 5 ####
#K0986
#Gen 2 Exam 10/Omni 1 Exam 5
exam_10_1b <- read_sas("e_exam_ex10_1b_1409.sas7bdat")
select_10_1b <- exam_10_1b[,c("id", "idtype", "K0986")]
#create framid column
select_10_1b$framid <- with(select_10_1b, ifelse(idtype == 1, 80000 + id, 
                                                 ifelse(idtype == 7, 700000 + id, id)))

#recode maybe answers as NA
select_10_1b$K0986 <- replace(select_10_1b$K0986, select_10_1b$K0986 == 2, NA)

#medications from Gen 2 Exam 10
meds_10_1b <- read_sas("vr_meds_ex10_1b_1198.sas7bdat")
cod_10_1b <- meds_10_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#create framid column
cod_10_1b$framid <- with(cod_10_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Placeholder values, used for testing
select_10_1b$thyroid_core10 = 9999

#for loop to determine thyroid_core10
for (i in 1:nrow(select_10_1b)) {
  if (is.na(select_10_1b$K0986[i])) {
    #If K0986 = NA then thyroid_core10 = NA
    select_10_1b$thyroid_core10[i] = NA
  } else if (select_10_1b$K0986[i] == 0) {
    #If K0986 = 0 then thyroid_core10 = 0
    select_10_1b$thyroid_core10[i] = 0
  } else {
    #Else thyroid_core10 = 1
    select_10_1b$thyroid_core10[i] = 1
  }
}

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
thyro_filtered10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% thyro_atc_list))
#Constructing the output data frame
atc_framid_10_1b <- unique(cod_10_1b$framid)
med_found_10_1b <- data.frame(atc_framid_10_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_10_1b['thyroid_med_core10'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% thyro_filtered10_1b$framid)
#prepare for merge
colnames(med_found_10_1b)[1] <- "framid"

#merge
all_10_1b <- merge(select_10_1b, med_found_10_1b, all = TRUE)

#remove unneeded columns of original questions
all_10_1b <- select(all_10_1b, -K0986)

#replacement of missing values with .
all_10_1b <- all_10_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Final Merge ####

#list all exam data frames
gen2_all_list <- list(select_01_1, select_02_1, select_03_1, select_04_1, select_05_1b, select_06_1,
                      select_07_1b, all_08_1b, all_09_1b, all_10_1b)
#join all exam data frames together
gen2_joined <- gen2_all_list %>% reduce(full_join, by = c("idtype", "id", "framid"))

#Write final dataset to file
write.csv(gen2_joined, file = "Thyroid_Data_Gen_2_Full_Spreadsheet.csv", row.names = FALSE)


