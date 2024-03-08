# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 hormones derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: January 2023
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 2/Omni 1 hormones usage.
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
library(readxl)

#Reading ATC list from excel spreadsheet
hormone_codes <- read_excel("ATC2_HormonesCodes_ALL_20231012.xlsx", sheet = "Hormones_ALL")
hormone_atc_list <- hormone_codes$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`

#### Gen 2 Exam 1 ####

#Hormone questions: A94
#Menopause questions: A88

exam_01_1 <- read_sas("e_exam_ex01_1_0079_v1.sas7bdat")
select_01_1 <- exam_01_1[,c("ID", "A88", "A94")]
#create framid column
colnames(select_01_1)[1] <- "id"
select_01_1$idtype <- 1
select_01_1$framid <- (80000 + select_01_1$id)

#Replacing NA with 10 avoids R treating those values differently
all_01_1 <- select_01_1 %>% mutate_at(c("A88", "A94"), ~replace_na(.,10))

#Placeholder values, used for testing
all_01_1$hormones_data1 = 9999
all_01_1$hormones_core1 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_01_1)) {
  if (all_01_1$A94[i] %in% c(4,5)) {
    #A94 in (1,2) then 1
    all_01_1$hormones_data1[i] = 1
  } else if (all_01_1$A94[i] == 3) {
    #A94 = 0 then 0
    all_01_1$hormones_data1[i] = 0
  } else if (all_01_1$A94[i] %in% c(10,8)) {
    #A94 in (.,8) then hormones_core16 = NA
    all_01_1$hormones_data1[i] = NA
  } else {
    #Else NA
    all_01_1$hormones_data1[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_01_1)) {
  if (is.na(all_01_1$hormones_data1[i])) {
    #NA remains NA
    all_01_1$hormones_core1[i] = NA
  } else if (all_01_1$A88[i] == 0 & all_01_1$hormones_data1[i] == 1) {
    #taking hormones though not in menopause
    all_01_1$hormones_core1[i] = 2
  } else {
    #Else same value as before
    all_01_1$hormones_core1[i] = all_01_1$hormones_data1[i]
  }
}


#### Gen 2 Exam 2 ####

#Hormone questions: B81
#Menopause questions: B70

exam_02_1 <- read_sas("e_exam_ex02_1_0080_v1.sas7bdat")
select_02_1 <- exam_02_1[,c("ID", "B70", "B81")]
#create framid column
colnames(select_02_1)[1] <- "id"
select_02_1$idtype <- 1
select_02_1$framid <- (80000 + select_02_1$id)

#Replacing NA with 10 avoids R treating those values differently
all_02_1 <- select_02_1 %>% mutate_at(c("B70", "B81"), ~replace_na(.,10))

#Placeholder values, used for testing
all_02_1$hormones_data2 = 9999
all_02_1$hormones_core2 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_02_1)) {
  if (all_02_1$B81[i] %in% c(1,2)) {
    #B81 in (1,2) then 1
    all_02_1$hormones_data2[i] = 1
  } else if (all_02_1$B81[i] == 0) {
    #B81 = 0 then 0
    all_02_1$hormones_data2[i] = 0
  } else if (all_02_1$B81[i] %in% c(10,8)) {
    #B81 in (.,8) then hormones_core26 = NA
    all_02_1$hormones_data2[i] = NA
  } else {
    #Else NA
    all_02_1$hormones_data2[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_02_1)) {
  if (is.na(all_02_1$hormones_data2[i])) {
    #NA remains NA
    all_02_1$hormones_core2[i] = NA
  } else if (all_02_1$B70[i] == 0 & all_02_1$hormones_data2[i] == 1) {
    #taking hormones though not in menopause
    all_02_1$hormones_core2[i] = 2
  } else {
    #Else same value as before
    all_02_1$hormones_core2[i] = all_02_1$hormones_data2[i]
  }
}


#### Gen 2 Exam 3 ####

#Hormone questions: C31, C55
#Menopause questions: C47

exam_03_1 <- read_sas("e_exam_ex03_1_0081.sas7bdat")
select_03_1 <- exam_03_1[,c("ID", "C47", "C31", "C55")]
#create framid column
colnames(select_03_1)[1] <- "id"
select_03_1$idtype <- 1
select_03_1$framid <- (80000 + select_03_1$id)

#Replacing NA with 10 avoids R treating those values differently
all_03_1 <- select_03_1 %>% mutate_at(c("C47", "C31", "C55"), ~replace_na(.,10))

#Placeholder values, used for testing
all_03_1$hormones_data3 = 9999
all_03_1$hormones_core3 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_03_1)) {
  if (all_03_1$C31[i] %in% c(1,2) | all_03_1$C55[i] %in% c(1,2)) {
    #C31 in (1,2) or C55 in (1,2) then 1
    all_03_1$hormones_data3[i] = 1
  } else if (all_03_1$C31[i] %in% c(0,3) & all_03_1$C55[i] == 0) {
    #C31 in (0,3) and C55 = 0 then 0
    all_03_1$hormones_data3[i] = 0
  } else if (all_03_1$C31[i] %in% c(10,8) & all_03_1$C55[i] %in% c(10,8)) {
    #C31 in (.,8) and C55 in (.,8) then hormones_core26 = NA
    all_03_1$hormones_data3[i] = NA
  } else {
    #Else NA
    all_03_1$hormones_data3[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_03_1)) {
  if (is.na(all_03_1$hormones_data3[i])) {
    #NA remains NA
    all_03_1$hormones_core3[i] = NA
  } else if (all_03_1$C47[i] == 0 & all_03_1$hormones_data3[i] == 1) {
    #taking hormones though not in menopause
    all_03_1$hormones_core3[i] = 2
  } else {
    #Else same value as before
    all_03_1$hormones_core3[i] = all_03_1$hormones_data3[i]
  }
}


#### Gen 2 Exam 4 ####

#Hormone questions: D038, D061, D064, D065
#Menopause questions: D053

exam_04_1 <- read_sas("e_exam_ex04_1_0082.sas7bdat")
select_04_1 <- exam_04_1[,c("ID", "D053", "D038", "D061", "D064", "D065")]
#create framid column
colnames(select_04_1)[1] <- "id"
select_04_1$idtype <- 1
select_04_1$framid <- (80000 + select_04_1$id)

#Replacing NA with 10 avoids R treating those values differently
all_04_1 <- select_04_1 %>% mutate_at(c("D053", "D038", "D061", "D064", "D065"), ~replace_na(.,10))

#Placeholder values, used for testing
all_04_1$hormones_data4 = 9999
all_04_1$hormones_core4 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_04_1)) {
  if (all_04_1$D038[i] %in% c(1,2) | all_04_1$D061[i] %in% c(1,2) 
      | all_04_1$D064[i] %in% c(1,2) | all_04_1$D065[i] %in% c(1,2)) {
    #D038 in (1,2) or D061 in (1,2) or D064 in (1,2) or D065 in (1,2) then 1
    all_04_1$hormones_data4[i] = 1
  } else if (all_04_1$D038[i] %in% c(0,3) & all_04_1$D061[i] == 0 
             & all_04_1$D064[i] == 0 & all_04_1$D065[i] == 0) {
    #D038 in (0,3) and D061 = 0 and D064 = 0 and D065 = 0 then 0
    all_04_1$hormones_data4[i] = 0
  } else if (all_04_1$D038[i] %in% c(10,8) & all_04_1$D061[i] %in% c(10,8) 
             & all_04_1$D064[i] %in% c(10,8) & all_04_1$D065[i] %in% c(10,8)) {
    #D038 in (.,8) and D061 in (.,8) and D064 in (.,8) and D065 in (.,8) then hormones_core26 = NA
    all_04_1$hormones_data4[i] = NA
  } else {
    #Else NA
    all_04_1$hormones_data4[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_04_1)) {
  if (is.na(all_04_1$hormones_data4[i])) {
    #NA remains NA
    all_04_1$hormones_core4[i] = NA
  } else if (all_04_1$D053[i] == 0 & all_04_1$hormones_data4[i] == 1) {
    #taking hormones though not in menopause
    all_04_1$hormones_core4[i] = 2
  } else {
    #Else same value as before
    all_04_1$hormones_core4[i] = all_04_1$hormones_data4[i]
  }
}


#### Gen 2 Exam 5/Omni 1 Exam 5 ####

#Hormone questions: E257, E281, E285, E286
#Menopause questions: E273

#Gen 2 Exam 5
exam_05_1 <- read_sas("e_exam_ex05_1_0083.sas7bdat")
select_05_1 <- exam_05_1[,c("ID", "IDTYPE", "E273", "E257", "E281", "E285", "E286")]
#create framid column
select_05_1$FRAMID <- (80000 + select_05_1$ID)

#Omni 1 Exam 1
exam_01_7 <- read_sas("e_exam_ex01_7_0020.sas7bdat")
select_01_7 <- exam_01_7[,c("id", "idtype", "e273", "e257", "e281", "e285", "e286")]
#create framid column
select_01_7$framid <- (700000 + select_01_7$id)

#Combine Exam 1 sections into one data frame
colnames(select_05_1) <- colnames(select_01_7)
all_05_1b <- do.call("rbind", list(select_05_1, select_01_7))

#Replacing NA with 10 avoids R treating those values differently
all_05_1b <- all_05_1b %>% mutate_at(c("e273", "e257", "e281", "e285", "e286"), ~replace_na(.,10))

#Placeholder values, used for testing
all_05_1b$hormones_data5 = 9999
all_05_1b$hormones_core5 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_05_1b)) {
  if (all_05_1b$e257[i] %in% c(1,2) | all_05_1b$e281[i] %in% c(1,2) 
      | all_05_1b$e285[i] %in% c(1,2) | all_05_1b$e286[i] %in% c(1,2)) {
    #e257 in (1,2) or e281 in (1,2) or e285 in (1,2) or e286 in (1,2) then 1
    all_05_1b$hormones_data5[i] = 1
  } else if (all_05_1b$e257[i] %in% c(0,3) & all_05_1b$e281[i] == 0 
             & all_05_1b$e285[i] == 0 & all_05_1b$e286[i] == 0) {
    #e257 in (0,3) and e281 = 0 and e285 = 0 and e286 = 0 then 0
    all_05_1b$hormones_data5[i] = 0
  } else if (all_05_1b$e257[i] %in% c(10,8) & all_05_1b$e281[i] %in% c(10,8) 
             & all_05_1b$e285[i] %in% c(10,8) & all_05_1b$e286[i] %in% c(10,8)) {
    #e257 in (.,8) and e281 in (.,8) and e285 in (.,8) and e286 in (.,8) then hormones_core26 = NA
    all_05_1b$hormones_data5[i] = NA
  } else {
    #Else NA
    all_05_1b$hormones_data5[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_05_1b)) {
  if (is.na(all_05_1b$hormones_data5[i])) {
    #NA remains NA
    all_05_1b$hormones_core5[i] = NA
  } else if (all_05_1b$e273[i] == 0 & all_05_1b$hormones_data5[i] == 1) {
    #taking hormones though not in menopause
    all_05_1b$hormones_core5[i] = 2
  } else {
    #Else same value as before
    all_05_1b$hormones_core5[i] = all_05_1b$hormones_data5[i]
  }
}


#### Gen 2 Exam 6 ####

#Hormone questions: F221, F247, F251, F252
#Menopause questions: F237

exam_06_1 <- read_sas("e_exam_ex06_1_0084.sas7bdat")
select_06_1 <- exam_06_1[,c("ID", "IDTYPE", "F237", "F221", "F247", "F251", "F252")]
#create framid column
colnames(select_06_1)[1:2] <- c("id","idtype")
select_06_1$framid <- (80000 + select_06_1$id)

#Replacing NA with 10 avoids R treating those values differently
all_06_1 <- select_06_1 %>% mutate_at(c("F237", "F221", "F247", "F251", "F252"), ~replace_na(.,10))

#Placeholder values, used for testing
all_06_1$hormones_data6 = 9999
all_06_1$hormones_core6 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_06_1)) {
  if (all_06_1$F221[i] %in% c(1,2) | all_06_1$F247[i] %in% c(1,2) 
      | all_06_1$F251[i] %in% c(1,2) | all_06_1$F252[i] %in% c(1,2)) {
    #F221 in (1,2) or F247 in (1,2) or F251 in (1,2) or F252 in (1,2) then 1
    all_06_1$hormones_data6[i] = 1
  } else if (all_06_1$F221[i] %in% c(0,3) & all_06_1$F247[i] == 0 
             & all_06_1$F251[i] == 0 & all_06_1$F252[i] == 0) {
    #F221 in (0,3) and F247 = 0 and F251 = 0 and F252 = 0 then 0
    all_06_1$hormones_data6[i] = 0
  } else if (all_06_1$F221[i] %in% c(10,8) & all_06_1$F247[i] %in% c(10,8) 
             & all_06_1$F251[i] %in% c(10,8) & all_06_1$F252[i] %in% c(10,8)) {
    #F221 in (.,8) and F247 in (.,8) and F251 in (.,8) and F252 in (.,8) then hormones_core26 = NA
    all_06_1$hormones_data6[i] = NA
  } else {
    #Else NA
    all_06_1$hormones_data6[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_06_1)) {
  if (is.na(all_06_1$hormones_data6[i])) {
    #NA remains NA
    all_06_1$hormones_core6[i] = NA
  } else if (all_06_1$F237[i] == 0 & all_06_1$hormones_data6[i] == 1) {
    #taking hormones though not in menopause
    all_06_1$hormones_core6[i] = 2
  } else {
    #Else same value as before
    all_06_1$hormones_core6[i] = all_06_1$hormones_data6[i]
  }
}


#### Gen 2 Exam 7/Omni 1 Exam 7 ####

#Hormone questions: G060, G088, G093, G094
#Menopause questions: G077

exam_07_1 <- read_sas("e_exam_ex07_1_0085_v1.sas7bdat")
select_07_1 <- exam_07_1[,c("ID", "IDTYPE", "G077", "G060", "G088", "G093", "G094")]
#create framid column
select_07_1$FRAMID <- (80000 + select_07_1$ID)

#Omni 1 Exam 3
exam_02_7 <- read_sas("e_exam_ex02_7_0003.sas7bdat")
select_02_7 <- exam_02_7[,c("id", "idtype", "g077", "g060", "g088", "g093", "g094")]
#create framid column
select_02_7$framid <- (700000 + select_02_7$id)

#Combine sections into one data frame
colnames(select_07_1) <- colnames(select_02_7)
all_07_1b <- do.call("rbind", list(select_07_1, select_02_7))

#Replacing NA with 10 avoids R treating those values differently
all_07_1b <- all_07_1b %>% mutate_at(c("g077", "g060", "g088", "g093", "g094"), ~replace_na(.,10))

#Placeholder values, used for testing
all_07_1b$hormones_data7 = 9999
all_07_1b$hormones_core7 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_07_1b)) {
  if (all_07_1b$g060[i] %in% c(1,2) | all_07_1b$g088[i] %in% c(1,2) 
      | all_07_1b$g093[i] %in% c(1,2) | all_07_1b$g094[i] %in% c(1,2)) {
    #g060 in (1,2) or g088 in (1,2) or g093 in (1,2) or g094 in (1,2) then 1
    all_07_1b$hormones_data7[i] = 1
  } else if (all_07_1b$g060[i] %in% c(0,3) & all_07_1b$g088[i] == 0 
             & all_07_1b$g093[i] == 0 & all_07_1b$g094[i] == 0) {
    #g060 in (0,3) and g088 = 0 and g093 = 0 and g094 = 0 then 0
    all_07_1b$hormones_data7[i] = 0
  } else if (all_07_1b$g060[i] %in% c(10,8) & all_07_1b$g088[i] %in% c(10,8) 
             & all_07_1b$g093[i] %in% c(10,8) & all_07_1b$g094[i] %in% c(10,8)) {
    #g060 in (.,8) and g088 in (.,8) and g093 in (.,8) and g094 in (.,8) then hormones_core26 = NA
    all_07_1b$hormones_data7[i] = NA
  } else {
    #Else NA
    all_07_1b$hormones_data7[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_07_1b)) {
  if (is.na(all_07_1b$hormones_data7[i])) {
    #NA remains NA
    all_07_1b$hormones_core7[i] = NA
  } else if (all_07_1b$g077[i] == 0 & all_07_1b$hormones_data7[i] == 1) {
    #taking hormones though not in menopause
    all_07_1b$hormones_core7[i] = 2
  } else {
    #Else same value as before
    all_07_1b$hormones_core7[i] = all_07_1b$hormones_data7[i]
  }
}


#### Gen 2 Exam 8/Omni 1 Exam 8 ####

#Hormone questions: H045, H057
#Menopause questions: H038

exam_08_1 <- read_sas("e_exam_ex08_1_0005.sas7bdat")
select_08_1 <- exam_08_1[,c("ID", "IDTYPE", "H038", "H045", "H057")]
#create framid column
select_08_1$FRAMID <- (80000 + select_08_1$ID)

#Omni 1 Exam 3
exam_03_7 <- read_sas("e_exam_ex03_7_0426.sas7bdat")
select_03_7 <- exam_03_7[,c("id", "idtype", "h038", "h045", "h057")]
#create framid column
select_03_7$framid <- (700000 + select_03_7$id)

#Combine sections into one data frame
colnames(select_08_1) <- colnames(select_03_7)
select_08_1b <- do.call("rbind", list(select_08_1, select_03_7))


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
hormone_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% hormone_atc_list))
#Constructing the output data frame
atc_framid_08_1b <- unique(cod_08_1b$framid)
med_found_08_1b <- data.frame(atc_framid_08_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_08_1b['atc_hormones_08_1b'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% hormone_filtered_08_1b$framid)
#prepare for merge
colnames(med_found_08_1b)[1] <- "framid"

#merge
all_08_1b <- merge(select_08_1b, med_found_08_1b, all = TRUE)

#Replacing NA with 10 avoids R treating those values differently
all_08_1b <- all_08_1b %>% mutate_at(c("h038", "h045", "h057", "atc_hormones_08_1b"), ~replace_na(.,10))

#Placeholder values, used for testing
all_08_1b$hormones_data8 = 9999
all_08_1b$hormones_core8 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_08_1b)) {
  if (all_08_1b$h045[i] %in% c(1,2) | all_08_1b$h057[i] %in% c(1,2)) {
    #h045 in (1,2) or h057 in (1,2) then 1
    all_08_1b$hormones_data8[i] = 1
  } else if (all_08_1b$atc_hormones_08_1b[i] == 1) {
    #if ATC data = 1 then 1
    all_08_1b$hormones_data8[i] = 1
  } else if (all_08_1b$h045[i] == 0 & all_08_1b$h057[i] == 0) {
    #h045 = 0 and h057 = 0 and then 0
    all_08_1b$hormones_data8[i] = 0
  } else if (all_08_1b$h045[i] %in% c(10,8) & all_08_1b$h057[i] %in% c(10,8)) {
    #h045 in (.,8) and h057 in (.,8) then NA
    all_08_1b$hormones_data8[i] = NA
  } else {
    #Else NA
    all_08_1b$hormones_data8[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_08_1b)) {
  if (is.na(all_08_1b$hormones_data8[i])) {
    #NA remains NA
    all_08_1b$hormones_core8[i] = NA
  } else if (all_08_1b$h038[i] == 0 & all_08_1b$hormones_data8[i] == 1) {
    #taking hormones though not in menopause
    all_08_1b$hormones_core8[i] = 2
  } else {
    #Else same value as before
    all_08_1b$hormones_core8[i] = all_08_1b$hormones_data8[i]
  }
}


#### Gen 2 Exam 9/Omni 1 Exam 9 ####

#Hormone questions: J061
#Menopause questions: J039
exam_09_1b <- read_sas("e_exam_ex09_1b_0844.sas7bdat")
select_09_1b <- exam_09_1b[,c("id", "idtype", "j039", "j061")]
#create framid column
select_09_1b$framid <- with(select_09_1b, ifelse(idtype == 1, 80000 + id, 
                                                 ifelse(idtype == 7, 700000 + id, id)))

#medications
meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
cod_09_1b <- meds_09_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#create framid column
cod_09_1b$framid <- with(cod_09_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
hormone_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% hormone_atc_list))
#Constructing the output data frame
atc_framid_09_1b <- unique(cod_09_1b$framid)
med_found_09_1b <- data.frame(atc_framid_09_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_09_1b['atc_hormones_09_1b'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% hormone_filtered_09_1b$framid)
#prepare for merge
colnames(med_found_09_1b)[1] <- "framid"

#merge
all_09_1b <- merge(select_09_1b, med_found_09_1b, all = TRUE)

#Replacing NA with 10 avoids R treating those values differently
all_09_1b <- all_09_1b %>% mutate_at(c("j039", "j061", "atc_hormones_09_1b"), ~replace_na(.,10))

#Placeholder values, used for testing
all_09_1b$hormones_data9 = 9999
all_09_1b$hormones_core9 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_09_1b)) {
  if (all_09_1b$j061[i] %in% c(1,2)) {
    #j061 in (1,2) then 1
    all_09_1b$hormones_data9[i] = 1
  } else if (all_09_1b$atc_hormones_09_1b[i] == 1) {
    #if ATC data = 1 then 1
    all_09_1b$hormones_data9[i] = 1
  } else if (all_09_1b$j061[i] == 0) {
    #j061 = 0 then 0
    all_09_1b$hormones_data9[i] = 0
  } else if (all_09_1b$j061[i] %in% c(10,8)) {
    #j061 in (.,8) then NA
    all_09_1b$hormones_data9[i] = NA
  } else {
    #Else NA
    all_09_1b$hormones_data9[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_09_1b)) {
  if (is.na(all_09_1b$hormones_data9[i])) {
    #NA remains NA
    all_09_1b$hormones_core9[i] = NA
  } else if (all_09_1b$j039[i] %in% c(0,1,2) & all_09_1b$hormones_data9[i] == 1) {
    #taking hormones though not in menopause
    all_09_1b$hormones_core9[i] = 2
  } else {
    #Else same value as before
    all_09_1b$hormones_core9[i] = all_09_1b$hormones_data9[i]
  }
}

#### Gen 2 Exam 10/Omni 1 Exam 5 ####

#Hormone questions: K0293
#Menopause questions: K0285
exam_10_1b <- read_sas("e_exam_ex10_1b_1409.sas7bdat")
select_10_1b <- exam_10_1b[,c("id", "idtype", "K0285", "K0293")]
#create framid column
select_10_1b$framid <- with(select_10_1b, ifelse(idtype == 1, 80000 + id, 
                                                 ifelse(idtype == 7, 700000 + id, id)))

#medications
meds_10_1b <- read_sas("vr_meds_ex10_1b_1198.sas7bdat")
cod_10_1b <- meds_10_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#create framid column
cod_10_1b$framid <- with(cod_10_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
hormone_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% hormone_atc_list))
#Constructing the output data frame
atc_framid_10_1b <- unique(cod_10_1b$framid)
med_found_10_1b <- data.frame(atc_framid_10_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_10_1b['atc_hormones_10_1b'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% hormone_filtered_10_1b$framid)
#prepare for merge
colnames(med_found_10_1b)[1] <- "framid"

#merge
all_10_1b <- merge(select_10_1b, med_found_10_1b, all = TRUE)

#Replacing NA with 10 avoids R treating those values differently
all_10_1b <- all_10_1b %>% mutate_at(c("K0285", "K0293", "atc_hormones_10_1b"), ~replace_na(.,10))

#Placeholder values, used for testing
all_10_1b$hormones_data10 = 9999
all_10_1b$hormones_core10 = 9999

#first for loop to determine data variable
for (i in 1:nrow(all_10_1b)) {
  if (all_10_1b$K0293[i] %in% c(1,2)) {
    #K0293 in (1,2) then 1
    all_10_1b$hormones_data10[i] = 1
  } else if (all_10_1b$atc_hormones_10_1b[i] == 1) {
    #if ATC data = 1 then 1
    all_10_1b$hormones_data10[i] = 1
  } else if (all_10_1b$K0293[i] == 0) {
    #K0293 = 0 then 0
    all_10_1b$hormones_data10[i] = 0
  } else if (all_10_1b$K0293[i] %in% c(10,8)) {
    #K0293 in (.,8) then NA
    all_10_1b$hormones_data10[i] = NA
  } else {
    #Else NA
    all_10_1b$hormones_data10[i] = NA
  }
}

#second for loop to determine core variable
for (i in 1:nrow(all_10_1b)) {
  if (is.na(all_10_1b$hormones_data10[i])) {
    #NA remains NA
    all_10_1b$hormones_core10[i] = NA
  } else if (all_10_1b$K0285[i] %in% c(0,1,2) & all_10_1b$hormones_data10[i] == 1) {
    #taking hormones though not in menopause
    all_10_1b$hormones_core10[i] = 2
  } else {
    #Else same value as before
    all_10_1b$hormones_core10[i] = all_10_1b$hormones_data10[i]
  }
}


#### Final Merge ####
#merge datasets
horm_list <- list(all_01_1, all_02_1, all_03_1, all_04_1, all_05_1b, all_06_1, all_07_1b, all_08_1b, all_09_1b, all_10_1b)
horm_joined <- horm_list %>% reduce(full_join, by = c("framid", "id", "idtype"))

#subset for only ID and menopause columns
horm_only <- subset(horm_joined, select = c(framid, id, idtype, hormones_core1, hormones_core2, hormones_core3, 
                                            hormones_core4, hormones_core5, hormones_core6, hormones_core7,
                                            hormones_core8, hormones_core9, hormones_core10))

write.csv(horm_only, file = "Hormone_Usage_Variables_Gen2_Omni1.csv", row.names = FALSE)

