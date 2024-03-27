# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 Anxiety/Depression derived variable exam drug usage source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: February 2024
# 
# 
# The purpose of this R code is to
# 1) abstract data about clinical exams for anxiety/depression in all relevant Gen 2/Omni 1 exams.
# 2) find individuals in those examswho used drugs in categories 81-85 (Anxiolytics/Hypnotics/Antidepressants), using ATC codes.
# 3) create derived variables based on those data sources and return data containing all important variables.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Category 81-85 (Anxiolytics/Hypnotics/Antidepressants) ATC information - ATClabels_Anxio_Hyp_Depr_ALL.xlsx
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


#### Reading ATC data ####

#This is used for exams 8-10
#reading three drug lists from excel spreadsheet
anxiolytics <- read_excel("ATClabels_Anxio_Hyp_Depr_ALL.xlsx", sheet = "Anxiolytics")
anxio_atc_list <- anxiolytics$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`
hypnotics <- read_excel("ATClabels_Anxio_Hyp_Depr_ALL.xlsx", sheet = "Hypnotics")
hypno_atc_list <- hypnotics$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`
antidep <- read_excel("ATClabels_Anxio_Hyp_Depr_ALL.xlsx", sheet = "Antidep")
antidep_atc_list <- antidep$`ATC CODE FOR MEDICATION OR FIRST DRUG IN COMPOUND`


#In each of these sections an individual .sas7bdat file is read
#Then columns containing the data for questions we want are isolated
#New columns are created based on these sources
#This can change between exams


#### Gen 2 Exam 1 ####
#A85
exam_01_1 <- read_sas("e_exam_ex01_1_0079_v1.sas7bdat")
select_01_1 <- exam_01_1[,c("ID", "A85")]
#add further id columns
select_01_1 <- select_01_1 %>% mutate(IDTYPE = 1, .before = ID)
select_01_1 <- select_01_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Placeholder values, used for testing
select_01_1$Anxiolytics_core1 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_01_1)) {
  if (is.na(select_01_1$A85[i])) {
    #Unknown = NA
    select_01_1$Anxiolytics_core1[i] = NA
  } else if (select_01_1$A85[i] == 6) {
    #No = 0
    select_01_1$Anxiolytics_core1[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_01_1$Anxiolytics_core1[i] = 1
  }
}

#remove unneeded column
select_01_1 <- select_01_1 %>% select(-A85)
#replace NA with "."
select_01_1 <- select_01_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 2 ####
#B67
exam_02_1 <- read_sas("e_exam_ex02_1_0080_v1.sas7bdat")
select_02_1 <- exam_02_1[,c("ID", "B66", "B67")]
#add further id columns
select_02_1 <- select_02_1 %>% mutate(IDTYPE = 1, .before = ID)
select_02_1 <- select_02_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)


#Placeholder values, used for testing
select_02_1$Hypnotics_core2 = 9999
select_02_1$Anxiolytics_core2 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_02_1)) {
  if (is.na(select_02_1$B66[i])) {
    #Unknown = NA
    select_02_1$Hypnotics_core2[i] = NA
  } else if (select_02_1$B66[i] == 0) {
    #No = 0
    select_02_1$Hypnotics_core2[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_02_1$Hypnotics_core2[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_02_1)) {
  if (is.na(select_02_1$B67[i])) {
    #Unknown = NA
    select_02_1$Anxiolytics_core2[i] = NA
  } else if (select_02_1$B67[i] == 0) {
    #No = 0
    select_02_1$Anxiolytics_core2[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_02_1$Anxiolytics_core2[i] = 1
  }
}

#remove unneeded columns
select_02_1 <- subset(select_02_1, select = -c(B66, B67))
#replace NA with "."
select_02_1 <- select_02_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 3 ####
#C40, C41, C42
exam_03_1 <- read_sas("e_exam_ex03_1_0081.sas7bdat")
select_03_1 <- exam_03_1[,c("ID", "C40", "C41", "C42")]
#add further id columns
select_03_1 <- select_03_1 %>% mutate(IDTYPE = 1, .before = ID)
select_03_1 <- select_03_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Placeholder values, used for testing
select_03_1$Anxiolytics_core3 = 9999
select_03_1$Hypnotics_core3 = 9999
select_03_1$Antidepressants_core3 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_03_1)) {
  if (is.na(select_03_1$C40[i])) {
    #Unknown = NA
    select_03_1$Anxiolytics_core3[i] = NA
  } else if (select_03_1$C40[i] == 0 | select_03_1$C40[i] == 3) {
    #No/Maybe = 0
    select_03_1$Anxiolytics_core3[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_03_1$Anxiolytics_core3[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_03_1)) {
  if (is.na(select_03_1$C41[i])) {
    #Unknown = NA
    select_03_1$Hypnotics_core3[i] = NA
  } else if (select_03_1$C41[i] == 0 | select_03_1$C41[i] == 3) {
    #No/Maybe = 0
    select_03_1$Hypnotics_core3[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_03_1$Hypnotics_core3[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_03_1)) {
  if (is.na(select_03_1$C42[i])) {
    #Unknown = NA
    select_03_1$Antidepressants_core3[i] = NA
  } else if (select_03_1$C42[i] == 0 | select_03_1$C42[i] == 3) {
    #No/Maybe = 0
    select_03_1$Antidepressants_core3[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_03_1$Antidepressants_core3[i] = 1
  }
}

#remove unneeded columns
select_03_1 <- subset(select_03_1, select = -c(C40, C41, C42))
#replace NA with "."
select_03_1 <- select_03_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 4 #### 
#D046, D047, D048
exam_04_1 <- read_sas("e_exam_ex04_1_0082.sas7bdat")
select_04_1 <- exam_04_1[,c("ID", "D046", "D047", "D048")]
#add further id columns
select_04_1 <- select_04_1 %>% mutate(IDTYPE = 1, .before = ID)
select_04_1 <- select_04_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Placeholder values, used for testing
select_04_1$Anxiolytics_core4 = 9999
select_04_1$Hypnotics_core4 = 9999
select_04_1$Antidepressants_core4 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_04_1)) {
  if (is.na(select_04_1$D046[i])) {
    #Unknown = NA
    select_04_1$Anxiolytics_core4[i] = NA
  } else if (select_04_1$D046[i] == 0 | select_04_1$D046[i] == 3) {
    #No/Maybe = 0
    select_04_1$Anxiolytics_core4[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_04_1$Anxiolytics_core4[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_04_1)) {
  if (is.na(select_04_1$D047[i])) {
    #Unknown = NA
    select_04_1$Hypnotics_core4[i] = NA
  } else if (select_04_1$D047[i] == 0 | select_04_1$D047[i] == 3) {
    #No/Maybe = 0
    select_04_1$Hypnotics_core4[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_04_1$Hypnotics_core4[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_04_1)) {
  if (is.na(select_04_1$D048[i])) {
    #Unknown = NA
    select_04_1$Antidepressants_core4[i] = NA
  } else if (select_04_1$D048[i] == 0 | select_04_1$D048[i] == 3) {
    #No/Maybe = 0
    select_04_1$Antidepressants_core4[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_04_1$Antidepressants_core4[i] = 1
  }
}

#remove unneeded columns
select_04_1 <- subset(select_04_1, select = -c(D046, D047, D048))
#replace NA with "."
select_04_1 <- select_04_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 5/Omni 1 Exam 1 ####

#E265, E266, E267
#Gen 2 Exam 5
exam_05_1 <- read_sas("e_exam_ex05_1_0083.sas7bdat")
select_05_1 <- exam_05_1[,c("IDTYPE", "ID", "E265", "E266", "E267")]
#add further id columns
select_05_1 <- select_05_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Omni 1 Exam 1
exam_01_7 <- read_sas("e_exam_ex01_7_0020.sas7bdat")
select_01_7 <- exam_01_7[,c("idtype", "id", "e265", "e266", "e267")]
#add further id columns
select_01_7 <- select_01_7 %>% mutate(framid = (700000 + id), .after = id)

#Combine sections into one data frame
colnames(select_01_7) <- colnames(select_05_1)
select_05_1b <- rbind(select_05_1, select_01_7)

#Placeholder values, used for testing
select_05_1b$Anxiolytics_core5 = 9999
select_05_1b$Hypnotics_core5 = 9999
select_05_1b$Antidepressants_core5 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_05_1b)) {
  if (is.na(select_05_1b$E265[i])) {
    #Unknown = NA
    select_05_1b$Anxiolytics_core5[i] = NA
  } else if (select_05_1b$E265[i] == 0 | select_05_1b$E265[i] == 3) {
    #No/Maybe = 0
    select_05_1b$Anxiolytics_core5[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_05_1b$Anxiolytics_core5[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_05_1b)) {
  if (is.na(select_05_1b$E266[i])) {
    #Unknown = NA
    select_05_1b$Hypnotics_core5[i] = NA
  } else if (select_05_1b$E266[i] == 0 | select_05_1b$E266[i] == 3) {
    #No/Maybe = 0
    select_05_1b$Hypnotics_core5[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_05_1b$Hypnotics_core5[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_05_1b)) {
  if (is.na(select_05_1b$E267[i])) {
    #Unknown = NA
    select_05_1b$Antidepressants_core5[i] = NA
  } else if (select_05_1b$E267[i] == 0 | select_05_1b$E267[i] == 3) {
    #No/Maybe = 0
    select_05_1b$Antidepressants_core5[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_05_1b$Antidepressants_core5[i] = 1
  }
}

#remove unneeded columns
select_05_1b <- subset(select_05_1b, select = -c(E265, E266, E267))
#replace NA with "."
select_05_1b <- select_05_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 6 #### 
#F228, F229, F230
exam_06_1 <- read_sas("e_exam_ex06_1_0084.sas7bdat")
select_06_1 <- exam_06_1[,c("ID", "F228", "F229", "F230")]
#create framid column
select_06_1$FRAMID <- (80000 + select_06_1$ID)

#Placeholder values, used for testing
select_06_1$Anxiolytics_core6 = 9999
select_06_1$Hypnotics_core6 = 9999
select_06_1$Antidepressants_core6 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_06_1)) {
  if (is.na(select_06_1$F228[i])) {
    #Unknown = NA
    select_06_1$Anxiolytics_core6[i] = NA
  } else if (select_06_1$F228[i] == 0 | select_06_1$F228[i] == 3) {
    #No/Maybe = 0
    select_06_1$Anxiolytics_core6[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_06_1$Anxiolytics_core6[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_06_1)) {
  if (is.na(select_06_1$F229[i])) {
    #Unknown = NA
    select_06_1$Hypnotics_core6[i] = NA
  } else if (select_06_1$F229[i] == 0 | select_06_1$F229[i] == 3) {
    #No/Maybe = 0
    select_06_1$Hypnotics_core6[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_06_1$Hypnotics_core6[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_06_1)) {
  if (is.na(select_06_1$F230[i])) {
    #Unknown = NA
    select_06_1$Antidepressants_core6[i] = NA
  } else if (select_06_1$F230[i] == 0 | select_06_1$F230[i] == 3) {
    #No/Maybe = 0
    select_06_1$Antidepressants_core6[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_06_1$Antidepressants_core6[i] = 1
  }
}

#remove unneeded columns
select_06_1 <- subset(select_06_1, select = -c(F228, F229, F230))
#replace NA with "."
select_06_1 <- select_06_1 %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))
#add IDTYPE column
select_06_1 <- select_06_1 %>% add_column(IDTYPE = "1", .after = 1)


#CES-D Gen 2 Exam 6
#negatively phrased questions
cesd_first_06_1 <- exam_06_1[,c("ID", "F151", "F152", "F153", "F155", "F156", 
                                "F157", "F159", "F160", "F161", "F163", "F164", 
                                "F165", "F167", "F168", "F169", "F170")]
#positively phrased questions
cesd_second_06_1 <- exam_06_1[,c("ID", "F154", "F158", "F162", "F166")]
#sum of negatively phrased questions
cesd_first_06_1$first_sum <- rowSums(cesd_first_06_1[2:17], na.rm=TRUE)
#sum of positively phrased questions
cesd_second_06_1$F154_new <- (3 - cesd_second_06_1$F154)
cesd_second_06_1$F158_new <- (3 - cesd_second_06_1$F158)
cesd_second_06_1$F162_new <- (3 - cesd_second_06_1$F162)
cesd_second_06_1$F166_new <- (3 - cesd_second_06_1$F166)
cesd_second_06_1$second_sum <- rowSums(cesd_second_06_1[6:9], na.rm=TRUE)
#add sums together
cesd_second_06_1$first_sum <- cesd_first_06_1$first_sum
cesd_second_06_1$CESD_core6 <- cesd_second_06_1$first_sum + cesd_second_06_1$second_sum
select_06_1$CESD_core6 <- cesd_second_06_1$CESD_core6


#### Gen 2 Exam 7/Omni 1 Exam 2 ####

#G067, G068, G069
#Gen 2 Exam 7
exam_07_1 <- read_sas("e_exam_ex07_1_0085_v1.sas7bdat")
select_07_1 <- exam_07_1[,c("IDTYPE", "ID", "G067", "G068", "G069", "G498")]
#add further id columns
select_07_1 <- select_07_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Omni 1 Exam 2
exam_02_7 <- read_sas("e_exam_ex02_7_0003.sas7bdat")
select_02_7 <- exam_02_7[,c("idtype", "id", "g067", "g068", "g069")]
#G498 is not in exam dataset
select_02_7$g498 <- NA
#add further id columns
select_02_7 <- select_02_7 %>% mutate(framid = (700000 + id), .after = id)

#Combine sections into one data frame
colnames(select_02_7) <- colnames(select_07_1)
select_07_1b <- rbind(select_07_1, select_02_7)

#Placeholder values, used for testing
select_07_1b$Anxiolytics_core7 = 9999
select_07_1b$Hypnotics_core7 = 9999
select_07_1b$Antidepressants_core7 = 9999
select_07_1b$CDI_Depression_core7 = 9999
select_07_1b$Depression_core7 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_07_1b)) {
  if (is.na(select_07_1b$G067[i])) {
    #Unknown = NA
    select_07_1b$Anxiolytics_core7[i] = NA
  } else if (select_07_1b$G067[i] == 0 | select_07_1b$G067[i] == 3) {
    #No/Maybe = 0
    select_07_1b$Anxiolytics_core7[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_07_1b$Anxiolytics_core7[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_07_1b)) {
  if (is.na(select_07_1b$G068[i])) {
    #Unknown = NA
    select_07_1b$Hypnotics_core7[i] = NA
  } else if (select_07_1b$G068[i] == 0 | select_07_1b$G068[i] == 3) {
    #No/Maybe = 0
    select_07_1b$Hypnotics_core7[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_07_1b$Hypnotics_core7[i] = 1
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_07_1b)) {
  if (is.na(select_07_1b$G069[i])) {
    #Unknown = NA
    select_07_1b$Antidepressants_core7[i] = NA
  } else if (select_07_1b$G069[i] == 0 | select_07_1b$G069[i] == 3) {
    #No/Maybe = 0
    select_07_1b$Antidepressants_core7[i] = 0
  } else {
    #Yes now/Yes not now = 1
    select_07_1b$Antidepressants_core7[i] = 1
  }
}

for (i in 1:nrow(select_07_1b)) {
  if (is.na(select_07_1b$G498[i])) {
    #Unknown = NA
    select_07_1b$CDI_Depression_core7[i] = NA
  } else if (select_07_1b$G498[i] == 0 | select_07_1b$G498[i] == 2) {
    #No/Maybe = 0
    select_07_1b$CDI_Depression_core7[i] = 0
  } else {
    #Yes = 1
    select_07_1b$CDI_Depression_core7[i] = 1
  }
}

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
select_07_1b <- select_07_1b %>% mutate_at(c("Antidepressants_core7", "CDI_Depression_core7"), ~replace_na(.,9))

#for loop to determine depression derived variable
for (i in 1:nrow(select_07_1b)) {
  if (select_07_1b$Antidepressants_core7[i] == 1 & select_07_1b$CDI_Depression_core7[i] == 1) {
    #If Antidepressants_core7 = 1 and CDI_Depression_core7 = 1 then Depression_core7 = 1
    select_07_1b$Depression_core7[i] = 1
  } else if (select_07_1b$Antidepressants_core7[i] == 1 & select_07_1b$CDI_Depression_core7[i] == 0) {
    #If Antidepressants_core7 = 1 and CDI_Depression_core7 = 0 then Depression_core7 = 1
    select_07_1b$Depression_core7[i] = 1
  } else if (select_07_1b$Antidepressants_core7[i] == 0 & select_07_1b$CDI_Depression_core7[i] == 1) {
    #If Antidepressants_core7 = 0 and CDI_Depression_core7 = 1 then Depression_core7 = 1
    select_07_1b$Depression_core7[i] = 1
  } else if (select_07_1b$Antidepressants_core7[i] == 0 & select_07_1b$CDI_Depression_core7[i] == 0) {
    #If Antidepressants_core7 = 0 and CDI_Depression_core7 = 0 then Depression_core7 = 0
    select_07_1b$Depression_core7[i] = 0
  } else if (select_07_1b$Antidepressants_core7[i] == 9 & select_07_1b$CDI_Depression_core7[i] == 1) {
    #If Antidepressants_core7 = NA and CDI_Depression_core7 = 1 then Depression_core7 = 1
    select_07_1b$Depression_core7[i] = 1
  } else if (select_07_1b$Antidepressants_core7[i] == 9 & select_07_1b$CDI_Depression_core7[i] == 0) {
    #If Antidepressants_core7 = NA and CDI_Depression_core7 = 0 then Depression_core7 = 0
    select_07_1b$Depression_core7[i] = 0
  } else if (select_07_1b$Antidepressants_core7[i] == 1 & select_07_1b$CDI_Depression_core7[i] == 9) {
    #If Antidepressants_core7 = 1 and CDI_Depression_core7 = NA then Depression_core7 = 1
    select_07_1b$Depression_core7[i] = 1
  } else if (select_07_1b$Antidepressants_core7[i] == 0 & select_07_1b$CDI_Depression_core7[i] == 9) {
    #If Antidepressants_core7 = 0 and CDI_Depression_core7 = NA then Depression_core7 = 0
    select_07_1b$Depression_core7[i] = 0
  } else if (select_07_1b$Antidepressants_core7[i] == 9 & select_07_1b$CDI_Depression_core7[i] == 9) {
    #If Antidepressants_core7 = NA and CDI_Depression_core7 = NA then Depression_core7 = NA
    select_07_1b$Depression_core7[i] = NA
  }
}


#returning NA values to columns, removing placeholder value
select_07_1b[c("Antidepressants_core7", "CDI_Depression_core7")] <- 
  replace(select_07_1b[c("Antidepressants_core7", "CDI_Depression_core7")],
          select_07_1b[c("Antidepressants_core7", "CDI_Depression_core7")] == 9, NA)
#remove unneeded columns
select_07_1b <- subset(select_07_1b, select = -c(G067, G068, G069, G498))
#replace NA with "."
select_07_1b <- select_07_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#CES-D Gen 2 Exam 7
#negatively phrased questions
cesd_first_07_1 <- exam_07_1[,c("ID", "G587", "G588", "G589", "G591", "G592", 
                                "G593", "G595", "G596", "G597", "G599", "G600", 
                                "G601", "G603", "G604", "G605", "G606")]
#positively phrased questions
cesd_second_07_1 <- exam_07_1[,c("ID", "G590", "G594", "G598", "G602")]
#sum of negatively phrased questions
cesd_first_07_1$first_sum <- rowSums(cesd_first_07_1[2:17], na.rm=TRUE)
#sum of positively phrased questions
cesd_second_07_1$G590_new <- (3 - cesd_second_07_1$G590)
cesd_second_07_1$G594_new <- (3 - cesd_second_07_1$G594)
cesd_second_07_1$G598_new <- (3 - cesd_second_07_1$G598)
cesd_second_07_1$G602_new <- (3 - cesd_second_07_1$G602)
cesd_second_07_1$second_sum <- rowSums(cesd_second_07_1[6:9], na.rm=TRUE)
#add sums together
cesd_second_07_1$first_sum <- cesd_first_07_1$first_sum
cesd_second_07_1$CESD_core7 <- cesd_second_07_1$first_sum + cesd_second_07_1$second_sum


#CES_D Omni 1 Exam 2
#negatively phrased questions
cesd_first_02_7 <- exam_02_7[,c("id", "g587", "g588", "g589", "g591", "g592", 
                                "g593", "g595", "g596", "g597", "g599", "g600", 
                                "g601", "g603", "g604", "g605", "g606")]
#positively phrased questions
cesd_second_02_7 <- exam_02_7[,c("id", "g590", "g594", "g598", "g602")]
#sum of negatively phrased questions
cesd_first_02_7$first_sum <- rowSums(cesd_first_02_7[2:17], na.rm=TRUE)
#sum of positively phrased questions
cesd_second_02_7$g590_new <- (3 - cesd_second_02_7$g590)
cesd_second_02_7$g594_new <- (3 - cesd_second_02_7$g594)
cesd_second_02_7$g598_new <- (3 - cesd_second_02_7$g598)
cesd_second_02_7$g602_new <- (3 - cesd_second_02_7$g602)
cesd_second_02_7$second_sum <- rowSums(cesd_second_02_7[6:9], na.rm=TRUE)
#add sums together
cesd_second_02_7$first_sum <- cesd_first_02_7$first_sum
cesd_second_02_7$CESD_core7 <- cesd_second_02_7$first_sum + cesd_second_02_7$second_sum

#bind columns together
colnames(cesd_second_02_7) <- colnames(cesd_second_07_1)
cesd_second_07_1b <- rbind(cesd_second_07_1, cesd_second_02_7)
#add column to main data frame
select_07_1b$CESD_core7 <- cesd_second_07_1b$CESD_core7


#### Gen 2 Exam 8/Omni 1 Exam 3 ####


#ex08_1 (Offspring (GEN 2) Exam 8)
#H376, H377
exam_08_1 <- read_sas("e_exam_ex08_1_0005.sas7bdat")
select_08_1 <- exam_08_1[,c("ID", "IDTYPE", "H376", "H377")]
#add further id columns
select_08_1 <- select_08_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)


#ex03_7 (Omni 1 Exam 3)
#H376, H377
exam_03_7 <- read_sas("e_exam_ex03_7_0426.sas7bdat")
select_03_7 <- exam_03_7[,c("id", "idtype", "h376", "h377")]
#add further id columns
select_03_7 <- select_03_7 %>% mutate(framid = (700000 + id), .after = id)


#Combine sections into one data frame
colnames(select_03_7) <- colnames(select_08_1)
select_08_1b <- rbind(select_08_1, select_03_7)

#Placeholder values, used for testing
select_08_1b$CDI_Depression_core8 = 9999
select_08_1b$Depression_core8 = 9999
select_08_1b$CDI_Anxiety_core8 = 9999
select_08_1b$Anxiety_core8 = 9999

for (i in 1:nrow(select_08_1b)) {
  if (is.na(select_08_1b$H376[i])) {
    #Unknown = NA
    select_08_1b$CDI_Depression_core8[i] = NA
  } else if (select_08_1b$H376[i] == 0 | select_08_1b$H376[i] == 2) {
    #No/Maybe = 0
    select_08_1b$CDI_Depression_core8[i] = 0
  } else {
    #Yes = 1
    select_08_1b$CDI_Depression_core8[i] = 1
  }
}


for (i in 1:nrow(select_08_1b)) {
  if (is.na(select_08_1b$H377[i])) {
    #Unknown = NA
    select_08_1b$CDI_Anxiety_core8[i] = NA
  } else if (select_08_1b$H377[i] == 0 | select_08_1b$H377[i] == 2) {
    #No/Maybe = 0
    select_08_1b$CDI_Anxiety_core8[i] = 0
  } else {
    #Yes = 1
    select_08_1b$CDI_Anxiety_core8[i] = 1
  }
}


#### Gen 2 Exam 8/Omni 1 Exam 3: ATC ####

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
anxio_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
atc_framid_08_1b <- unique(cod_08_1b$framid)
med_found_08_1b <- data.frame(atc_framid_08_1b)

#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_08_1b['anxio_column'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% anxio_filtered_08_1b$id)
med_found_08_1b['hypno_column'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% hypno_filtered_08_1b$id)
med_found_08_1b['antidep_column'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% antidep_filtered_08_1b$id)

#standardizing column names
colnames(med_found_08_1b) <- c("FRAMID", "Anxiolytics_core8", "Hypnotics_core8", "Antidepressants_core8")

#merge
main_08_1b <- merge(select_08_1b, med_found_08_1b, by = "FRAMID", all = TRUE)

#### Gen 2 Exam 8/Omni 1 Exam 3: Logic ####

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
main_08_1b <- main_08_1b %>% mutate_at(c("Antidepressants_core8", "CDI_Depression_core8"), ~replace_na(.,9))

#for loop to determine depression derived variable
for (i in 1:nrow(main_08_1b)) {
  if (main_08_1b$Antidepressants_core8[i] == 1 & main_08_1b$CDI_Depression_core8[i] == 1) {
    #If Antidepressants_core8 = 1 and CDI_Depression_core8 = 1 then Depression_core8 = 1
    main_08_1b$Depression_core8[i] = 1
  } else if (main_08_1b$Antidepressants_core8[i] == 1 & main_08_1b$CDI_Depression_core8[i] == 0) {
    #If Antidepressants_core8 = 1 and CDI_Depression_core8 = 0 then Depression_core8 = 1
    main_08_1b$Depression_core8[i] = 1
  } else if (main_08_1b$Antidepressants_core8[i] == 0 & main_08_1b$CDI_Depression_core8[i] == 1) {
    #If Antidepressants_core8 = 0 and CDI_Depression_core8 = 1 then Depression_core8 = 1
    main_08_1b$Depression_core8[i] = 1
  } else if (main_08_1b$Antidepressants_core8[i] == 0 & main_08_1b$CDI_Depression_core8[i] == 0) {
    #If Antidepressants_core8 = 0 and CDI_Depression_core8 = 0 then Depression_core8 = 0
    main_08_1b$Depression_core8[i] = 0
  } else if (main_08_1b$Antidepressants_core8[i] == 9 & main_08_1b$CDI_Depression_core8[i] == 1) {
    #If Antidepressants_core8 = NA and CDI_Depression_core8 = 1 then Depression_core8 = 1
    main_08_1b$Depression_core8[i] = 1
  } else if (main_08_1b$Antidepressants_core8[i] == 9 & main_08_1b$CDI_Depression_core8[i] == 0) {
    #If Antidepressants_core8 = NA and CDI_Depression_core8 = 0 then Depression_core8 = 0
    main_08_1b$Depression_core8[i] = 0
  } else if (main_08_1b$Antidepressants_core8[i] == 1 & main_08_1b$CDI_Depression_core8[i] == 9) {
    #If Antidepressants_core8 = 1 and CDI_Depression_core8 = NA then Depression_core8 = 1
    main_08_1b$Depression_core8[i] = 1
  } else if (main_08_1b$Antidepressants_core8[i] == 0 & main_08_1b$CDI_Depression_core8[i] == 9) {
    #If Antidepressants_core8 = 0 and CDI_Depression_core8 = NA then Depression_core8 = 0
    main_08_1b$Depression_core8[i] = 0
  } else if (main_08_1b$Antidepressants_core8[i] == 9 & main_08_1b$CDI_Depression_core8[i] == 9) {
    #If Antidepressants_core8 = NA and CDI_Depression_core8 = NA then Depression_core8 = NA
    main_08_1b$Depression_core8[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
main_08_1b[c("Antidepressants_core8", "CDI_Depression_core8")] <- replace(main_08_1b[c("Antidepressants_core8", "CDI_Depression_core8")], main_08_1b[c("Antidepressants_core8", "CDI_Depression_core8")] == 9, NA)

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
main_08_1b <- main_08_1b %>% mutate_at(c("Anxiolytics_core8", "CDI_Anxiety_core8"), ~replace_na(.,9))

#for loop to determine Anxiety derived variable
for (i in 1:nrow(main_08_1b)) {
  if (main_08_1b$Anxiolytics_core8[i] == 1 & main_08_1b$CDI_Anxiety_core8[i] == 1) {
    #If Anxiolytics_core8 = 1 and CDI_Anxiety_core8 = 1 then Anxiety_core8 = 1
    main_08_1b$Anxiety_core8[i] = 1
  } else if (main_08_1b$Anxiolytics_core8[i] == 1 & main_08_1b$CDI_Anxiety_core8[i] == 0) {
    #If Anxiolytics_core8 = 1 and CDI_Anxiety_core8 = 0 then Anxiety_core8 = 1
    main_08_1b$Anxiety_core8[i] = 1
  } else if (main_08_1b$Anxiolytics_core8[i] == 0 & main_08_1b$CDI_Anxiety_core8[i] == 1) {
    #If Anxiolytics_core8 = 0 and CDI_Anxiety_core8 = 1 then Anxiety_core8 = 1
    main_08_1b$Anxiety_core8[i] = 1
  } else if (main_08_1b$Anxiolytics_core8[i] == 0 & main_08_1b$CDI_Anxiety_core8[i] == 0) {
    #If Anxiolytics_core8 = 0 and CDI_Anxiety_core8 = 0 then Anxiety_core8 = 0
    main_08_1b$Anxiety_core8[i] = 0
  } else if (main_08_1b$Anxiolytics_core8[i] == 9 & main_08_1b$CDI_Anxiety_core8[i] == 1) {
    #If Anxiolytics_core8 = NA and CDI_Anxiety_core8 = 1 then Anxiety_core8 = 1
    main_08_1b$Anxiety_core8[i] = 1
  } else if (main_08_1b$Anxiolytics_core8[i] == 9 & main_08_1b$CDI_Anxiety_core8[i] == 0) {
    #If Anxiolytics_core8 = NA and CDI_Anxiety_core8 = 0 then Anxiety_core8 = 0
    main_08_1b$Anxiety_core8[i] = 0
  } else if (main_08_1b$Anxiolytics_core8[i] == 1 & main_08_1b$CDI_Anxiety_core8[i] == 9) {
    #If Anxiolytics_core8 = 1 and CDI_Anxiety_core8 = NA then Anxiety_core8 = 1
    main_08_1b$Anxiety_core8[i] = 1
  } else if (main_08_1b$Anxiolytics_core8[i] == 0 & main_08_1b$CDI_Anxiety_core8[i] == 9) {
    #If Anxiolytics_core8 = 0 and CDI_Anxiety_core8 = NA then Anxiety_core8 = 0
    main_08_1b$Anxiety_core8[i] = 0
  } else if (main_08_1b$Anxiolytics_core8[i] == 9 & main_08_1b$CDI_Anxiety_core8[i] == 9) {
    #If Anxiolytics_core8 = NA and CDI_Anxiety_core8 = NA then Anxiety_core8 = NA
    main_08_1b$Anxiety_core8[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
main_08_1b[c("Anxiolytics_core8", "CDI_Anxiety_core8")] <- replace(main_08_1b[c("Anxiolytics_core8", "CDI_Anxiety_core8")], main_08_1b[c("Anxiolytics_core8", "CDI_Anxiety_core8")] == 9, NA)

#remove unneeded columns
final_08_1b <- subset(main_08_1b, select = -c(H376, H377))
#replace NA with "."
final_08_1b <- final_08_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#### Gen 2 Exam 9/Omni 1 Exam 9 ####
#J438, J439
exam_09_1b <- read_sas("e_exam_ex09_1b_0844.sas7bdat")
select_09_1b <- exam_09_1b[,c("id", "idtype", "j438", "j439")]
#create framid column
select_09_1b$framid <- with(select_09_1b, ifelse(idtype == 1, 80000 + id, 
                                                 ifelse(idtype == 7, 700000 + id, id)))

#Placeholder values, used for testing
select_09_1b$CDI_Depression_core9 = 9999
select_09_1b$Depression_core9 = 9999
select_09_1b$CDI_Anxiety_core9 = 9999
select_09_1b$Anxiety_core9 = 9999


for (i in 1:nrow(select_09_1b)) {
  if (is.na(select_09_1b$j438[i])) {
    #Unknown = NA
    select_09_1b$CDI_Depression_core9[i] = NA
  } else if (select_09_1b$j438[i] == 0 | select_09_1b$j438[i] == 2) {
    #No/Maybe = 0
    select_09_1b$CDI_Depression_core9[i] = 0
  } else {
    #Yes = 1
    select_09_1b$CDI_Depression_core9[i] = 1
  }
}


for (i in 1:nrow(select_09_1b)) {
  if (is.na(select_09_1b$j439[i])) {
    #Unknown = NA
    select_09_1b$CDI_Anxiety_core9[i] = NA
  } else if (select_09_1b$j439[i] == 0 | select_09_1b$j439[i] == 2) {
    #No/Maybe = 0
    select_09_1b$CDI_Anxiety_core9[i] = 0
  } else {
    #Yes = 1
    select_09_1b$CDI_Anxiety_core9[i] = 1
  }
}

#CES-D Gen 2 Exam 9/Omni 1 Exam 9
#negatively phrased questions
cesd_first_09_1b <- exam_09_1b[,c("id", "j727", "j728", "j729", "j731", "j732", 
                                  "j733", "j735", "j736", "j737", "j739", "j740", 
                                  "j741", "j743", "j744", "j745", "j746")]
#positively phrased questions
cesd_second_09_1b <- exam_09_1b[,c("id", "j730", "j734", "j738", "j742")]
#sum of negatively phrased questions
cesd_first_09_1b$first_sum <- rowSums(cesd_first_09_1b[2:17], na.rm=TRUE)
#sum of positively phrased questions
cesd_second_09_1b$j730_new <- (3 - cesd_second_09_1b$j730)
cesd_second_09_1b$j734_new <- (3 - cesd_second_09_1b$j734)
cesd_second_09_1b$j738_new <- (3 - cesd_second_09_1b$j738)
cesd_second_09_1b$j742_new <- (3 - cesd_second_09_1b$j742)
cesd_second_09_1b$second_sum <- rowSums(cesd_second_09_1b[6:9], na.rm=TRUE)
#add sums together
cesd_second_09_1b$first_sum <- cesd_first_09_1b$first_sum
cesd_second_09_1b$CESD_core9 <- cesd_second_09_1b$first_sum + cesd_second_09_1b$second_sum
select_09_1b$CESD_core9 <- cesd_second_09_1b$CESD_core9


#### Gen 2 Exam 9/Omni 1 Exam 9: ATC ####

#medications from Gen 2 Exam 9
meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
cod_09_1b <- meds_09_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#create framid column
cod_09_1b$framid <- with(cod_09_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
atc_framid_09_1b <- unique(cod_09_1b$framid)
med_found_09_1b <- data.frame(atc_framid_09_1b)

#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_09_1b['anxio_column'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% anxio_filtered_09_1b$id)
med_found_09_1b['hypno_column'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% hypno_filtered_09_1b$id)
med_found_09_1b['antidep_column'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% antidep_filtered_09_1b$id)

#standardizing column names
colnames(med_found_09_1b) <- c("framid", "Anxiolytics_core9", "Hypnotics_core9", "Antidepressants_core9")

#merge
main_09_1b <- merge(select_09_1b, med_found_09_1b, by = "framid", all = TRUE)


#### Gen 2 Exam 9/Omni 1 Exam 9: Logic ####

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
main_09_1b <- main_09_1b %>% mutate_at(c("Antidepressants_core9", "CDI_Depression_core9"), ~replace_na(.,9))

#for loop to determine depression derived variable
for (i in 1:nrow(main_09_1b)) {
  if (main_09_1b$Antidepressants_core9[i] == 1 & main_09_1b$CDI_Depression_core9[i] == 1) {
    #If Antidepressants_core9 = 1 and CDI_Depression_core9 = 1 then Depression_core9 = 1
    main_09_1b$Depression_core9[i] = 1
  } else if (main_09_1b$Antidepressants_core9[i] == 1 & main_09_1b$CDI_Depression_core9[i] == 0) {
    #If Antidepressants_core9 = 1 and CDI_Depression_core9 = 0 then Depression_core9 = 1
    main_09_1b$Depression_core9[i] = 1
  } else if (main_09_1b$Antidepressants_core9[i] == 0 & main_09_1b$CDI_Depression_core9[i] == 1) {
    #If Antidepressants_core9 = 0 and CDI_Depression_core9 = 1 then Depression_core9 = 1
    main_09_1b$Depression_core9[i] = 1
  } else if (main_09_1b$Antidepressants_core9[i] == 0 & main_09_1b$CDI_Depression_core9[i] == 0) {
    #If Antidepressants_core9 = 0 and CDI_Depression_core9 = 0 then Depression_core9 = 0
    main_09_1b$Depression_core9[i] = 0
  } else if (main_09_1b$Antidepressants_core9[i] == 9 & main_09_1b$CDI_Depression_core9[i] == 1) {
    #If Antidepressants_core9 = NA and CDI_Depression_core9 = 1 then Depression_core9 = 1
    main_09_1b$Depression_core9[i] = 1
  } else if (main_09_1b$Antidepressants_core9[i] == 9 & main_09_1b$CDI_Depression_core9[i] == 0) {
    #If Antidepressants_core9 = NA and CDI_Depression_core9 = 0 then Depression_core9 = 0
    main_09_1b$Depression_core9[i] = 0
  } else if (main_09_1b$Antidepressants_core9[i] == 1 & main_09_1b$CDI_Depression_core9[i] == 9) {
    #If Antidepressants_core9 = 1 and CDI_Depression_core9 = NA then Depression_core9 = 1
    main_09_1b$Depression_core9[i] = 1
  } else if (main_09_1b$Antidepressants_core9[i] == 0 & main_09_1b$CDI_Depression_core9[i] == 9) {
    #If Antidepressants_core9 = 0 and CDI_Depression_core9 = NA then Depression_core9 = 0
    main_09_1b$Depression_core9[i] = 0
  } else if (main_09_1b$Antidepressants_core9[i] == 9 & main_09_1b$CDI_Depression_core9[i] == 9) {
    #If Antidepressants_core9 = NA and CDI_Depression_core9 = NA then Depression_core9 = NA
    main_09_1b$Depression_core9[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
main_09_1b[c("Antidepressants_core9", "CDI_Depression_core9")] <- replace(main_09_1b[c("Antidepressants_core9", "CDI_Depression_core9")], main_09_1b[c("Antidepressants_core9", "CDI_Depression_core9")] == 9, NA)

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
main_09_1b <- main_09_1b %>% mutate_at(c("Anxiolytics_core9", "CDI_Anxiety_core9"), ~replace_na(.,9))

#for loop to determine Anxiety derived variable
for (i in 1:nrow(main_09_1b)) {
  if (main_09_1b$Anxiolytics_core9[i] == 1 & main_09_1b$CDI_Anxiety_core9[i] == 1) {
    #If Anxiolytics_core9 = 1 and CDI_Anxiety_core9 = 1 then Anxiety_core9 = 1
    main_09_1b$Anxiety_core9[i] = 1
  } else if (main_09_1b$Anxiolytics_core9[i] == 1 & main_09_1b$CDI_Anxiety_core9[i] == 0) {
    #If Anxiolytics_core9 = 1 and CDI_Anxiety_core9 = 0 then Anxiety_core9 = 1
    main_09_1b$Anxiety_core9[i] = 1
  } else if (main_09_1b$Anxiolytics_core9[i] == 0 & main_09_1b$CDI_Anxiety_core9[i] == 1) {
    #If Anxiolytics_core9 = 0 and CDI_Anxiety_core9 = 1 then Anxiety_core9 = 1
    main_09_1b$Anxiety_core9[i] = 1
  } else if (main_09_1b$Anxiolytics_core9[i] == 0 & main_09_1b$CDI_Anxiety_core9[i] == 0) {
    #If Anxiolytics_core9 = 0 and CDI_Anxiety_core9 = 0 then Anxiety_core9 = 0
    main_09_1b$Anxiety_core9[i] = 0
  } else if (main_09_1b$Anxiolytics_core9[i] == 9 & main_09_1b$CDI_Anxiety_core9[i] == 1) {
    #If Anxiolytics_core9 = NA and CDI_Anxiety_core9 = 1 then Anxiety_core9 = 1
    main_09_1b$Anxiety_core9[i] = 1
  } else if (main_09_1b$Anxiolytics_core9[i] == 9 & main_09_1b$CDI_Anxiety_core9[i] == 0) {
    #If Anxiolytics_core9 = NA and CDI_Anxiety_core9 = 0 then Anxiety_core9 = 0
    main_09_1b$Anxiety_core9[i] = 0
  } else if (main_09_1b$Anxiolytics_core9[i] == 1 & main_09_1b$CDI_Anxiety_core9[i] == 9) {
    #If Anxiolytics_core9 = 1 and CDI_Anxiety_core9 = NA then Anxiety_core9 = 1
    main_09_1b$Anxiety_core9[i] = 1
  } else if (main_09_1b$Anxiolytics_core9[i] == 0 & main_09_1b$CDI_Anxiety_core9[i] == 9) {
    #If Anxiolytics_core9 = 0 and CDI_Anxiety_core9 = NA then Anxiety_core9 = 0
    main_09_1b$Anxiety_core9[i] = 0
  } else if (main_09_1b$Anxiolytics_core9[i] == 9 & main_09_1b$CDI_Anxiety_core9[i] == 9) {
    #If Anxiolytics_core9 = NA and CDI_Anxiety_core9 = NA then Anxiety_core9 = NA
    main_09_1b$Anxiety_core9[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
main_09_1b[c("Anxiolytics_core9", "CDI_Anxiety_core9")] <- replace(main_09_1b[c("Anxiolytics_core9", "CDI_Anxiety_core9")], main_09_1b[c("Anxiolytics_core9", "CDI_Anxiety_core9")] == 9, NA)

#remove unneeded columns
final_09_1b <- subset(main_09_1b, select = -c(j438, j439))

#lowercase column names for merge
colnames(final_09_1b)[1:3] <- c("FRAMID", "ID", "IDTYPE")

#replace NA with "."
final_09_1b <- final_09_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Gen 2 Exam 10/Omni 1 Exam 5 ####
#K1015, K1016
exam_10_1b <- read_sas("e_exam_ex10_1b_1409.sas7bdat")
select_10_1b <- exam_10_1b[,c("id", "idtype", "K1015", "K1016")]
#create framid column
select_10_1b$framid <- with(select_10_1b, ifelse(idtype == 1, 80000 + id, 
                                                 ifelse(idtype == 7, 700000 + id, id)))

#Placeholder values, used for testing
select_10_1b$CDI_Depression_core10 = 9999
select_10_1b$Depression_core10 = 9999
select_10_1b$CDI_Anxiety_core10 = 9999
select_10_1b$Anxiety_core10 = 9999


for (i in 1:nrow(select_10_1b)) {
  if (is.na(select_10_1b$K1015[i])) {
    #Unknown = NA
    select_10_1b$CDI_Depression_core10[i] = NA
  } else if (select_10_1b$K1015[i] == 0 | select_10_1b$K1015[i] == 2) {
    #No/Maybe = 0
    select_10_1b$CDI_Depression_core10[i] = 0
  } else {
    #Yes = 1
    select_10_1b$CDI_Depression_core10[i] = 1
  }
}


for (i in 1:nrow(select_10_1b)) {
  if (is.na(select_10_1b$K1016[i])) {
    #Unknown = NA
    select_10_1b$CDI_Anxiety_core10[i] = NA
  } else if (select_10_1b$K1016[i] == 0 | select_10_1b$K1016[i] == 2) {
    #No/Maybe = 0
    select_10_1b$CDI_Anxiety_core10[i] = 0
  } else {
    #Yes = 1
    select_10_1b$CDI_Anxiety_core10[i] = 1
  }
}

#CES-D Gen 2 Exam 10/Omni 1 Exam 5
#negatively phrased questions
cesd_first_10_1b <- exam_10_1b[,c("id", "K1197", "K1198", "K1199",
                                  "K1201", "K1202", "K1203", 
                                  "K1205", "K1206", "K1207",
                                  "K1209", "K1200", "K1211",
                                  "K1213", "K1214", "K1215", "K1216")]
#positively phrased questions
cesd_second_10_1b <- exam_10_1b[,c("id", "K1200", "K1204", "K1208", "K1212")]
#sum of negatively phrased questions
cesd_first_10_1b$first_sum <- rowSums(cesd_first_10_1b[2:17], na.rm=TRUE)
#sum of positively phrased questions
cesd_second_10_1b$K1200_new <- (3 - cesd_second_10_1b$K1200)
cesd_second_10_1b$K1204_new <- (3 - cesd_second_10_1b$K1204)
cesd_second_10_1b$K1208_new <- (3 - cesd_second_10_1b$K1208)
cesd_second_10_1b$K1212_new <- (3 - cesd_second_10_1b$K1212)
cesd_second_10_1b$second_sum <- rowSums(cesd_second_10_1b[6:9], na.rm=TRUE)
#add sums together
cesd_second_10_1b$first_sum <- cesd_first_10_1b$first_sum
cesd_second_10_1b$CESD_core10 <- cesd_second_10_1b$first_sum + cesd_second_10_1b$second_sum
select_10_1b$CESD_core10 <- cesd_second_10_1b$CESD_core10


#### Gen 2 Exam 10/Omni 1 Exam 5: ATC ####

#medications from Gen 2 Exam 10
meds_10_1b <- read_sas("vr_meds_ex10_1b_1198.sas7bdat")
cod_10_1b <- meds_10_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#create framid column
cod_10_1b$framid <- with(cod_10_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anxio_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% anxio_atc_list))
hypno_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% hypno_atc_list))
antidep_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% antidep_atc_list))

#Constructing the output data frame
atc_framid_10_1b <- unique(cod_10_1b$framid)
med_found_10_1b <- data.frame(atc_framid_10_1b)

#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_10_1b['anxio_column'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% anxio_filtered_10_1b$id)
med_found_10_1b['hypno_column'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% hypno_filtered_10_1b$id)
med_found_10_1b['antidep_column'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% antidep_filtered_10_1b$id)

#standardizing column names
colnames(med_found_10_1b) <- c("framid", "Anxiolytics_core10", "Hypnotics_core10", "Antidepressants_core10")

#merge
main_10_1b <- merge(select_10_1b, med_found_10_1b, by = "framid", all = TRUE)


#### Gen 2 Exam 10/Omni 1 Exam 5: Logic ####

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
main_10_1b <- main_10_1b %>% mutate_at(c("Antidepressants_core10", "CDI_Depression_core10"), ~replace_na(.,9))

#for loop to determine depression derived variable
for (i in 1:nrow(main_10_1b)) {
  if (main_10_1b$Antidepressants_core10[i] == 1 & main_10_1b$CDI_Depression_core10[i] == 1) {
    #If Antidepressants_core10 = 1 and CDI_Depression_core10 = 1 then Depression_core10 = 1
    main_10_1b$Depression_core10[i] = 1
  } else if (main_10_1b$Antidepressants_core10[i] == 1 & main_10_1b$CDI_Depression_core10[i] == 0) {
    #If Antidepressants_core10 = 1 and CDI_Depression_core10 = 0 then Depression_core10 = 1
    main_10_1b$Depression_core10[i] = 1
  } else if (main_10_1b$Antidepressants_core10[i] == 0 & main_10_1b$CDI_Depression_core10[i] == 1) {
    #If Antidepressants_core10 = 0 and CDI_Depression_core10 = 1 then Depression_core10 = 1
    main_10_1b$Depression_core10[i] = 1
  } else if (main_10_1b$Antidepressants_core10[i] == 0 & main_10_1b$CDI_Depression_core10[i] == 0) {
    #If Antidepressants_core10 = 0 and CDI_Depression_core10 = 0 then Depression_core10 = 0
    main_10_1b$Depression_core10[i] = 0
  } else if (main_10_1b$Antidepressants_core10[i] == 9 & main_10_1b$CDI_Depression_core10[i] == 1) {
    #If Antidepressants_core10 = NA and CDI_Depression_core10 = 1 then Depression_core10 = 1
    main_10_1b$Depression_core10[i] = 1
  } else if (main_10_1b$Antidepressants_core10[i] == 9 & main_10_1b$CDI_Depression_core10[i] == 0) {
    #If Antidepressants_core10 = NA and CDI_Depression_core10 = 0 then Depression_core10 = 0
    main_10_1b$Depression_core10[i] = 0
  } else if (main_10_1b$Antidepressants_core10[i] == 1 & main_10_1b$CDI_Depression_core10[i] == 9) {
    #If Antidepressants_core10 = 1 and CDI_Depression_core10 = NA then Depression_core10 = 1
    main_10_1b$Depression_core10[i] = 1
  } else if (main_10_1b$Antidepressants_core10[i] == 0 & main_10_1b$CDI_Depression_core10[i] == 9) {
    #If Antidepressants_core10 = 0 and CDI_Depression_core10 = NA then Depression_core10 = 0
    main_10_1b$Depression_core10[i] = 0
  } else if (main_10_1b$Antidepressants_core10[i] == 9 & main_10_1b$CDI_Depression_core10[i] == 9) {
    #If Antidepressants_core10 = NA and CDI_Depression_core10 = NA then Depression_core10 = NA
    main_10_1b$Depression_core10[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
main_10_1b[c("Antidepressants_core10", "CDI_Depression_core10")] <- replace(main_10_1b[c("Antidepressants_core10", "CDI_Depression_core10")], main_10_1b[c("Antidepressants_core10", "CDI_Depression_core10")] == 9, NA)

#Replacing NA with placeholder value 9 in columns that will be used in logic avoids R treating those values differently
main_10_1b <- main_10_1b %>% mutate_at(c("Anxiolytics_core10", "CDI_Anxiety_core10"), ~replace_na(.,9))

#for loop to determine Anxiety derived variable
for (i in 1:nrow(main_10_1b)) {
  if (main_10_1b$Anxiolytics_core10[i] == 1 & main_10_1b$CDI_Anxiety_core10[i] == 1) {
    #If Anxiolytics_core10 = 1 and CDI_Anxiety_core10 = 1 then Anxiety_core10 = 1
    main_10_1b$Anxiety_core10[i] = 1
  } else if (main_10_1b$Anxiolytics_core10[i] == 1 & main_10_1b$CDI_Anxiety_core10[i] == 0) {
    #If Anxiolytics_core10 = 1 and CDI_Anxiety_core10 = 0 then Anxiety_core10 = 1
    main_10_1b$Anxiety_core10[i] = 1
  } else if (main_10_1b$Anxiolytics_core10[i] == 0 & main_10_1b$CDI_Anxiety_core10[i] == 1) {
    #If Anxiolytics_core10 = 0 and CDI_Anxiety_core10 = 1 then Anxiety_core10 = 1
    main_10_1b$Anxiety_core10[i] = 1
  } else if (main_10_1b$Anxiolytics_core10[i] == 0 & main_10_1b$CDI_Anxiety_core10[i] == 0) {
    #If Anxiolytics_core10 = 0 and CDI_Anxiety_core10 = 0 then Anxiety_core10 = 0
    main_10_1b$Anxiety_core10[i] = 0
  } else if (main_10_1b$Anxiolytics_core10[i] == 9 & main_10_1b$CDI_Anxiety_core10[i] == 1) {
    #If Anxiolytics_core10 = NA and CDI_Anxiety_core10 = 1 then Anxiety_core10 = 1
    main_10_1b$Anxiety_core10[i] = 1
  } else if (main_10_1b$Anxiolytics_core10[i] == 9 & main_10_1b$CDI_Anxiety_core10[i] == 0) {
    #If Anxiolytics_core10 = NA and CDI_Anxiety_core10 = 0 then Anxiety_core10 = 0
    main_10_1b$Anxiety_core10[i] = 0
  } else if (main_10_1b$Anxiolytics_core10[i] == 1 & main_10_1b$CDI_Anxiety_core10[i] == 9) {
    #If Anxiolytics_core10 = 1 and CDI_Anxiety_core10 = NA then Anxiety_core10 = 1
    main_10_1b$Anxiety_core10[i] = 1
  } else if (main_10_1b$Anxiolytics_core10[i] == 0 & main_10_1b$CDI_Anxiety_core10[i] == 9) {
    #If Anxiolytics_core10 = 0 and CDI_Anxiety_core10 = NA then Anxiety_core10 = 0
    main_10_1b$Anxiety_core10[i] = 0
  } else if (main_10_1b$Anxiolytics_core10[i] == 9 & main_10_1b$CDI_Anxiety_core10[i] == 9) {
    #If Anxiolytics_core10 = NA and CDI_Anxiety_core10 = NA then Anxiety_core10 = NA
    main_10_1b$Anxiety_core10[i] = NA
  }
}

#returning NA values to columns, removing placeholder value
main_10_1b[c("Anxiolytics_core10", "CDI_Anxiety_core10")] <- replace(main_10_1b[c("Anxiolytics_core10", "CDI_Anxiety_core10")], main_10_1b[c("Anxiolytics_core10", "CDI_Anxiety_core10")] == 9, NA)

#remove unneeded columns
final_10_1b <- subset(main_10_1b, select = -c(K1015, K1016))

#lowercase column names for merge
colnames(final_10_1b)[1:3] <- c("FRAMID", "ID", "IDTYPE")

#replace NA with "."
final_10_1b <- final_10_1b %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Final Merge ####

#list all exam data frames
gen2_all_list <- list(select_01_1, select_02_1, select_03_1, select_04_1, select_05_1b, select_06_1,
                      select_07_1b, final_08_1b, final_09_1b, final_10_1b)
#join all exam data frames together
gen2_joined <- gen2_all_list %>% reduce(full_join, by = c("IDTYPE", "ID", "FRAMID"))

#reorder columns
gen2_reordered <- gen2_joined[, c("IDTYPE", "ID", "FRAMID", 
                                  "Antidepressants_core3", "Antidepressants_core4", "Antidepressants_core5", 
                                  "Antidepressants_core6", "Antidepressants_core7", "Antidepressants_core8", 
                                  "Antidepressants_core9", "Antidepressants_core10",
                                  "CDI_Depression_core7", "CDI_Depression_core8", "CDI_Depression_core9",
                                  "CDI_Depression_core10",
                                  "CESD_core6", "CESD_core7", "CESD_core9", "CESD_core10",
                                  "Depression_core7", "Depression_core8", "Depression_core9",
                                  "Depression_core10",
                                  "Anxiolytics_core1", "Anxiolytics_core2", "Anxiolytics_core3", 
                                  "Anxiolytics_core4", "Anxiolytics_core5", "Anxiolytics_core6", 
                                  "Anxiolytics_core7", "Anxiolytics_core8", "Anxiolytics_core9",
                                  "Anxiolytics_core10",
                                  "CDI_Anxiety_core8", "CDI_Anxiety_core9", "CDI_Anxiety_core10",
                                  "Anxiety_core8", "Anxiety_core9", "Anxiety_core10",
                                  "Hypnotics_core2", "Hypnotics_core3", "Hypnotics_core4", 
                                  "Hypnotics_core5", "Hypnotics_core6", "Hypnotics_core7", 
                                  "Hypnotics_core8", "Hypnotics_core9", "Hypnotics_core10")] 

#writing final CSV to file
write.csv(gen2_reordered, file = "Gen2_Anxio_Hyp_Depr_Full.csv", row.names = FALSE)

