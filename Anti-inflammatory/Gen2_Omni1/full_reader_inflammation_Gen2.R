# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 inflammation drugs data abstraction source code
# ******************************************************************************************************************************************
# 
# Created by Michael Cummings
# Last updated: January 2023
# 
# 
# The purpose of this R code is to abstract data from questions related to inflammation drugs in Gen 2/Omni 1,
# exams 3-9.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
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
# 2)  Inflammation ATC information - ATC3_InflammationCodes.xlsx
# 
# 3)  Full ATC medication info for patients
# vr_meds_ex03_7_0535.sas7bdat (Omni 1 Exam 3)
# vr_meds_ex08_1_0280_v1.sas7bdat (Gen 2 Exam 8)
# vr_meds_ex09_1b_0879.sas7bdat (Gen 2 Exam 9 and Omni 1 Exam 9)
# vr_meds_ex10_1b_1198.sas7bdat (Gen 2 Exam 10 and Omni 1 Exam 5)
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


#### Gen 2 Exam 3 ####
#ex03_1 (GEN 2 Exam 3)
#C27, C32, C33
exam_03_1 <- read_sas("e_exam_ex03_1_0081.sas7bdat")
select_03_1 <- exam_03_1[,c("ID", "C27", "C32", "C33")]
#add further id columns
select_03_1 <- select_03_1 %>% mutate(IDTYPE = 1, .before = ID)
select_03_1 <- select_03_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Replacing NA with 10 avoids R treating those values differently
select_03_1 <- select_03_1 %>% mutate_at(c("C27", "C32", "C33"), ~replace_na(.,10))

#Placeholder values, used for testing
select_03_1$all_inflammation_core3 = 9999
select_03_1$steroids_core3 = 9999
select_03_1$nsaids_core3 = 9999

#all inflammation
for (i in 1:nrow(select_03_1)) {
  if (select_03_1$C27[i] %in% c(1,2) | select_03_1$C32[i] %in% c(1,2)
      | select_03_1$C33[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_03_1$all_inflammation_core3[i] = 1
  } else if (select_03_1$C27[i] %in% c(0,3) & select_03_1$C32[i] %in% c(0,3)
             & select_03_1$C33[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_03_1$all_inflammation_core3[i] = 0
  } else {
    #Else NA
    select_03_1$all_inflammation_core3[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_03_1)) {
  if (select_03_1$C32[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_03_1$steroids_core3[i] = 1
  } else if (select_03_1$C32[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_03_1$steroids_core3[i] = 0
  } else {
    #Else NA
    select_03_1$steroids_core3[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_03_1)) {
  if (select_03_1$C33[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_03_1$nsaids_core3[i] = 1
  } else if (select_03_1$C33[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_03_1$nsaids_core3[i] = 0
  } else {
    #Else NA
    select_03_1$nsaids_core3[i] = NA
  }
}


#### Gen 2 Exam 4 ####
#ex04_1 (GEN 2 Exam 3)
#D031, D032, D039, D040
exam_04_1 <- read_sas("e_exam_ex04_1_0082.sas7bdat")
select_04_1 <- exam_04_1[,c("ID", "D031", "D032", "D039", "D040")]
#add further id columns
select_04_1 <- select_04_1 %>% mutate(IDTYPE = 1, .before = ID)
select_04_1 <- select_04_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Replacing NA with 10 avoids R treating those values differently
select_04_1 <- select_04_1 %>% mutate_at(c("D031", "D032", "D039", "D040"), ~replace_na(.,10))

#Placeholder values, used for testing
select_04_1$all_inflammation_core4 = 9999
select_04_1$steroids_core4 = 9999
select_04_1$nsaids_core4 = 9999

#all inflammation
for (i in 1:nrow(select_04_1)) {
  if (select_04_1$D031[i] %in% c(1,2) | select_04_1$D032[i] %in% c(1,2)
      | select_04_1$D039[i] %in% c(1,2) | select_04_1$D040[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_04_1$all_inflammation_core4[i] = 1
  } else if (select_04_1$D031[i] %in% c(0,3) & select_04_1$D032[i] %in% c(0,3)
             & select_04_1$D039[i] %in% c(0,3) & select_04_1$D040[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_04_1$all_inflammation_core4[i] = 0
  } else {
    #Else NA
    select_04_1$all_inflammation_core4[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_04_1)) {
  if (select_04_1$D039[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_04_1$steroids_core4[i] = 1
  } else if (select_04_1$D039[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_04_1$steroids_core4[i] = 0
  } else {
    #Else NA
    select_04_1$steroids_core4[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_04_1)) {
  if (select_04_1$D040[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_04_1$nsaids_core4[i] = 1
  } else if (select_04_1$D040[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_04_1$nsaids_core4[i] = 0
  } else {
    #Else NA
    select_04_1$nsaids_core4[i] = NA
  }
}


#### Gen 2 Exam 5 / Omni 1 Exam 1 ####
#ex05_1 (GEN 2 Exam 5)
#E250, E251, E258, E259
exam_05_1 <- read_sas("e_exam_ex05_1_0083.sas7bdat")
select_05_1 <- exam_05_1[,c("IDTYPE", "ID", "E250", "E251", "E258", "E259")]
#add further id columns
select_05_1 <- select_05_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Omni 1 Exam 1
exam_01_7 <- read_sas("e_exam_ex01_7_0020.sas7bdat")
select_01_7 <- exam_01_7[,c("idtype", "id", "e250", "e251", "e258", "e259")]
#add further id columns
select_01_7 <- select_01_7 %>% mutate(framid = (700000 + id), .after = id)

#Combine sections into one data frame
colnames(select_01_7) <- colnames(select_05_1)
select_05_1b <- rbind(select_05_1, select_01_7)

#Replacing NA with 10 avoids R treating those values differently
select_05_1b <- select_05_1b %>% mutate_at(c("E250", "E251", "E258", "E259"), ~replace_na(.,10))

#Placeholder values, used for testing
select_05_1b$all_inflammation_core5 = 9999
select_05_1b$steroids_core5 = 9999
select_05_1b$nsaids_core5 = 9999

#all inflammation
for (i in 1:nrow(select_05_1b)) {
  if (select_05_1b$E250[i] %in% c(1,2) | select_05_1b$E251[i] %in% c(1,2)
      | select_05_1b$E258[i] %in% c(1,2) | select_05_1b$E259[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_05_1b$all_inflammation_core5[i] = 1
  } else if (select_05_1b$E250[i] %in% c(0,3) & select_05_1b$E251[i] %in% c(0,3)
             & select_05_1b$E258[i] %in% c(0,3) & select_05_1b$E259[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_05_1b$all_inflammation_core5[i] = 0
  } else {
    #Else NA
    select_05_1b$all_inflammation_core5[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_05_1b)) {
  if (select_05_1b$E258[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_05_1b$steroids_core5[i] = 1
  } else if (select_05_1b$E258[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_05_1b$steroids_core5[i] = 0
  } else {
    #Else NA
    select_05_1b$steroids_core5[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_05_1b)) {
  if (select_05_1b$E259[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_05_1b$nsaids_core5[i] = 1
  } else if (select_05_1b$E259[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_05_1b$nsaids_core5[i] = 0
  } else {
    #Else NA
    select_05_1b$nsaids_core5[i] = NA
  }
}


#### Gen 2 Exam 6 ####
#ex06_1 (GEN 2 Exam 6)
#F214, F215, F222, F223
exam_06_1 <- read_sas("e_exam_ex06_1_0084.sas7bdat")
select_06_1 <- exam_06_1[,c("ID", "F214", "F215", "F222", "F223")]
#add further id columns
select_06_1 <- select_06_1 %>% mutate(IDTYPE = 1, .before = ID)
select_06_1 <- select_06_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Replacing NA with 10 avoids R treating those values differently
select_06_1 <- select_06_1 %>% mutate_at(c("F214", "F215", "F222", "F223"), ~replace_na(.,10))

#Placeholder values, used for testing
select_06_1$all_inflammation_core6 = 9999
select_06_1$steroids_core6 = 9999
select_06_1$nsaids_core6 = 9999

#all inflammation
for (i in 1:nrow(select_06_1)) {
  if (select_06_1$F214[i] %in% c(1,2) | select_06_1$F215[i] %in% c(1,2)
      | select_06_1$F222[i] %in% c(1,2) | select_06_1$F223[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_06_1$all_inflammation_core6[i] = 1
  } else if (select_06_1$F214[i] %in% c(0,3) & select_06_1$F215[i] %in% c(0,3)
             & select_06_1$F222[i] %in% c(0,3) & select_06_1$F223[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_06_1$all_inflammation_core6[i] = 0
  } else {
    #Else NA
    select_06_1$all_inflammation_core6[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_06_1)) {
  if (select_06_1$F222[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_06_1$steroids_core6[i] = 1
  } else if (select_06_1$F222[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_06_1$steroids_core6[i] = 0
  } else {
    #Else NA
    select_06_1$steroids_core6[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_06_1)) {
  if (select_06_1$F223[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_06_1$nsaids_core6[i] = 1
  } else if (select_06_1$F223[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_06_1$nsaids_core6[i] = 0
  } else {
    #Else NA
    select_06_1$nsaids_core6[i] = NA
  }
}

#### Gen 2 Exam 7 / Omni 1 Exam 2 ####
#ex07_1 (GEN 2 Exam 7)
#G046, G047, G061, G062, G679, G680, G681
exam_07_1 <- read_sas("e_exam_ex07_1_0085_v1.sas7bdat")
select_07_1 <- exam_07_1[,c("IDTYPE", "ID", "G046", "G047", "G061", "G062", "G679", "G680", "G681")]
#add further id columns
select_07_1 <- select_07_1 %>% mutate(FRAMID = (80000 + ID), .after = ID)

#Omni 1 Exam 2
exam_02_7 <- read_sas("e_exam_ex02_7_0003.sas7bdat")
select_02_7 <- exam_02_7[,c("idtype", "id", "g046", "g047", "g061", "g062", "g679", "g680", "g681")]
#add further id columns
select_02_7 <- select_02_7 %>% mutate(framid = (700000 + id), .after = id)

#Combine sections into one data frame
colnames(select_02_7) <- colnames(select_07_1)
select_07_1b <- rbind(select_07_1, select_02_7)

#Replacing NA with 10 avoids R treating those values differently
select_07_1b <- select_07_1b %>% mutate_at(c("G046", "G047", "G061", "G062"), ~replace_na(.,10))
select_07_1b <- select_07_1b %>% mutate_at(c("G679", "G680", "G681"), ~replace_na(.,0))

#error checking
#table(select_07_1b$G679 == 0 & select_07_1b$G680 == 1) #no cases
#table(select_07_1b$G679 == 0 & select_07_1b$G681 == 1) #no cases


#Placeholder values, used for testing
select_07_1b$all_inflammation_core7 = 9999
select_07_1b$steroids_core7 = 9999
select_07_1b$nsaids_core7 = 9999

#all inflammation
for (i in 1:nrow(select_07_1b)) {
  if (select_07_1b$G046[i] %in% c(1,2) | select_07_1b$G047[i] %in% c(1,2)
      | select_07_1b$G061[i] %in% c(1,2) | select_07_1b$G062[i] %in% c(1,2)
      | select_07_1b$G680[i] == 1 | select_07_1b$G681[i] == 1) {
    #any in (1,2) then 1
    select_07_1b$all_inflammation_core7[i] = 1
  } else if (select_07_1b$G046[i] %in% c(0,3) & select_07_1b$G047[i] %in% c(0,3)
             & select_07_1b$G061[i] %in% c(0,3) & select_07_1b$G062[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_07_1b$all_inflammation_core7[i] = 0
  } else {
    #Else NA
    select_07_1b$all_inflammation_core7[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_07_1b)) {
  if (select_07_1b$G061[i] %in% c(1,2) | select_07_1b$G680[i] == 1 | select_07_1b$G681[i] == 1) {
    #any in (1,2) then 1
    select_07_1b$steroids_core7[i] = 1
  } else if (select_07_1b$G061[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_07_1b$steroids_core7[i] = 0
  } else {
    #Else NA
    select_07_1b$steroids_core7[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_07_1b)) {
  if (select_07_1b$G062[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_07_1b$nsaids_core7[i] = 1
  } else if (select_07_1b$G062[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_07_1b$nsaids_core7[i] = 0
  } else {
    #Else NA
    select_07_1b$nsaids_core7[i] = NA
  }
}


#### Gen 2 Exam 8 / Omni 1 Exam 3 ####
#Gen 2 Exam 8
exam_08_1 <- read_sas("e_exam_ex08_1_0005.sas7bdat")
uniq_08_1 <- exam_08_1[,c("ID", "IDTYPE")]
#create framid column
uniq_08_1$FRAMID <- (80000 + uniq_08_1$ID)

#Omni 1 Exam 3
exam_03_7 <- read_sas("e_exam_ex03_7_0426.sas7bdat")
uniq_03_7 <- exam_03_7[,c("id", "idtype")]
#create framid column
uniq_03_7$framid <- (700000 + uniq_03_7$id)

#Combine core sections into one data frame
colnames(uniq_08_1) <- colnames(uniq_03_7)
uniq_08_1b <- do.call("rbind", list(uniq_08_1, uniq_03_7))


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
anti_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
atc_framid_08_1b <- unique(cod_08_1b$framid)
med_found_08_1b <- data.frame(atc_framid_08_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_08_1b['anti_column'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% anti_filtered_08_1b$framid)
med_found_08_1b['nsaid_column'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% nsaid_filtered_08_1b$framid)
med_found_08_1b['steroid_column'] <- as.integer(med_found_08_1b$atc_framid_08_1b %in% steroid_filtered_08_1b$framid)

#standardizing column names
colnames(med_found_08_1b) <- c("framid", "all_inflammation_core8", "nsaids_core8", "steroids_core8")
select_08_1b <- left_join(uniq_08_1b, med_found_08_1b, by = "framid")
colnames(select_08_1b)[1:3] <- c("ID", "IDTYPE", "FRAMID")


#### Gen 2 Exam 9 / Omni 1 Exam 9 ####
#Gen 2 Exam 9/Omni 1 Exam 9
exam_09_1b <- read_sas("e_exam_ex09_1b_0844.sas7bdat")
uniq_09_1b <- exam_09_1b[,c("id", "idtype")]
#create framid column
uniq_09_1b$framid <- with(uniq_09_1b, ifelse(idtype == 1, 80000 + id, 
                                             ifelse(idtype == 7, 700000 + id, id)))


meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
cod_09_1b <- meds_09_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#create framid column
cod_09_1b$framid <- with(cod_09_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Exam 9
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
atc_framid_09_1b <- unique(cod_09_1b$framid)
med_found_09_1b <- data.frame(atc_framid_09_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_09_1b['anti_column'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% anti_filtered_09_1b$framid)
med_found_09_1b['nsaid_column'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% nsaid_filtered_09_1b$framid)
med_found_09_1b['steroid_column'] <- as.integer(med_found_09_1b$atc_framid_09_1b %in% steroid_filtered_09_1b$framid)

#standardizing column names
colnames(med_found_09_1b) <- c("framid", "all_inflammation_core9", "nsaids_core9", "steroids_core9")
select_09_1b <- left_join(uniq_09_1b, med_found_09_1b, by = "framid")
colnames(select_09_1b)[1:3] <- c("ID", "IDTYPE", "FRAMID")


#### Gen 2 Exam 10 / Omni 1 Exam 5 ####
#Gen 2 Exam 10/Omni 1 Exam 5
exam_10_1b <- read_sas("e_exam_ex10_1b_1409.sas7bdat")
uniq_10_1b <- exam_10_1b[,c("id", "idtype")]
#create framid column
uniq_10_1b$framid <- with(uniq_10_1b, ifelse(idtype == 1, 80000 + id, 
                                             ifelse(idtype == 7, 700000 + id, id)))


meds_10_1b <- read_sas("vr_meds_ex10_1b_1198.sas7bdat")
cod_10_1b <- meds_10_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#create framid column
cod_10_1b$framid <- with(cod_10_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#Exam 10
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
atc_framid_10_1b <- unique(cod_10_1b$framid)
med_found_10_1b <- data.frame(atc_framid_10_1b)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_10_1b['anti_column'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% anti_filtered_10_1b$framid)
med_found_10_1b['nsaid_column'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% nsaid_filtered_10_1b$framid)
med_found_10_1b['steroid_column'] <- as.integer(med_found_10_1b$atc_framid_10_1b %in% steroid_filtered_10_1b$framid)

#standardizing column names
colnames(med_found_10_1b) <- c("framid", "all_inflammation_core10", "nsaids_core10", "steroids_core10")
select_10_1b <- left_join(uniq_10_1b, med_found_10_1b, by = "framid")
colnames(select_10_1b)[1:3] <- c("ID", "IDTYPE", "FRAMID")


#### Final Merge ####
#Joining columns together

df_list <- list(select_03_1, select_04_1, select_05_1b, select_06_1, select_07_1b, select_08_1b, select_09_1b, select_10_1b)

df_joined <- df_list %>% reduce(full_join, by=c("ID", "IDTYPE", "FRAMID"))
#Ordering the columns, they can be out of order after the join
df_ordered <- df_joined %>% arrange(as.numeric(FRAMID))

df_final <- df_ordered[,c("ID", "IDTYPE", "FRAMID", "all_inflammation_core3", "nsaids_core3", "steroids_core3",
                          "all_inflammation_core4", "nsaids_core4", "steroids_core4",
                          "all_inflammation_core5", "nsaids_core5", "steroids_core5",
                          "all_inflammation_core6", "nsaids_core6", "steroids_core6",
                          "all_inflammation_core7", "nsaids_core7", "steroids_core7",
                          "all_inflammation_core8", "nsaids_core8", "steroids_core8",
                          "all_inflammation_core9", "nsaids_core9", "steroids_core9",
                          "all_inflammation_core10", "nsaids_core10", "steroids_core10")]

#write final CSV containing abstracted data
write.csv(df_final, file = "Gen2_all_inflammation_core.csv", row.names = FALSE)



