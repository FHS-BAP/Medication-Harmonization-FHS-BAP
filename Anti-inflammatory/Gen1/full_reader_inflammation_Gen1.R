# ******************************************************************************************************************************************
# Introduction to Gen 1 inflammation drugs data abstraction source code
# ******************************************************************************************************************************************
# 
# Created by Michael Cummings
# Last updated: January 2023
# 
# 
# The purpose of this R code is to abstract data from questions related to inflammation drugs in Gen 1, exams 17-32.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
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
# 
# 2)  Inflammation ATC information - ATC3_InflammationCodes.xlsx
# 
# 3)  Full ATC medication info for patients
# vr_meds_ex28_0_0441.sas7bdat (ATC Info Exam 28)
# vr_meds_ex31_0_0763.sas7bdat (ATC Info Exams 29/30/31)
# vr_meds_ex32_0_0880.sas7bdat (ATC Info Exam 32)
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")

library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #library for reading .xlsx files


#### Drug Lists ####
#Meds 28
meds_28 <- read_sas("vr_meds_ex28_0_0441.sas7bdat")
cod_28 <- meds_28[,c("ID", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Reading joined 29/30/31 drugs file
meds_29_30_31 <- read_sas("vr_meds_ex31_0_0763.sas7bdat")

#Meds 29
meds_29 <- subset(meds_29_30_31, exam == 29)
cod_29 <- meds_29[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Meds 30
meds_30 <- subset(meds_29_30_31, exam == 30)
cod_30 <- meds_30[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Meds 31
meds_31 <- subset(meds_29_30_31, exam == 31)
cod_31 <- meds_31[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Meds 32
meds_32 <- read_sas("vr_meds_ex32_0_0880.sas7bdat")
cod_32 <- meds_32[,c("id", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#reading three drug lists from excel spreadsheet
anti_codes <- read_excel("ATC3_InflammationCodes.xlsx", sheet = "All_inflammation_NoASA")
anti_atc_list <- na.omit(anti_codes$`ATC CODE FOR MEDICATION`)
nsaid_codes <- read_excel("ATC3_InflammationCodes.xlsx", sheet = "NSAIDS")
nsaid_atc_list <- nsaid_codes$`ATC Code`
steroid_codes <- read_excel("ATC3_InflammationCodes.xlsx", sheet = "Steroids(Glucocorticoids)")
steroid_atc_list <- steroid_codes$H02AB


#### Individual exams ####

#ex17_0 (GEN 1 Exam 17)
#FJ24, FJ29
exam_17_0 <- read_sas("e_exam_ex17_0_0065.sas7bdat")
select_17_0 <- exam_17_0[,c("ID", "FJ24", "FJ29")]

#Replacing NA with 10 avoids R treating those values differently
select_17_0 <- select_17_0 %>% mutate_at(c("FJ24", "FJ29"), ~replace_na(.,10))

#Placeholder values, used for testing
select_17_0$all_inflammation_core17 = 9999
select_17_0$steroids_core17 = 9999

#all inflammation
for (i in 1:nrow(select_17_0)) {
  if (select_17_0$FJ24[i] %in% c(1,2) | select_17_0$FJ29[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_17_0$all_inflammation_core17[i] = 1
  } else if (select_17_0$FJ24[i] %in% c(0,3) & select_17_0$FJ29[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_17_0$all_inflammation_core17[i] = 0
  } else {
    #Else NA
    select_17_0$all_inflammation_core17[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_17_0)) {
  if (select_17_0$FJ29[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_17_0$steroids_core17[i] = 1
  } else if (select_17_0$FJ29[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_17_0$steroids_core17[i] = 0
  } else {
    #Else NA
    select_17_0$steroids_core17[i] = NA
  }
}


#ex18_0 (GEN 1 Exam 18)
#FK104, FK114
exam_18_0 <- read_sas("e_exam_ex18_0_0066.sas7bdat")
select_18_0 <- exam_18_0[,c("ID", "FK104", "FK114")]

#Replacing NA with 10 avoids R treating those values differently
select_18_0 <- select_18_0 %>% mutate_at(c("FK104", "FK114"), ~replace_na(.,10))

#Placeholder values, used for testing
select_18_0$all_inflammation_core18 = 9999
select_18_0$steroids_core18 = 9999

#all inflammation
for (i in 1:nrow(select_18_0)) {
  if (select_18_0$FK104[i] %in% c(1,2) | select_18_0$FK114[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_18_0$all_inflammation_core18[i] = 1
  } else if (select_18_0$FK104[i] %in% c(0,3) & select_18_0$FK114[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_18_0$all_inflammation_core18[i] = 0
  } else {
    #Else NA
    select_18_0$all_inflammation_core18[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_18_0)) {
  if (select_18_0$FK114[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_18_0$steroids_core18[i] = 1
  } else if (select_18_0$FK114[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_18_0$steroids_core18[i] = 0
  } else {
    #Else NA
    select_18_0$steroids_core18[i] = NA
  }
}


#ex19_0 (GEN 1 Exam 19)
#FL140, FL141, FL148, FL149
exam_19_0 <- read_sas("e_exam_ex19_0_0067.sas7bdat")
select_19_0 <- exam_19_0[,c("ID", "FL140", "FL141", "FL148", "FL149")]

#Replacing NA with 10 avoids R treating those values differently
select_19_0 <- select_19_0 %>% mutate_at(c("FL140", "FL141", "FL148", "FL149"), ~replace_na(.,10))

#Placeholder values, used for testing
select_19_0$all_inflammation_core19 = 9999
select_19_0$steroids_core19 = 9999
select_19_0$nsaids_core19 = 9999

#all inflammation
for (i in 1:nrow(select_19_0)) {
  if (select_19_0$FL140[i] %in% c(1,2) | select_19_0$FL141[i] %in% c(1,2)
      | select_19_0$FL148[i] %in% c(1,2) | select_19_0$FL149[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_19_0$all_inflammation_core19[i] = 1
  } else if (select_19_0$FL140[i] %in% c(0,3) & select_19_0$FL141[i] %in% c(0,3)
             & select_19_0$FL148[i] %in% c(0,3) & select_19_0$FL149[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_19_0$all_inflammation_core19[i] = 0
  } else {
    #Else NA
    select_19_0$all_inflammation_core19[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_19_0)) {
  if (select_19_0$FL148[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_19_0$steroids_core19[i] = 1
  } else if (select_19_0$FL148[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_19_0$steroids_core19[i] = 0
  } else {
    #Else NA
    select_19_0$steroids_core19[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_19_0)) {
  if (select_19_0$FL149[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_19_0$nsaids_core19[i] = 1
  } else if (select_19_0$FL149[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_19_0$nsaids_core19[i] = 0
  } else {
    #Else NA
    select_19_0$nsaids_core19[i] = NA
  }
}


#ex20_0 (GEN 1 Exam 20)
#FM178, FM179, FM186, FM187
exam_20_0 <- read_sas("e_exam_ex20_0_0068.sas7bdat")
select_20_0 <- exam_20_0[,c("ID", "FM178", "FM179", "FM186", "FM187")]

#Replacing NA with 10 avoids R treating those values differently
select_20_0 <- select_20_0 %>% mutate_at(c("FM178", "FM179", "FM186", "FM187"), ~replace_na(.,10))

#Placeholder values, used for testing
select_20_0$all_inflammation_core20 = 9999
select_20_0$steroids_core20 = 9999
select_20_0$nsaids_core20 = 9999

#all inflammation
for (i in 1:nrow(select_20_0)) {
  if (select_20_0$FM178[i] %in% c(1,2) | select_20_0$FM179[i] %in% c(1,2)
      | select_20_0$FM186[i] %in% c(1,2) | select_20_0$FM187[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_20_0$all_inflammation_core20[i] = 1
  } else if (select_20_0$FM178[i] %in% c(0,3) & select_20_0$FM179[i] %in% c(0,3)
             & select_20_0$FM186[i] %in% c(0,3) & select_20_0$FM187[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_20_0$all_inflammation_core20[i] = 0
  } else {
    #Else NA
    select_20_0$all_inflammation_core20[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_20_0)) {
  if (select_20_0$FM186[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_20_0$steroids_core20[i] = 1
  } else if (select_20_0$FM186[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_20_0$steroids_core20[i] = 0
  } else {
    #Else NA
    select_20_0$steroids_core20[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_20_0)) {
  if (select_20_0$FM187[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_20_0$nsaids_core20[i] = 1
  } else if (select_20_0$FM187[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_20_0$nsaids_core20[i] = 0
  } else {
    #Else NA
    select_20_0$nsaids_core20[i] = NA
  }
}


#ex21_0 (GEN 1 Exam 21)
#FN117, FN118, FN125, FN126
exam_21_0 <- read_sas("e_exam_ex21_0_0069.sas7bdat")
select_21_0 <- exam_21_0[,c("ID", "FN117", "FN118", "FN125", "FN126")]

#Replacing NA with 10 avoids R treating those values differently
select_21_0 <- select_21_0 %>% mutate_at(c("FN117", "FN118", "FN125", "FN126"), ~replace_na(.,10))

#Placeholder values, used for testing
select_21_0$all_inflammation_core21 = 9999
select_21_0$steroids_core21 = 9999
select_21_0$nsaids_core21 = 9999

#all inflammation
for (i in 1:nrow(select_21_0)) {
  if (select_21_0$FN117[i] %in% c(1,2) | select_21_0$FN118[i] %in% c(1,2)
      | select_21_0$FN125[i] %in% c(1,2) | select_21_0$FN126[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_21_0$all_inflammation_core21[i] = 1
  } else if (select_21_0$FN117[i] %in% c(0,3) & select_21_0$FN118[i] %in% c(0,3)
             & select_21_0$FN125[i] %in% c(0,3) & select_21_0$FN126[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_21_0$all_inflammation_core21[i] = 0
  } else {
    #Else NA
    select_21_0$all_inflammation_core21[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_21_0)) {
  if (select_21_0$FN125[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_21_0$steroids_core21[i] = 1
  } else if (select_21_0$FN125[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_21_0$steroids_core21[i] = 0
  } else {
    #Else NA
    select_21_0$steroids_core21[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_21_0)) {
  if (select_21_0$FN126[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_21_0$nsaids_core21[i] = 1
  } else if (select_21_0$FN126[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_21_0$nsaids_core21[i] = 0
  } else {
    #Else NA
    select_21_0$nsaids_core21[i] = NA
  }
}


#ex22_0 (GEN 1 Exam 22)
#FO124, FO125, FO132, FO133, FO134
exam_22_0 <- read_sas("e_exam_ex22_0_0070.sas7bdat")
select_22_0 <- exam_22_0[,c("ID", "FO124", "FO125", "FO132", "FO133", "FO134")]

#Replacing NA with 10 avoids R treating those values differently
select_22_0 <- select_22_0 %>% mutate_at(c("FO124", "FO125", "FO132", "FO133", "FO134"), ~replace_na(.,10))

#Placeholder values, used for testing
select_22_0$all_inflammation_core22 = 9999
select_22_0$steroids_core22 = 9999
select_22_0$nsaids_core22 = 9999

#all inflammation
for (i in 1:nrow(select_22_0)) {
  if (select_22_0$FO124[i] %in% c(1,2) | select_22_0$FO125[i] %in% c(1,2)
      | select_22_0$FO132[i] %in% c(1,2) | select_22_0$FO133[i] %in% c(1,2)
      | select_22_0$FO134[i] == 1) {
    #any in (1,2) then 1
    select_22_0$all_inflammation_core22[i] = 1
  } else if (select_22_0$FO124[i] %in% c(0,3) & select_22_0$FO125[i] %in% c(0,3)
             & select_22_0$FO132[i] %in% c(0,3) & select_22_0$FO133[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_22_0$all_inflammation_core22[i] = 0
  } else {
    #Else NA
    select_22_0$all_inflammation_core22[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_22_0)) {
  if (select_22_0$FO132[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_22_0$steroids_core22[i] = 1
  } else if (select_22_0$FO132[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_22_0$steroids_core22[i] = 0
  } else {
    #Else NA
    select_22_0$steroids_core22[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_22_0)) {
  if (select_22_0$FO133[i] %in% c(1,2) | select_22_0$FO134[i] == 1) {
    #any in (1,2) then 1
    select_22_0$nsaids_core22[i] = 1
  } else if (select_22_0$FO133[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_22_0$nsaids_core22[i] = 0
  } else {
    #Else NA
    select_22_0$nsaids_core22[i] = NA
  }
}

#ex23_0 (GEN 1 Exam 23)
#FP078, FP079, FP086, FP087, FP088
exam_23_0 <- read_sas("e_exam_ex23_0_0071.sas7bdat")
select_23_0 <- exam_23_0[,c("ID", "FP078", "FP079", "FP086", "FP087", "FP088")]

#Replacing NA with 10 avoids R treating those values differently
select_23_0 <- select_23_0 %>% mutate_at(c("FP078", "FP079", "FP086", "FP087", "FP088"), ~replace_na(.,10))

#Placeholder values, used for testing
select_23_0$all_inflammation_core23 = 9999
select_23_0$steroids_core23 = 9999
select_23_0$nsaids_core23 = 9999

#all inflammation
for (i in 1:nrow(select_23_0)) {
  if (select_23_0$FP078[i] %in% c(1,2) | select_23_0$FP079[i] %in% c(1,2)
      | select_23_0$FP086[i] %in% c(1,2) | select_23_0$FP087[i] %in% c(1,2)
      | select_23_0$FP088[i] == 1) {
    #any in (1,2) then 1
    select_23_0$all_inflammation_core23[i] = 1
  } else if (select_23_0$FP078[i] %in% c(0,3) & select_23_0$FP079[i] %in% c(0,3)
             & select_23_0$FP086[i] %in% c(0,3) & select_23_0$FP087[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_23_0$all_inflammation_core23[i] = 0
  } else {
    #Else NA
    select_23_0$all_inflammation_core23[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_23_0)) {
  if (select_23_0$FP086[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_23_0$steroids_core23[i] = 1
  } else if (select_23_0$FP086[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_23_0$steroids_core23[i] = 0
  } else {
    #Else NA
    select_23_0$steroids_core23[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_23_0)) {
  if (select_23_0$FP087[i] %in% c(1,2) | select_23_0$FP088[i] == 1) {
    #any in (1,2) then 1
    select_23_0$nsaids_core23[i] = 1
  } else if (select_23_0$FP087[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_23_0$nsaids_core23[i] = 0
  } else {
    #Else NA
    select_23_0$nsaids_core23[i] = NA
  }
}


#ex24_0 (GEN 1 Exam 24)
#FQ163, FQ164, FQ171, FQ172
exam_24_0 <- read_sas("e_exam_ex24_0_0072.sas7bdat")
select_24_0 <- exam_24_0[,c("ID", "FQ163", "FQ164", "FQ171", "FQ172")]

#Replacing NA with 10 avoids R treating those values differently
select_24_0 <- select_24_0 %>% mutate_at(c("FQ163", "FQ164", "FQ171", "FQ172"), ~replace_na(.,10))

#Placeholder values, used for testing
select_24_0$all_inflammation_core24 = 9999
select_24_0$steroids_core24 = 9999
select_24_0$nsaids_core24 = 9999

#all inflammation
for (i in 1:nrow(select_24_0)) {
  if (select_24_0$FQ163[i] %in% c(1,2) | select_24_0$FQ164[i] %in% c(1,2)
      | select_24_0$FQ171[i] %in% c(1,2) | select_24_0$FQ172[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_24_0$all_inflammation_core24[i] = 1
  } else if (select_24_0$FQ163[i] %in% c(0,3) & select_24_0$FQ164[i] %in% c(0,3)
             & select_24_0$FQ171[i] %in% c(0,3) & select_24_0$FQ172[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_24_0$all_inflammation_core24[i] = 0
  } else {
    #Else NA
    select_24_0$all_inflammation_core24[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_24_0)) {
  if (select_24_0$FQ171[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_24_0$steroids_core24[i] = 1
  } else if (select_24_0$FQ171[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_24_0$steroids_core24[i] = 0
  } else {
    #Else NA
    select_24_0$steroids_core24[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_24_0)) {
  if (select_24_0$FQ172[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_24_0$nsaids_core24[i] = 1
  } else if (select_24_0$FQ172[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_24_0$nsaids_core24[i] = 0
  } else {
    #Else NA
    select_24_0$nsaids_core24[i] = NA
  }
}

#ex25_0 (GEN 1 Exam 25)
#FR215, FR216, FR223, FR224
exam_25_0 <- read_sas("e_exam_ex25_0_0073.sas7bdat")
select_25_0 <- exam_25_0[,c("ID", "FR215", "FR216", "FR223", "FR224")]

#Replacing NA with 10 avoids R treating those values differently
select_25_0 <- select_25_0 %>% mutate_at(c("FR215", "FR216", "FR223", "FR224"), ~replace_na(.,10))

#Placeholder values, used for testing
select_25_0$all_inflammation_core25 = 9999
select_25_0$steroids_core25 = 9999
select_25_0$nsaids_core25 = 9999

#all inflammation
for (i in 1:nrow(select_25_0)) {
  if (select_25_0$FR215[i] %in% c(1,2) | select_25_0$FR216[i] %in% c(1,2)
      | select_25_0$FR223[i] %in% c(1,2) | select_25_0$FR224[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_25_0$all_inflammation_core25[i] = 1
  } else if (select_25_0$FR215[i] %in% c(0,3) & select_25_0$FR216[i] %in% c(0,3)
             & select_25_0$FR223[i] %in% c(0,3) & select_25_0$FR224[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_25_0$all_inflammation_core25[i] = 0
  } else {
    #Else NA
    select_25_0$all_inflammation_core25[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_25_0)) {
  if (select_25_0$FR223[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_25_0$steroids_core25[i] = 1
  } else if (select_25_0$FR223[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_25_0$steroids_core25[i] = 0
  } else {
    #Else NA
    select_25_0$steroids_core25[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_25_0)) {
  if (select_25_0$FR224[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_25_0$nsaids_core25[i] = 1
  } else if (select_25_0$FR224[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_25_0$nsaids_core25[i] = 0
  } else {
    #Else NA
    select_25_0$nsaids_core25[i] = NA
  }
}


#ex26_0 (GEN 1 Exam 26)
#FS274, FS275, FS291, FS292
exam_26_0 <- read_sas("e_exam_ex26_0_0074.sas7bdat")
select_26_0 <- exam_26_0[,c("ID", "FS274", "FS275", "FS291", "FS292")]

#Replacing NA with 10 avoids R treating those values differently
select_26_0 <- select_26_0 %>% mutate_at(c("FS274", "FS275", "FS291", "FS292"), ~replace_na(.,10))

#Placeholder values, used for testing
select_26_0$all_inflammation_core26 = 9999
select_26_0$steroids_core26 = 9999
select_26_0$nsaids_core26 = 9999

#all inflammation
for (i in 1:nrow(select_26_0)) {
  if (select_26_0$FS274[i] %in% c(1,2) | select_26_0$FS275[i] %in% c(1,2)
      | select_26_0$FS291[i] %in% c(1,2) | select_26_0$FS292[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_26_0$all_inflammation_core26[i] = 1
  } else if (select_26_0$FS274[i] %in% c(0,3) & select_26_0$FS275[i] %in% c(0,3)
             & select_26_0$FS291[i] %in% c(0,3) & select_26_0$FS292[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_26_0$all_inflammation_core26[i] = 0
  } else {
    #Else NA
    select_26_0$all_inflammation_core26[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_26_0)) {
  if (select_26_0$FS291[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_26_0$steroids_core26[i] = 1
  } else if (select_26_0$FS291[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_26_0$steroids_core26[i] = 0
  } else {
    #Else NA
    select_26_0$steroids_core26[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_26_0)) {
  if (select_26_0$FS292[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_26_0$nsaids_core26[i] = 1
  } else if (select_26_0$FS292[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_26_0$nsaids_core26[i] = 0
  } else {
    #Else NA
    select_26_0$nsaids_core26[i] = NA
  }
}


#ex27_0 (GEN 1 Exam 27)
#FT281, FT282, FT298, FT299
exam_27_0 <- read_sas("e_exam_ex27_0_0075.sas7bdat")
select_27_0 <- exam_27_0[,c("ID", "FT281", "FT282", "FT298", "FT299")]

#Replacing NA with 10 avoids R treating those values differently
select_27_0 <- select_27_0 %>% mutate_at(c("FT281", "FT282", "FT298", "FT299"), ~replace_na(.,10))

#Placeholder values, used for testing
select_27_0$all_inflammation_core27 = 9999
select_27_0$steroids_core27 = 9999
select_27_0$nsaids_core27 = 9999

#all inflammation
for (i in 1:nrow(select_27_0)) {
  if (select_27_0$FT281[i] %in% c(1,2) | select_27_0$FT282[i] %in% c(1,2)
      | select_27_0$FT298[i] %in% c(1,2) | select_27_0$FT299[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_27_0$all_inflammation_core27[i] = 1
  } else if (select_27_0$FT281[i] %in% c(0,3) & select_27_0$FT282[i] %in% c(0,3)
             & select_27_0$FT298[i] %in% c(0,3) & select_27_0$FT299[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_27_0$all_inflammation_core27[i] = 0
  } else {
    #Else NA
    select_27_0$all_inflammation_core27[i] = NA
  }
}

#steroids
for (i in 1:nrow(select_27_0)) {
  if (select_27_0$FT298[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_27_0$steroids_core27[i] = 1
  } else if (select_27_0$FT298[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_27_0$steroids_core27[i] = 0
  } else {
    #Else NA
    select_27_0$steroids_core27[i] = NA
  }
}

#nsaids
for (i in 1:nrow(select_27_0)) {
  if (select_27_0$FT299[i] %in% c(1,2)) {
    #any in (1,2) then 1
    select_27_0$nsaids_core27[i] = 1
  } else if (select_27_0$FT299[i] %in% c(0,3)) {
    #all in (0,3) then 0
    select_27_0$nsaids_core27[i] = 0
  } else {
    #Else NA
    select_27_0$nsaids_core27[i] = NA
  }
}


#Exam 28
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered28 <- cod_28 %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered28 <- cod_28 %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered28 <- cod_28 %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
ID28 <- unique(cod_28$ID)
med_found28 <- data.frame(ID28)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found28['anti_column'] <- as.integer(med_found28$ID28 %in% anti_filtered28$ID)
med_found28['nsaid_column'] <- as.integer(med_found28$ID28 %in% nsaid_filtered28$ID)
med_found28['steroid_column'] <- as.integer(med_found28$ID28 %in% steroid_filtered28$ID)

#standardizing column names
colnames(med_found28) <- c("ID", "all_inflammation_core28", "nsaids_core28", "steroids_core28")
#ordering ID column
select_28_0 <- med_found28[order(med_found28$ID),] 


#Exam 29
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered29 <- cod_29 %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered29 <- cod_29 %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered29 <- cod_29 %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
ID29 <- unique(cod_29$id)
med_found29 <- data.frame(ID29)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found29['anti_column'] <- as.integer(med_found29$ID29 %in% anti_filtered29$id)
med_found29['nsaid_column'] <- as.integer(med_found29$ID29 %in% nsaid_filtered29$id)
med_found29['steroid_column'] <- as.integer(med_found29$ID29 %in% steroid_filtered29$id)

#standardizing column names
colnames(med_found29) <- c("ID", "all_inflammation_core29", "nsaids_core29", "steroids_core29")
#ordering ID column
select_29_0 <- med_found29[order(med_found29$ID),] 


#Exam 30
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered30 <- cod_30 %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered30 <- cod_30 %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered30 <- cod_30 %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
ID30 <- unique(cod_30$id)
med_found30 <- data.frame(ID30)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found30['anti_column'] <- as.integer(med_found30$ID30 %in% anti_filtered30$id)
med_found30['nsaid_column'] <- as.integer(med_found30$ID30 %in% nsaid_filtered30$id)
med_found30['steroid_column'] <- as.integer(med_found30$ID30 %in% steroid_filtered30$id)

#standardizing column names
colnames(med_found30) <- c("ID", "all_inflammation_core30", "nsaids_core30", "steroids_core30")
#ordering ID column
select_30_0 <- med_found30[order(med_found30$ID),] 


#Exam 31
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered31 <- cod_31 %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered31 <- cod_31 %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered31 <- cod_31 %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
ID31 <- unique(cod_31$id)
med_found31 <- data.frame(ID31)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found31['anti_column'] <- as.integer(med_found31$ID31 %in% anti_filtered31$id)
med_found31['nsaid_column'] <- as.integer(med_found31$ID31 %in% nsaid_filtered31$id)
med_found31['steroid_column'] <- as.integer(med_found31$ID31 %in% steroid_filtered31$id)

#standardizing column names
colnames(med_found31) <- c("ID", "all_inflammation_core31", "nsaids_core31", "steroids_core31")
#ordering ID column
select_31_0 <- med_found31[order(med_found31$ID),] 


#Exam 32
#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
anti_filtered32 <- cod_32 %>% filter_all(any_vars(. %in% anti_atc_list))
nsaid_filtered32 <- cod_32 %>% filter_all(any_vars(. %in% nsaid_atc_list))
steroid_filtered32 <- cod_32 %>% filter_all(any_vars(. %in% steroid_atc_list))

#Constructing the output data frame
ID32 <- unique(cod_32$id)
med_found32 <- data.frame(ID32)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found32['anti_column'] <- as.integer(med_found32$ID32 %in% anti_filtered32$id)
med_found32['nsaid_column'] <- as.integer(med_found32$ID32 %in% nsaid_filtered32$id)
med_found32['steroid_column'] <- as.integer(med_found32$ID32 %in% steroid_filtered32$id)

#standardizing column names
colnames(med_found32) <- c("ID", "all_inflammation_core32", "nsaids_core32", "steroids_core32")
#ordering ID column
select_32_0 <- med_found32[order(med_found32$ID),] 


#### Final Merge ####
#Joining columns together

df_list <- list(select_17_0, select_18_0, select_19_0, select_20_0, select_21_0, select_22_0, select_23_0, select_24_0,
                select_25_0, select_26_0, select_27_0, select_28_0, select_29_0, select_30_0, select_31_0, select_32_0)

df_joined <- df_list %>% reduce(full_join, by='ID')
#Ordering the columns, they can be out of order after the join
df_ordered <- df_joined %>% arrange(as.numeric(ID))

#add further id columns
df_ordered$IDTYPE <- 0
df_ordered$FRAMID <- df_ordered$ID


df_final <- df_ordered[,c("IDTYPE", "ID", "FRAMID", "all_inflammation_core17", "steroids_core17",
                          "all_inflammation_core18", "steroids_core18",
                          "all_inflammation_core19", "nsaids_core19", "steroids_core19",
                          "all_inflammation_core20", "nsaids_core20", "steroids_core20",
                          "all_inflammation_core21", "nsaids_core21", "steroids_core21",
                          "all_inflammation_core22", "nsaids_core22", "steroids_core22",
                          "all_inflammation_core23", "nsaids_core23", "steroids_core23",
                          "all_inflammation_core24", "nsaids_core24", "steroids_core24",
                          "all_inflammation_core25", "nsaids_core25", "steroids_core25",
                          "all_inflammation_core26", "nsaids_core26", "steroids_core26",
                          "all_inflammation_core27", "nsaids_core27", "steroids_core27",
                          "all_inflammation_core28", "nsaids_core28", "steroids_core28",
                          "all_inflammation_core29", "nsaids_core29", "steroids_core29",
                          "all_inflammation_core30", "nsaids_core30", "steroids_core30",
                          "all_inflammation_core31", "nsaids_core31", "steroids_core31",
                          "all_inflammation_core32", "nsaids_core32", "steroids_core32")]

#write final CSV containing abstracted data
write.csv(df_final, file = "Gen1_all_inflammation_core.csv", row.names = FALSE)


