# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS aspirin derived variable creation source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Reviewed by Alvin Ang
# Last updated: August 2023
# 
# 
# The purpose of this R code is to allow users to create the derived variables for Gen 3/Omni 2/NOS aspirin usage.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Individual FHS exam questionnaires:
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


#### Exam 1 ####
#ex01_3 (GEN 3 Exam 1)
#G3A008-G3A011
exam_01_3 <- read_sas("e_exam_ex01_3_0086_v2.sas7bdat")
select_01_3 <- exam_01_3[,c("ID", "IDTYPE", "G3A008", "G3A009", "G3A010", "G3A011")]

#ex01_2 (NOS Exam 1)
#G3A008-G3A011
exam_01_2 <- read_sas("e_exam_ex01_2_0813.sas7bdat")
select_01_2 <- exam_01_2[,c("id", "idtype", "g3a008", "g3a009", "g3a010", "g3a011")]

#ex01_72 (Omni 2 Exam 1)
#G3A008-G3A011
exam_01_72 <- read_sas("e_exam_ex01_72_0652.sas7bdat")
select_01_72 <- exam_01_72[,c("id", "idtype", "g3a008", "g3a009", "g3a010", "g3a011")]


#Combine Exam 1 sections into one data frame
colnames(select_01_3) <- colnames(select_01_2)
select_01_3b <- do.call("rbind", list(select_01_3, select_01_2, select_01_72))

#Placeholder values, used for testing
select_01_3b$aspirin_usage_core1 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_01_3b)) {
  if (is.na(select_01_3b$g3a009[i]) | is.na(select_01_3b$g3a010[i]) | is.na(select_01_3b$g3a011[i])) {
    #If NA in any column then aspirin_usage_core1 = NA (This has to go first for logic)
    select_01_3b$aspirin_usage_core1[i] = NA
  } else if (select_01_3b$g3a009[i] == 1 & select_01_3b$g3a010[i] == 1 
             & select_01_3b$g3a011[i] >= 81 & select_01_3b$g3a011[i] <= 325) {
    #If G3A009 = 1 and G3A010 = 1 and G3A011 = (081, 160, 325) then aspirin_usage_core1 = 1
    select_01_3b$aspirin_usage_core1[i] = 1
  } else if (select_01_3b$g3a009[i] == 2 & select_01_3b$g3a010[i] == 1 
             & select_01_3b$g3a011[i] >= 81 & select_01_3b$g3a011[i] <= 160) {
    #If G3A009 = 2 and G3A010 = 1 and G3A011 = (081, 160) then aspirin_usage_core1 = 1
    select_01_3b$aspirin_usage_core1[i] = 1
  } else if (select_01_3b$g3a009[i] >= 3 & select_01_3b$g3a009[i] <= 4 
             & select_01_3b$g3a010[i] == 1 & select_01_3b$g3a011[i] == 81) {
    #If G3A009 = (3, 4) and G3A010 = 1 and G3A011 = 081 then aspirin_usage_core1 = 1
    select_01_3b$aspirin_usage_core1[i] = 1
  } else if (select_01_3b$g3a009[i] >= 7 & select_01_3b$g3a009[i] <= 10 
             & select_01_3b$g3a010[i] == 2 & select_01_3b$g3a011[i] >= 81 
             & select_01_3b$g3a011[i] <= 325) {
    #If G3A009 = (7, 8, 9, 10)  and G3A010 = 2 and G3A011 = (081, 160, 325) then aspirin_usage_core1 = 1
    select_01_3b$aspirin_usage_core1[i] = 1
  } else if (select_01_3b$g3a009[i] == 10 & select_01_3b$g3a010[i] == 1 
             & select_01_3b$g3a011[i] == 325) {
    #If G3A009 = 10 and G3A010 = 1 and G3A011 = 325 then aspirin_usage_core1 = 3
    select_01_3b$aspirin_usage_core1[i] = 3
  } else if (select_01_3b$g3a009[i] >= 6 & select_01_3b$g3a009[i] <= 10 
             & select_01_3b$g3a010[i] == 1 & select_01_3b$g3a011[i] == 500) {
    #If G3A009 = (6, 7, 8, 9, 10) and G3A010 = 1 and G3A011 = 500 then aspirin_usage_core1 = 3
    select_01_3b$aspirin_usage_core1[i] = 3
  } else if (select_01_3b$g3a009[i] == 0 | select_01_3b$g3a010[i] == 0
             | select_01_3b$g3a010[i] == 3 | select_01_3b$g3a010[i] == 4
             | select_01_3b$g3a011[i] == 0) {
    #If G3A009 = (0, NA) OR G3A010 = (0, 3, 4, NA) OR G3A011 = (0, NA) then aspirin_usage_core1 = NA
    select_01_3b$aspirin_usage_core1[i] = NA
  } else if (select_01_3b$g3a009[i] >= 3 & select_01_3b$g3a009[i] <= 6 
             & select_01_3b$g3a010[i] == 2 & select_01_3b$g3a011[i] >= 81 
             & select_01_3b$g3a011[i] <= 325) {
    #If G3A009 = 3 to 6 and G3A010 = 2 and G3A011 = 81 to 325, then aspirin_usage_core1 = 1
    select_01_3b$aspirin_usage_core1[i] = 1
  } else {
    #Else, aspirin_usage_core1 = 2
    select_01_3b$aspirin_usage_core1[i] = 2
  }
}

#assign framid
select_01_3b$framid <- with(select_01_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))
#rename columns
colnames(select_01_3b)[3:7] <- c("aspirin_use_core1", "aspirin_quantity_core1", "aspirin_freq_core1",
                                 "aspirin_dose_core1", "aspirin_purpose_core1")
#replace NA
select_01_3b[3:7] <- select_01_3b[3:7] %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))

#### Exam 2 ####
#G3B0009-G3B0012
exam_02_3b <- read_sas("e_exam_2011_m_0017_v1.sas7bdat")
select_02_3b <- exam_02_3b[,c("id", "idtype", "g3b0009", "g3b0010", "g3b0011", "g3b0012")]

#Placeholder values, used for testing
select_02_3b$aspirin_usage_core2 = 9999

#for loop to determine derived variable
for (i in 1:nrow(select_02_3b)) {
  if (is.na(select_02_3b$g3b0010[i]) | is.na(select_02_3b$g3b0011[i]) | is.na(select_02_3b$g3b0012[i])) {
    #If NA in any column then aspirin_usage_core2 = NA (This has to go first for logic)
    select_02_3b$aspirin_usage_core2[i] = NA
  } else if (select_02_3b$g3b0010[i] == 1 & select_02_3b$g3b0011[i] == 1 
             & select_02_3b$g3b0012[i] >= 80 & select_02_3b$g3b0012[i] <= 325) {
    #If G3B0010 = 1 and G3B0011 = 1 and G3B0012 = (80 – 325) then aspirin_usage_core2 = 1
    select_02_3b$aspirin_usage_core2[i] = 1
  } else if (select_02_3b$g3b0010[i] == 2 & select_02_3b$g3b0011[i] == 1 
             & select_02_3b$g3b0012[i] >= 80 & select_02_3b$g3b0012[i]<= 163) {
    #If G3B0010 = 2 and G3B0011 = 1 and G3B0012 = (80 – 163) then aspirin_usage_core2 = 1
    select_02_3b$aspirin_usage_core2[i] = 1
  } else if (select_02_3b$g3b0010[i] >= 3 & select_02_3b$g3b0010[i] <= 4 
             & select_02_3b$g3b0011[i] == 1 & select_02_3b$g3b0012[i] >= 80 
             & select_02_3b$g3b0012[i] <= 163) {
    #If G3B0010 = (3, 4) and G3B0011 = 1 and G3B0012 = (80 – 85) then aspirin_usage_core2 = 1
    select_02_3b$aspirin_usage_core2[i] = 1
  } else if (select_02_3b$g3b0010[i] >= 7 & select_02_3b$g3b0010[i] <= 10 
             & select_02_3b$g3b0011[i] == 2 & select_02_3b$g3b0012[i] >= 80 
             & select_02_3b$g3b0012[i] <= 325) {
    #If G3B0010 = (7, 8, 9, 10) and G3B0011 = 2 and G3B0012 = (80 – 325) then aspirin_usage_core2 = 1
    select_02_3b$aspirin_usage_core2[i] = 1
  } else if (select_02_3b$g3b0010[i] == 10 & select_02_3b$g3b0011[i] == 1 
             & select_02_3b$g3b0012[i] >= 320 & select_02_3b$g3b0012[i] <= 330) {
    #If G3B0010 = 10 and G3B0011 = 1 and G3B0012 = (320 – 330) then aspirin_usage_core2 = 3
    select_02_3b$aspirin_usage_core2[i] = 3
  } else if (select_02_3b$g3b0010[i] >= 6 & select_02_3b$g3b0010[i] <= 10 
             & select_02_3b$g3b0011[i] == 1 & select_02_3b$g3b0012[i] >= 500) {
    #If G3B0010 = (6 – 10) and G3B0011 = 1 and G3B0012 = (500+) then aspirin_usage_core2 = 3
    select_02_3b$aspirin_usage_core2[i] = 3
  } else if (select_02_3b$g3b0010[i] >= 5 & select_02_3b$g3b0010[i] <= 10 
             & select_02_3b$g3b0011[i] == 1 & select_02_3b$g3b0012[i] == 600) {
    #If G3B0010 = (5 – 10) and G3B0011 = 1 and G3B0012 = 600 then aspirin_usage_core2 = 3
    select_02_3b$aspirin_usage_core2[i] = 3
  } else if (select_02_3b$g3b0010[i] == 0 | select_02_3b$g3b0011[i] == 0
             | select_02_3b$g3b0011[i] == 3 | select_02_3b$g3b0011[i] == 4
             | select_02_3b$g3b0012[i] <= 79) {
    #If G3B0010 = (0, NA) or G3B0011 = (0, 3, 4, NA) or G3B0012 = (79 and below, NA) then aspirin_usage_core2 = NA
    select_02_3b$aspirin_usage_core2[i] = NA
  } else if (select_02_3b$g3b0010[i] >= 3 & select_02_3b$g3b0010[i] <= 6 
             & select_02_3b$g3b0011[i] == 2 & select_02_3b$g3b0012[i] >= 81 
             & select_02_3b$g3b0012[i] <= 325) {
    #If G3B0010 = 3 to 6 and G3B0011 = 2 and G3B0012 = 81 to 325, then aspirin_usage_core2 = 1
    select_02_3b$aspirin_usage_core2[i] = 1
  } else {
    #Else, aspirin_usage_core2 = 2
    select_02_3b$aspirin_usage_core2[i] = 2
  }
}

#assign framid
select_02_3b$framid <- with(select_02_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))
#rename columns
colnames(select_02_3b)[3:7] <- c("aspirin_use_core2", "aspirin_quantity_core2", "aspirin_freq_core2",
                                 "aspirin_dose_core2", "aspirin_purpose_core2")
#replace NA
select_02_3b[3:7] <- select_02_3b[3:7] %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Exam 3 ####
#G3C0133-G3C0137 
exam_03_3b <- read_sas("e_exam_ex03_3b_1069.sas7bdat")
select_03_3b <- exam_03_3b[,c("id", "idtype", "G3C0133", "G3C0134", "G3C0135", "G3C0136", "G3C0137")]

#Placeholder values, used for testing
select_03_3b$aspirin_usage_core3 = 9999

for (i in 1:nrow(select_03_3b)) {
  if (select_03_3b$G3C0136[i] == 888 & (!is.na(select_03_3b$G3C0136[i]))) {
    select_03_3b$G3C0136[i] = select_03_3b$G3C0137[i]
  }
}

#for loop to determine derived variable
for (i in 1:nrow(select_03_3b)) {
  if (is.na(select_03_3b$G3C0134[i]) | is.na(select_03_3b$G3C0135[i]) | is.na(select_03_3b$G3C0136[i])) {
    #If NA in any column then aspirin_usage_core3 = NA (This has to go first for logic)
    select_03_3b$aspirin_usage_core3[i] = NA
  } else if (select_03_3b$G3C0134[i] >= 0.5 & select_03_3b$G3C0134[i] <= 1 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] >= 80 
             & select_03_3b$G3C0136[i] <= 325) {
    #If G3C0134 = (0.5, 1) and G3C0135 = 1 and G3C0136 = (80 – 325) then aspirin_usage_core3 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else if (select_03_3b$G3C0134[i] >= 1.5 & select_03_3b$G3C0134[i] <= 2 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] >= 80 
             & select_03_3b$G3C0136[i] <= 163) {
    #If G3C0134 = (1.5, 2) and G3C0135 = 1 and G3C0136 = (80 – 163) then aspirin_usage_core3 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else if (select_03_3b$G3C0134[i] >= 3 & select_03_3b$G3C0134[i] <= 4 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] >= 80 
             & select_03_3b$G3C0136[i] <= 85) {
    #If G3C0134 = (3, 3.5, 4) and G3C0135 = 1 and G3C0136 = (80 – 85) then aspirin_usage_core3 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else if (select_03_3b$G3C0134[i] >= 7 & select_03_3b$G3C0134[i] <= 14 
             & select_03_3b$G3C0135[i] == 2 & select_03_3b$G3C0136[i] >= 80 
             & select_03_3b$G3C0136[i] <= 163) {
    #If G3C0134 = (7 - 14) and G3C0135 = 2 and G3C0136 = (80 – 163) then aspirin_usage_core3 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else if (select_03_3b$G3C0134[i] >= 7 & select_03_3b$G3C0134[i] <= 10 
             & select_03_3b$G3C0135[i] == 2 & select_03_3b$G3C0136[i] >= 320 
             & select_03_3b$G3C0136[i] <= 330) {
    #If G3C0134 = (7 - 10) and G3C0135 = 2 and G3C0136 = (320 – 330) then aspirin_usage_core3 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else if (select_03_3b$G3C0134[i] >= 7 & select_03_3b$G3C0134[i] <= 12 
             & select_03_3b$G3C0135[i] == 2 & select_03_3b$G3C0136[i] == 250) {
    #If G3C0134 = (7 - 12) and G3C0135 = 2 and G3C0136 = 250 then aspirin_usage_core3 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else if (select_03_3b$G3C0134[i] >= 10 & select_03_3b$G3C0134[i] <= 14 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] >= 320 
             & select_03_3b$G3C0136[i] <= 330) {
    ##If G3C0134 = (10 - 14) and G3C0135 = 1 and G3C0136 = (320 - 330) then aspirin_usage_core3 = 3
    select_03_3b$aspirin_usage_core3[i] = 3
  } else if (select_03_3b$G3C0134[i] >= 12 & select_03_3b$G3C0134[i] <= 14 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] == 250) {
    #If G3C0134 = (12 - 14) and G3C0135 = 1 and G3C0136 = 250 then aspirin_usage_core3 = 3
    select_03_3b$aspirin_usage_core3[i] = 3
  } else if (select_03_3b$G3C0134[i] >= 6 & select_03_3b$G3C0134[i] <= 14 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] == 500) {
    #If G3C0134 = (6 - 14) and G3C0135 = 1 and G3C0136 = 500 then aspirin_usage_core3 = 3
    select_03_3b$aspirin_usage_core3[i] = 3
  } else if (select_03_3b$G3C0134[i] >= 5 & select_03_3b$G3C0134[i] <= 14 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] == 600) {
    #If G3C0134 = (5 - 14) and G3C0135 = 1 and G3C0136 = 600 then aspirin_usage_core3 = 3
    select_03_3b$aspirin_usage_core3[i] = 3
  } else if (select_03_3b$G3C0134[i] >= 4 & select_03_3b$G3C0134[i] <= 14 
             & select_03_3b$G3C0135[i] == 1 & select_03_3b$G3C0136[i] == 800) {
    #If G3C0134 = (4 - 14) and G3C0135 = 1 and G3C0136 = 800 then aspirin_usage_core3 = 3
    select_03_3b$aspirin_usage_core3[i] = 3
  } else if (select_03_3b$G3C0134[i] == 0 | select_03_3b$G3C0135[i] == 0
             | select_03_3b$G3C0135[i] == 3 | select_03_3b$G3C0135[i] == 4
             | select_03_3b$G3C0136[i] == 0) {
    #If G3C0134 = (0, NA) or G3C0135 = (0, 3, 4, NA) or G3C0136 = (0, NA) then aspirin_usage_core3 = NA
    select_03_3b$aspirin_usage_core3[i] = NA
  } else if (select_03_3b$G3C0134[i] >= 3 & select_03_3b$G3C0134[i] <= 6 
             & select_03_3b$G3C0135[i] == 2 & select_03_3b$G3C0136[i] >= 81 
             & select_03_3b$G3C0136[i] <= 325) {
    #If G3C0134 = 3 to 6 and G3C0135 = 2 and G3C0136 = 81 to 325, then aspirin_usage_core2 = 1
    select_03_3b$aspirin_usage_core3[i] = 1
  } else  {
    #Else aspirin_usage_core3 = 2
    select_03_3b$aspirin_usage_core3[i] = 2
  }
}

select_03_3b <- select_03_3b %>% select(-G3C0137) #unnecessary column
#assign framid
select_03_3b$framid <- with(select_03_3b, ifelse(idtype == 3, 30000 + id, 
                                                 ifelse(idtype == 2, 20000 + id, 
                                                        ifelse(idtype == 72, 720000 + id, id))))
#rename columns
colnames(select_03_3b)[3:7] <- c("aspirin_use_core3", "aspirin_quantity_core3", "aspirin_freq_core3",
                                 "aspirin_dose_core3", "aspirin_purpose_core3")
#replace NA
select_03_3b[3:7] <- select_03_3b[3:7] %>% mutate(across(where(is.numeric), ~ replace_na(as.character(.x), ".")))


#### Final Merge ####
#merge datasets
merge_1_2 <- merge(select_01_3b, select_02_3b, by = c("idtype", "id", "framid"),  all.x = TRUE)
merge_final <- merge(merge_1_2, select_03_3b, by = c("idtype", "id", "framid"),  all.x = TRUE)
#write final csv
write.csv(merge_final, file = "aspirin_gen3_full_spreadsheet.csv", row.names = FALSE)

