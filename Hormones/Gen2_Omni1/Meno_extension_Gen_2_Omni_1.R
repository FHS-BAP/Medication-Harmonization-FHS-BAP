# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 Menopause, Hormone Replacement and Ovary Removal Exam 9/10 dataset extension/unification source code
# ******************************************************************************************************************************************
# 
# Created by Michael Cummings
# Last updated: February 2024
# 
# 
# The purpose of this R code is to extend the Gen 2/Omni 1 menopause, hormone replacement and ovary removal dataset with Exam 3 data,
# as well as unify the datasets.
# The new data is:
# 1) Exam 9 menopause information. (Not necessary afterward)
# 2) Exam 9 and 10 hormone replacement drug information.
# 3) Exam 9 and 10 ovary removal information.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Menopause, Hormone Replacement and Ovary Removal through Exam 2:
# vr_meno_ex08_1_0450.sas7bdat (Gen 2 through Exam 8)
# vr_meno_ex03_7_0916.sas7bdat (Omni 1 through Exam 3)
#
# 2)  Individual FHS exam questionnaire:
# e_exam_ex09_1b_0844.sas7bdat (Gen 2 Exam 9 and Omni 1 Exam 9)
# e_exam_ex10_1b_1409.sas7bdat (Gen 2 Exam 10 and Omni 1 Exam 5)
#
# 3)  Curated Gen 2/Omni 1 dataset - curated_bap_17_0712.sas7bdat
#
# 4)  Hormone ATC information - ATC2_HormonesCodes_ALL_20231012.xlsx
# 
# 5)  Full ATC medication info for patients:
# vr_meds_ex03_7_0535.sas7bdat (Omni 1 Exam 3)
# vr_meds_ex08_1_0280_v1.sas7bdat (Gen 2 Exam 8)
# vr_meds_ex09_1b_0879.sas7bdat (Gen 2 Exam 9 and Omni 1 Exam 9)
# vr_meds_ex10_1b_1198.sas7bdat (Gen 2 Exam 10 and Omni 1 Exam 5)
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
setwd("/Users/michaeltcummings/Desktop/BU-stuff/Drugs_and_related/vr_meno_data_extension_Gen2_Omni1")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files


#### Reading ####

#j052 is age when periods stopped, 
#j053 is cause of stop
#j060 is ovary removal info
#EST9 and SERM9 based on medication data

#read important sections
exam_09_1b <- read_sas("e_exam_ex09_1b_0844.sas7bdat")
select_09_1b <- exam_09_1b[,c("idtype", "id", "j052", "j053", "j060")]

curated <- read_sas("curated_bap_17_0712.sas7bdat")
curated_09_1 <- subset(curated, idtype == 1)
age_core_list_09_1 <- curated_09_1[,c("id", "age_core1", "age_core2", "age_core3",
                                      "age_core4", "age_core5", "age_core6",
                                      "age_core7", "age_core8", "age_core9")]

#reading two drug lists from excel spreadsheet
est_comb_sheet <- read_excel("ATC2_HormonesCodes_ALL_20231012.xlsx", sheet = "Est_Comb")
est_atc_list <- est_comb_sheet$`ATC Code for medication`
serm_sheet <- read_excel("ATC2_HormonesCodes_ALL_20231012.xlsx", sheet = "SERM")
serm_atc_list <- serm_sheet$`ATC Code for medication`

#### Gen 2 ####

vr_meno_ex08_1_orig <- read_sas("vr_meno_ex08_1_0450.sas7bdat")

pre_agelist_08_1 <- vr_meno_ex08_1_orig[,c("ID", "STOP_AGE")]
age_core_list_09_1 <- rename(age_core_list_09_1, ID = id)
joined_age_list_09_1 <- inner_join(pre_agelist_08_1, age_core_list_09_1, by = "ID")

#Placeholder values, used for testing
joined_age_list_09_1$EXAMSTOP = 9999

#for loop to determine EXAMSTOP
for (i in 1:nrow(joined_age_list_09_1)) {
  if (is.na(joined_age_list_09_1$STOP_AGE[i])) {
    #NA means NA
    joined_age_list_09_1$EXAMSTOP[i] = NA
  } else if (joined_age_list_09_1$STOP_AGE[i] == 0) {
    #0 means 0
    joined_age_list_09_1$EXAMSTOP[i] = 0
  } else {
    #match the age
    joined_age_list_09_1$EXAMSTOP[i] <- 
      min(which(joined_age_list_09_1[i, 3:11] >= joined_age_list_09_1$STOP_AGE[i]))
  }
}

for_merge_1 <- joined_age_list_09_1[,c("ID", "EXAMSTOP")]
vr_meno_ex08_1 <- inner_join(vr_meno_ex08_1_orig, for_merge_1, by = "ID")


#### Main section ####
vr_meno_ex08_1_section1 <- vr_meno_ex08_1[c(1:2, 20:21, 23)] #IDTYPE, ID, STOPAGE, CAUSE, EXAMSTOP
vr_meno_ex08_1_section2 <- vr_meno_ex08_1[c(1:19, 22)] #IDTYPE, ID, columns not above

#subset idtype == 1 for just Gen 2
select_09_1 <- subset(select_09_1b, idtype == 1)

stop_09_1 <- subset(select_09_1, select = -c(j060))
#removes 0 and 88 values in both columns (numbers are the same) while keeping NA values for now
menopause_09_1_na_present <- subset(stop_09_1, j052 != 0 & j052 != 88 | is.na(j052))
#remove rows where both j052 columns j053 are NA (while keeping rows where only one of them is)
menopause_09_1 <- menopause_09_1_na_present %>% filter(!if_all(c(j052, j053), is.na))

#only get IDS not already in list
already_stopped_09_1 <- subset(vr_meno_ex08_1, EXAMSTOP > 0) #excludes 0 values

#individuals that newly stopped
newly_stopped_09_1 <- menopause_09_1[!menopause_09_1$id %in% already_stopped_09_1$ID,]
colnames(newly_stopped_09_1)[3:4] <- c("STOPAGE", "CAUSE")
#set exam stopped as 9
newly_stopped_09_1$EXAMSTOP <- 9

#new rows not previously in derived data (not including newly stopped individuals already in dataset)
vr_meno_ex08_1_section1_new <- vr_meno_ex08_1_section1[!vr_meno_ex08_1_section1$ID %in% newly_stopped_09_1$id,]

#The only old columns with updated values are new STOPAGE and CAUSE
newly_stopped_09_1 <- newly_stopped_09_1 %>% relocate(idtype, .after = id)
colnames(newly_stopped_09_1) <- colnames(vr_meno_ex08_1_section1_new)
vr_meno_ex08_1_section1_full <- rbind(vr_meno_ex08_1_section1_new, newly_stopped_09_1)

vr_meno_ex08_1_complete <- left_join(vr_meno_ex08_1_section1_full, vr_meno_ex08_1_section2, by = c("IDTYPE", "ID"))


#### OVREM9 ####

ovrem_09_1 <- subset(select_09_1, select = -c(j052, j053))
colnames(ovrem_09_1) <- c("IDTYPE", "ID", "OVREM9")

#fully new rows
ovrem_09_1_subset <- subset(ovrem_09_1, OVREM9 == 1 | OVREM9 == 2)
ovrem_09_1_subset_new <- ovrem_09_1_subset[!ovrem_09_1_subset$ID %in% vr_meno_ex08_1_complete$ID,]

#this will leave out the new rows but leave in all the others
ovrem_09_1_merge <- left_join(vr_meno_ex08_1_complete, ovrem_09_1, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
ovrem_09_1_bind <- bind_rows(ovrem_09_1_merge, ovrem_09_1_subset_new)

#### SERM 8 ####
#medications from Gen 2 Exam 8
meds_08_1 <- read_sas("vr_meds_ex08_1_0280_v1.sas7bdat")
cod_08_1 <- meds_08_1[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
serm_filtered_08_1 <- cod_08_1 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_08_1 <- unique(cod_08_1$id)
med_found_08_1 <- data.frame(atc_id_08_1)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_08_1['SERM8'] <- as.integer(med_found_08_1$atc_id_08_1 %in% serm_filtered_08_1$id)

#return id and idtype
colnames(med_found_08_1)[1] <- "ID"
med_found_08_1$IDTYPE <- 1

#fully new rows
med_found_08_1_subset <- subset(med_found_08_1, SERM8 == 1)
med_found_08_1_subset_new <- med_found_08_1_subset[!med_found_08_1_subset$ID %in% ovrem_09_1_bind$ID,]

#this will leave out the new rows but leave in all the others
med_found_08_1_merge <- left_join(ovrem_09_1_bind, med_found_08_1, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
med_found_08_1_bind <- bind_rows(med_found_08_1_merge, med_found_08_1_subset_new)

#### EST_SERM 9 ####
#medications from Gen 2 Exam 9 and Omni 1 Exam 9
meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
cod_09_1b <- meds_09_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#subset idtype == 1 for just Gen 2
cod_09_1 <- subset(cod_09_1b, idtype == 1)

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_09_1 <- cod_09_1 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_09_1 <- cod_09_1 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_09_1 <- unique(cod_09_1$id)
med_found_09_1 <- data.frame(atc_id_09_1)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_09_1['EST9'] <- as.integer(med_found_09_1$atc_id_09_1 %in% est_filtered_09_1$id)
med_found_09_1['SERM9'] <- as.integer(med_found_09_1$atc_id_09_1 %in% serm_filtered_09_1$id)

#return id and idtype
colnames(med_found_09_1)[1] <- "ID"
med_found_09_1$IDTYPE <- 1

#fully new rows
med_found_09_1_subset <- subset(med_found_09_1, EST9 == 1 | SERM9 == 1)
med_found_09_1_subset_new <- med_found_09_1_subset[!med_found_09_1_subset$ID %in% med_found_08_1_bind$ID,]

#this will leave out the new rows but leave in all the others
med_found_09_1_merge <- left_join(med_found_08_1_bind, med_found_09_1, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
med_found_09_1_bind <- bind_rows(med_found_09_1_merge, med_found_09_1_subset_new)

#ordering ID column
final_09_1 <- med_found_09_1_bind[order(med_found_09_1_bind$ID),]

#pre-merge columns needed
final_09_1[c("REV_DATE", "SERM5", "SERM7")] = NA

#putting columns in order
ordered_09_1 <- final_09_1[,c("ID", "IDTYPE", "STOP_AGE", "STOP_AGELTY", "CAUSE", "EXAMSTOP",
                              "APPROX", "REV_DATE", "EST1", "EST2", "EST3", "EST4", "EST5", 
                              "EST6", "EST7", "EST8", "EST9", "SERM5", "SERM7", "SERM8", "SERM9",
                              "OVREM1", "OVREM2", "OVREM3", "OVREM4", "OVREM5", "OVREM6", "OVREM7",
                              "OVREM8", "OVREM9")]



#### Omni 1 ####

vr_meno_ex03_7 <- read_sas("vr_meno_ex03_7_0916.sas7bdat")

vr_meno_ex03_7_section1 <- vr_meno_ex03_7[c(1:2, 4:6)]
vr_meno_ex03_7_section2 <- vr_meno_ex03_7[c(1:2, 3, 7:15)]

#subset idtype == 7 for just Omni 1
select_09_7 <- subset(select_09_1b, idtype == 7)

stop_09_7 <- subset(select_09_7, select = -c(j060))
#removes 0 and 88 values in both columns (numbers are the same) while keeping NA values for now
menopause_09_7_na_present <- subset(stop_09_7, j052 != 0 & j052 != 88 | is.na(j052))
#remove rows where both j052 columns j053 are NA (while keeping rows where only one of them is)
menopause_09_7 <- menopause_09_7_na_present %>% filter(!if_all(c(j052, j053), is.na))

#only get IDS not already in list
already_stopped_09_7 <- subset(vr_meno_ex03_7, EXAMSTOP > 0) #excludes 0 values

#individuals that newly stopped
newly_stopped_09_7 <- menopause_09_7[!menopause_09_7$id %in% already_stopped_09_7$ID,]
colnames(newly_stopped_09_7)[3:4] <- c("STOPAGE", "CAUSE")
#set exam stopped as 9
newly_stopped_09_7$EXAMSTOP <- 9

#new rows not previously in derived data (not including newly stopped individuals already in dataset)
vr_meno_ex03_7_section1_new <- vr_meno_ex03_7_section1[!vr_meno_ex03_7_section1$ID %in% newly_stopped_09_7$id,]

#The only old columns with updated values are new STOPAGE and CAUSE
colnames(newly_stopped_09_7) <- colnames(vr_meno_ex03_7_section1_new)
vr_meno_ex03_7_section1_full <- rbind(vr_meno_ex03_7_section1_new, newly_stopped_09_7)

#new rows not previously in derived data (not including newly stopped individuals already in dataset)
vr_meno_ex03_7_section1_new <- vr_meno_ex03_7_section1[!vr_meno_ex03_7_section1$ID %in% newly_stopped_09_7$ID,]

#The only old columns with updated values are new STOPAGE and CAUSE
colnames(newly_stopped_09_7) <- colnames(vr_meno_ex03_7_section1_new)
vr_meno_ex03_7_section1_full <- rbind(vr_meno_ex03_7_section1_new, newly_stopped_09_7)

vr_meno_ex03_7_complete <- left_join(vr_meno_ex03_7_section1_full, vr_meno_ex03_7_section2, by = c("IDTYPE", "ID"))

#### OVREM9_7 ####

ovrem_09_7 <- subset(select_09_7, select = -c(j052, j053))
colnames(ovrem_09_7) <- c("IDTYPE", "ID", "OVREM9")

#fully new rows
ovrem_09_7_subset <- subset(ovrem_09_7, OVREM9 == 1 | OVREM9 == 2)
ovrem_09_7_subset_new <- ovrem_09_7_subset[!ovrem_09_7_subset$ID %in% vr_meno_ex03_7_complete$ID,]

#this will leave out the new rows but leave in all the others
ovrem_09_7_merge <- left_join(vr_meno_ex03_7_complete, ovrem_09_7, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
ovrem_09_7_bind <- bind_rows(ovrem_09_7_merge, ovrem_09_7_subset_new)

#### EST_SERM 9_7 ####
#medications from Gen 2 Exam 9 and Omni 1 Exam 9
meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
cod_09_1b <- meds_09_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
#subset idtype == 7 for just Omni 1
cod_09_7 <- subset(cod_09_1b, idtype == 7)

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_09_7 <- cod_09_7 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_09_7 <- cod_09_7 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_09_7 <- unique(cod_09_7$id)
med_found_09_7 <- data.frame(atc_id_09_7)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_09_7['EST9'] <- as.integer(med_found_09_7$atc_id_09_7 %in% est_filtered_09_7$id)
med_found_09_7['SERM9'] <- as.integer(med_found_09_7$atc_id_09_7 %in% serm_filtered_09_7$id)

#return id and idtype
colnames(med_found_09_7)[1] <- "ID"
med_found_09_7$IDTYPE <- 7

#fully new rows
med_found_09_7_subset <- subset(med_found_09_7, EST9 == 1 | SERM9 == 1)
med_found_09_7_subset_new <- med_found_09_7_subset[!med_found_09_7_subset$ID %in% ovrem_09_7_bind$ID,]

#this will leave out the new rows but leave in all the others
med_found_09_7_merge <- left_join(ovrem_09_7_bind, med_found_09_7, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
med_found_09_7_bind <- bind_rows(med_found_09_7_merge, med_found_09_7_subset_new)

#ordering ID column
final_09_7 <- med_found_09_7_bind[order(med_found_09_7_bind$ID),]

#rename columns
final_09_7 <- final_09_7 %>% 
  rename(
    STOP_AGE = STOPAGE,
    EST5 = EST1, EST7 = EST2, EST8 = EST3,
    SERM5 = SERM1, SERM7 = SERM2, SERM8 = SERM3,
    OVREM5 = OVREM1, OVREM7 = OVREM2, OVREM8 = OVREM3)

final_09_7[c("EST1", "EST2", "EST3", "EST4", "EST6",
             "OVREM1", "OVREM2", "OVREM3", "OVREM4", "OVREM6",
             "STOP_AGELTY", "APPROX")] = NA

#putting columns in order
ordered_09_7 <- final_09_7[,c("ID", "IDTYPE", "STOP_AGE", "STOP_AGELTY", "CAUSE", "EXAMSTOP",
                              "APPROX", "REV_DATE", "EST1", "EST2", "EST3", "EST4", "EST5", 
                              "EST6", "EST7", "EST8", "EST9", "SERM5", "SERM7", "SERM8", "SERM9",
                              "OVREM1", "OVREM2", "OVREM3", "OVREM4", "OVREM5", "OVREM6", "OVREM7",
                              "OVREM8", "OVREM9")]


#### Bind up to Exam 9 finished ####
exam_9_both_merge <- bind_rows(ordered_09_1, ordered_09_7)


#### OVREM10 ####

exam_10_1b <- read_sas("e_exam_ex10_1b_1409.sas7bdat")
ovrem_10_1b <- exam_10_1b[,c("idtype", "id", "K0300")]
colnames(ovrem_10_1b) <- c("IDTYPE", "ID", "OVREM10")

ovrem_10_1 <- subset(ovrem_10_1b, IDTYPE == 1)
#fully new rows
ovrem_10_1_subset <- subset(ovrem_10_1, OVREM10 == 1 | OVREM10 == 2)
ovrem_10_1_subset_new <- ovrem_10_1_subset[!ovrem_10_1_subset$ID %in% ordered_09_1$ID,]


ovrem_10_7 <- subset(ovrem_10_1b, IDTYPE == 7)
#fully new rows
ovrem_10_7_subset <- subset(ovrem_10_7, OVREM10 == 1 | OVREM10 == 2)
ovrem_10_7_subset_new <- ovrem_10_7_subset[!ovrem_10_7_subset$ID %in% ordered_09_7$ID,]


ovrem_10_1b_subset_new <- bind_rows(ovrem_10_1_subset_new, ovrem_10_7_subset_new)


#this will leave out the new rows but leave in all the others
ovrem_10_1b_merge <- left_join(exam_9_both_merge, ovrem_10_1b, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
ovrem_10_1b_bind <- bind_rows(ovrem_10_1b_merge, ovrem_10_1b_subset_new)


#### EST_SERM 10 ####
#medications from Gen 2 Exam 10 and Omni 1 Exam 10
meds_10_1b <- read_sas("vr_meds_ex10_1b_1198.sas7bdat")
cod_10_1b <- meds_10_1b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#subset idtype == 1 for just Gen 2
cod_10_1 <- subset(cod_10_1b, idtype == 1)

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_10_1 <- cod_10_1 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_10_1 <- cod_10_1 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_10_1 <- unique(cod_10_1$id)
med_found_10_1 <- data.frame(atc_id_10_1)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_10_1['EST10'] <- as.integer(med_found_10_1$atc_id_10_1 %in% est_filtered_10_1$id)
med_found_10_1['SERM10'] <- as.integer(med_found_10_1$atc_id_10_1 %in% serm_filtered_10_1$id)

#return id and idtype
colnames(med_found_10_1)[1] <- "ID"
med_found_10_1$IDTYPE <- 1

#fully new rows
med_found_10_1_subset <- subset(med_found_10_1, EST10 == 1 | SERM10 == 1)
med_found_10_1_subset_new <- med_found_10_1_subset[!med_found_10_1_subset$ID %in% ordered_09_1$ID,]



#subset idtype == 7 for just Omni 1
cod_10_7 <- subset(cod_10_1b, idtype == 7)

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_10_7 <- cod_10_7 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_10_7 <- cod_10_7 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_10_7 <- unique(cod_10_7$id)
med_found_10_7 <- data.frame(atc_id_10_7)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_10_7['EST10'] <- as.integer(med_found_10_7$atc_id_10_7 %in% est_filtered_10_7$id)
med_found_10_7['SERM10'] <- as.integer(med_found_10_7$atc_id_10_7 %in% serm_filtered_10_7$id)

#return id and idtype
colnames(med_found_10_7)[1] <- "ID"
med_found_10_7$IDTYPE <- 7

#fully new rows
med_found_10_7_subset <- subset(med_found_10_7, EST10 == 1 | SERM10 == 1)
med_found_10_7_subset_new <- med_found_10_7_subset[!med_found_10_7_subset$ID %in% ordered_09_1$ID,]


med_found_10_1b <- bind_rows(med_found_10_1, med_found_10_7)
med_found_10_1b_subset_new <- bind_rows(med_found_10_1_subset_new, med_found_10_7_subset_new)


#this will leave out the new rows but leave in all the others
med_found_10_1b_merge <- left_join(ovrem_10_1b_bind, med_found_10_1b, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM9
med_found_10_1b_bind <- bind_rows(med_found_10_1b_merge, med_found_10_1b_subset_new)


#putting columns in order
ordered_10_1b <- med_found_10_1b_bind[,c("ID", "IDTYPE", "STOP_AGE", "STOP_AGELTY", "CAUSE", 
                                         "EXAMSTOP", "APPROX", "REV_DATE", "EST1", "EST2", "EST3",
                                         "EST4", "EST5", "EST6", "EST7", "EST8", "EST9", "EST10",
                                         "SERM5", "SERM7", "SERM8", "SERM9", "SERM10", "OVREM1",
                                         "OVREM2", "OVREM3", "OVREM4", "OVREM5", "OVREM6", "OVREM7",
                                         "OVREM8", "OVREM9", "OVREM10")]


#writing final CSV to file
write.csv(ordered_10_1b, file = "vr_meno_Gen2_Omni1_exam_10.csv", row.names = FALSE)

