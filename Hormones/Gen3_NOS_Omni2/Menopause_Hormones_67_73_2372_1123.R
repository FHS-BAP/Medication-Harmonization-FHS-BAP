# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS Menopause, Hormone Replacement and Ovary Removal Exam 3 dataset extension/unification source code
# ******************************************************************************************************************************************
# 
# Created by Michael Cummings
# Last updated: November 2023
# 
# 
# The purpose of this R code is to extend the Gen 3/Omni 2/NOS menopause, hormone replacement and ovary removal dataset with Exam 3 data,
# as well as unify the datasets.
# The new data is:
# 1) Exam 3 menopause information.
# 2) Exam 3 hormone replacement drug information.
# 3) Exam 3 ovary removal information.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Menopause, Hormone Replacement and Ovary Removal through Exam 2:
# vr_meno_ex02_3_0653_v1.sas7bdat (Gen 3 through Exam 2)
# vr_meno_ex02_72_0720.sas7bdat (Omni 2 through Exam 2)
# vr_meno_ex02_2_0719.sas7bdat (NOS through Exam 2)
#
# 2)  Individual FHS exam questionnaire:
# e_exam_ex03_3b_1069.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
#
# 3)  Hormone ATC information - ATC2_HormonesCodes_ALL_20231012.xlsx
# 
# 4)  Full ATC medication info for patients:
# vr_meds_ex03_3b_1071_v1.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files


#### Reading ####

#read important sections
exam_03_3b <- read_sas("e_exam_ex03_3b_1069.sas7bdat")
select_03_3b <- exam_03_3b[,c("idtype", "id", "G3C0234", "G3C0235", "G3C0243")]

#reading two drug lists from excel spreadsheet
est_comb_sheet <- read_excel("ATC2_HormonesCodes_ALL_20231012.xlsx", sheet = "Est_Comb")
est_atc_list <- est_comb_sheet$`ATC Code for medication`
serm_sheet <- read_excel("ATC2_HormonesCodes_ALL_20231012.xlsx", sheet = "SERM")
serm_atc_list <- serm_sheet$`ATC Code for medication`


#### Gen 3 ####

vr_meno_ex02_3 <- read_sas("vr_meno_ex02_3_0653_v1.sas7bdat")

vr_meno_ex02_3_section1 <- vr_meno_ex02_3[1:5]
vr_meno_ex02_3_section2 <- vr_meno_ex02_3[c(1:2, 6:11)]

#G3C0234 is age when periods stopped, 
#G3C0235 is cause of stop
#G3C0243 is ovary removal info
#EST3 and SERM3 based on medication data

#subset idtype == 3 for just Gen 3
select_03_3 <- subset(select_03_3b, idtype == 3)

stop_03_3 <- subset(select_03_3, select = -c(G3C0243))
#removes 0 and 88 values in both columns (numbers are the same) while keeping NA values for now
menopause_03_3_na_present <- subset(stop_03_3, G3C0234 != 0 & G3C0234 != 88 | is.na(G3C0234))
#remove rows where both G3C0234 columns G3C0235 are NA (while keeping rows where only one of them is)
menopause_03_3 <- menopause_03_3_na_present %>% filter(!if_all(c(G3C0234, G3C0235), is.na))

#only get IDS not already in list
already_stopped_03_3 <- subset(vr_meno_ex02_3, EXAMSTOP > 0) #excludes 0 values

#individuals that newly stopped 
newly_stopped_03_3 <- menopause_03_3[!menopause_03_3$id %in% already_stopped_03_3$ID,]
colnames(newly_stopped_03_3)[3:4] <- c("STOPAGE", "CAUSE")
#set exam stopped as 3
newly_stopped_03_3$EXAMSTOP <- 3

#new rows not previously in derived data (not including newly stopped individuals already in dataset)
vr_meno_ex02_3_section1_new <- vr_meno_ex02_3_section1[!vr_meno_ex02_3_section1$ID %in% newly_stopped_03_3$id,]

#The only old columns with updated values are new STOPAGE and CAUSE
colnames(newly_stopped_03_3) <- colnames(vr_meno_ex02_3_section1_new)
vr_meno_ex02_3_section1_full <- rbind(vr_meno_ex02_3_section1_new, newly_stopped_03_3)

vr_meno_ex02_3_complete <- left_join(vr_meno_ex02_3_section1_full, vr_meno_ex02_3_section2, by = c("IDTYPE", "ID"))

#### OVREM3 ####

ovrem_03_3 <- subset(select_03_3, select = -c(G3C0234, G3C0235))
colnames(ovrem_03_3) <- c("IDTYPE", "ID", "OVREM3")

#fully new rows
ovrem_03_3_subset <- subset(ovrem_03_3, OVREM3 == 1 | OVREM3 == 2)
ovrem_03_3_subset_new <- ovrem_03_3_subset[!ovrem_03_3_subset$ID %in% vr_meno_ex02_3_complete$ID,]

#this will leave out the new rows but leave in all the others
ovrem_03_3_merge <- left_join(vr_meno_ex02_3_complete, ovrem_03_3, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM3
ovrem_03_3_bind <- bind_rows(ovrem_03_3_merge, ovrem_03_3_subset_new)

#### EST_SERM ####
#medications from Gen 3 Exam 3, NOS Exam 3 and Omni 2 Exam 3
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
cod_03_3b <- meds_03_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#subset idtype == 3 for just Gen 3
cod_03_3 <- subset(cod_03_3b, idtype == 3)


#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_03_3 <- cod_03_3 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_03_3 <- cod_03_3 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_03_3 <- unique(cod_03_3$id)
med_found_03_3 <- data.frame(atc_id_03_3)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_3['EST3'] <- as.integer(med_found_03_3$atc_id_03_3 %in% est_filtered_03_3$id)
med_found_03_3['SERM3'] <- as.integer(med_found_03_3$atc_id_03_3 %in% serm_filtered_03_3$id)

#return id and idtype
colnames(med_found_03_3)[1] <- "ID"
med_found_03_3$IDTYPE <- 3

#fully new rows
med_found_03_3_subset <- subset(med_found_03_3, EST3 == 1 | SERM3 == 1)
med_found_03_3_subset_new <- med_found_03_3_subset[!med_found_03_3_subset$ID %in% ovrem_03_3_bind$ID,]

#this will leave out the new rows but leave in all the others
med_found_03_3_merge <- left_join(ovrem_03_3_bind, med_found_03_3, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM3
med_found_03_3_bind <- bind_rows(med_found_03_3_merge, med_found_03_3_subset_new)

#ordering ID column
final_03_3 <- med_found_03_3_bind[order(med_found_03_3_bind$ID),]

#### Omni 2 ####

vr_meno_ex02_72 <- read_sas("vr_meno_ex02_72_0720.sas7bdat")

vr_meno_ex02_72_section1 <- vr_meno_ex02_72[1:5]
vr_meno_ex02_72_section2 <- vr_meno_ex02_72[c(1:2, 6:11)]

#G3C0234 is age when periods stopped, 
#G3C0235 is cause of stop
#G3C0243 is ovary removal info
#EST3 and SERM3 based on medication data

#subset idtype == 72 for just Omni 2
select_03_72 <- subset(select_03_3b, idtype == 72)

stop_03_72 <- subset(select_03_72, select = -c(G3C0243))
#removes 0 and 88 values in both columns (numbers are the same) while keeping NA values for now
menopause_03_72_na_present <- subset(stop_03_72, G3C0234 != 0 & G3C0234 != 88 | is.na(G3C0234))
#remove rows where both G3C0234 columns G3C0235 are NA (while keeping rows where only one of them is)
menopause_03_72 <- menopause_03_72_na_present %>% filter(!if_all(c(G3C0234, G3C0235), is.na))

#only get IDS not already in list
already_stopped_03_72 <- subset(vr_meno_ex02_72, EXAMSTOP > 0) #excludes 0 values

#individuals that newly stopped
newly_stopped_03_72 <- menopause_03_72[!menopause_03_72$id %in% already_stopped_03_72$ID,]
colnames(newly_stopped_03_72)[3:4] <- c("STOPAGE", "CAUSE")
#set exam stopped as 3
newly_stopped_03_72$EXAMSTOP <- 3

#new rows not previously in derived data (not including newly stopped individuals already in dataset)
vr_meno_ex02_72_section1_new <- vr_meno_ex02_72_section1[!vr_meno_ex02_72_section1$ID %in% newly_stopped_03_72$id,]

#The only old columns with updated values are new STOPAGE and CAUSE
colnames(newly_stopped_03_72) <- colnames(vr_meno_ex02_72_section1_new)
vr_meno_ex02_72_section1_full <- rbind(vr_meno_ex02_72_section1_new, newly_stopped_03_72)

vr_meno_ex02_72_complete <- left_join(vr_meno_ex02_72_section1_full, vr_meno_ex02_72_section2, by = c("IDTYPE", "ID"))

#### OVREM3 ####

ovrem_03_72 <- subset(select_03_72, select = -c(G3C0234, G3C0235))
colnames(ovrem_03_72) <- c("IDTYPE", "ID", "OVREM3")

#fully new rows
ovrem_03_72_subset <- subset(ovrem_03_72, OVREM3 == 1 | OVREM3 == 2)
ovrem_03_72_subset_new <- ovrem_03_72_subset[!ovrem_03_72_subset$ID %in% vr_meno_ex02_72_complete$ID,]

#this will leave out the new rows but leave in all the others
ovrem_03_72_merge <- left_join(vr_meno_ex02_72_complete, ovrem_03_72, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM3
ovrem_03_72_bind <- bind_rows(ovrem_03_72_merge, ovrem_03_72_subset_new)

#### EST_SERM ####
#medications from Gen 3 Exam 3, NOS Exam 3 and Omni 2 Exam 3
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
cod_03_3b <- meds_03_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#subset idtype == 72 for just Omni 2
cod_03_72 <- subset(cod_03_3b, idtype == 72)


#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_03_72 <- cod_03_72 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_03_72 <- cod_03_72 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_03_72 <- unique(cod_03_72$id)
med_found_03_72 <- data.frame(atc_id_03_72)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_72['EST3'] <- as.integer(med_found_03_72$atc_id_03_72 %in% est_filtered_03_72$id)
med_found_03_72['SERM3'] <- as.integer(med_found_03_72$atc_id_03_72 %in% serm_filtered_03_72$id)

#return id and idtype
colnames(med_found_03_72)[1] <- "ID"
med_found_03_72$IDTYPE <- 72

#fully new rows
med_found_03_72_subset <- subset(med_found_03_72, EST3 == 1 | SERM3 == 1)
med_found_03_72_subset_new <- med_found_03_72_subset[!med_found_03_72_subset$ID %in% ovrem_03_72_bind$ID,]

#this will leave out the new rows but leave in all the others
med_found_03_72_merge <- left_join(ovrem_03_72_bind, med_found_03_72, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM3
med_found_03_72_bind <- bind_rows(med_found_03_72_merge, med_found_03_72_subset_new)

#ordering ID column
final_03_72 <- med_found_03_72_bind[order(med_found_03_72_bind$ID),]

#### NOS ####

vr_meno_ex02_2 <- read_sas("vr_meno_ex02_2_0719.sas7bdat")

vr_meno_ex02_2_section1 <- vr_meno_ex02_2[1:5]
vr_meno_ex02_2_section2 <- vr_meno_ex02_2[c(1:2, 6:11)]

#G3C0234 is age when periods stopped, 
#G3C0235 is cause of stop
#G3C0243 is ovary removal info
#EST3 and SERM3 based on medication data

#subset idtype == 2 for just NOS
select_03_2 <- subset(select_03_3b, idtype == 2)

stop_03_2 <- subset(select_03_2, select = -c(G3C0243))
#removes 0 and 88 values in both columns (numbers are the same) while keeping NA values for now
menopause_03_2_na_present <- subset(stop_03_2, G3C0234 != 0 & G3C0234 != 88 | is.na(G3C0234))
#remove rows where both G3C0234 columns G3C0235 are NA (while keeping rows where only one of them is)
menopause_03_2 <- menopause_03_2_na_present %>% filter(!if_all(c(G3C0234, G3C0235), is.na))

#only get IDS not already in list
already_stopped_03_2 <- subset(vr_meno_ex02_2, EXAMSTOP > 0) #excludes 0 values

#individuals that newly stopped
newly_stopped_03_2 <- menopause_03_2[!menopause_03_2$id %in% already_stopped_03_2$ID,]
colnames(newly_stopped_03_2)[3:4] <- c("STOPAGE", "CAUSE")
#set exam stopped as 3
newly_stopped_03_2$EXAMSTOP <- 3

#new rows not previously in derived data (not including newly stopped individuals already in dataset)
vr_meno_ex02_2_section1_new <- vr_meno_ex02_2_section1[!vr_meno_ex02_2_section1$ID %in% newly_stopped_03_2$id,]

#The only old columns with updated values are new STOPAGE and CAUSE
colnames(newly_stopped_03_2) <- colnames(vr_meno_ex02_2_section1_new)
vr_meno_ex02_2_section1_full <- rbind(vr_meno_ex02_2_section1_new, newly_stopped_03_2)

vr_meno_ex02_2_complete <- left_join(vr_meno_ex02_2_section1_full, vr_meno_ex02_2_section2, by = c("IDTYPE", "ID"))

#### OVREM3 ####

ovrem_03_2 <- subset(select_03_2, select = -c(G3C0234, G3C0235))
colnames(ovrem_03_2) <- c("IDTYPE", "ID", "OVREM3")

#fully new rows
ovrem_03_2_subset <- subset(ovrem_03_2, OVREM3 == 1 | OVREM3 == 2)
ovrem_03_2_subset_new <- ovrem_03_2_subset[!ovrem_03_2_subset$ID %in% vr_meno_ex02_2_complete$ID,]

#this will leave out the new rows but leave in all the others
ovrem_03_2_merge <- left_join(vr_meno_ex02_2_complete, ovrem_03_2, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM3
ovrem_03_2_bind <- bind_rows(ovrem_03_2_merge, ovrem_03_2_subset_new)

#### EST_SERM ####
#medications from Gen 3 Exam 3, NOS Exam 3 and Omni 2 Exam 3
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
cod_03_3b <- meds_03_3b[,c("id", "idtype", "atc_cod1", "atc_cod2", "atc_cod3")]
#subset idtype == 2 for just NOS
cod_03_2 <- subset(cod_03_3b, idtype == 2)


#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of these types
est_filtered_03_2 <- cod_03_2 %>% filter_all(any_vars(. %in% est_atc_list))
serm_filtered_03_2 <- cod_03_2 %>% filter_all(any_vars(. %in% serm_atc_list))
#Constructing the output data frame
atc_id_03_2 <- unique(cod_03_2$id)
med_found_03_2 <- data.frame(atc_id_03_2)
#Formatting ATC rows into 0 or 1 notation for absent or present
med_found_03_2['EST3'] <- as.integer(med_found_03_2$atc_id_03_2 %in% est_filtered_03_2$id)
med_found_03_2['SERM3'] <- as.integer(med_found_03_2$atc_id_03_2 %in% serm_filtered_03_2$id)

#return id and idtype
colnames(med_found_03_2)[1] <- "ID"
med_found_03_2$IDTYPE <- 2

#fully new rows
med_found_03_2_subset <- subset(med_found_03_2, EST3 == 1 | SERM3 == 1)
med_found_03_2_subset_new <- med_found_03_2_subset[!med_found_03_2_subset$ID %in% ovrem_03_2_bind$ID,]

#this will leave out the new rows but leave in all the others
med_found_03_2_merge <- left_join(ovrem_03_2_bind, med_found_03_2, by = c("IDTYPE", "ID"))

#this brings in the new rows and leaves NA for all values except IDTYPE, ID, and OVREM3
med_found_03_2_bind <- bind_rows(med_found_03_2_merge, med_found_03_2_subset_new)

#ordering ID column
final_03_2 <- med_found_03_2_bind[order(med_found_03_2_bind$ID),]

#### Bind ####
final_03_3b <- bind_rows(final_03_2, final_03_3, final_03_72)
#writing final CSV to file
write.csv(final_03_3b, file = "Menopause_Hormones_67_73_2372_1123.csv", row.names = FALSE)

