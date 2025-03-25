
##AZQA_MAZHAR
#_______________________#
#Z_SCORES LENGTH & WEIGHT

rm(list = ls())
install.packages("anthro")
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(tidyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(anthro)


output_path <- "~/DESCRIPTIVE_PRISMA/data/07-03-25/output/Z_scores.csv"
mnh09<- read_csv("~/DESCRIPTIVE_PRISMA/data/07-03-25/data/mnh09.csv")
mnh13<- read_csv("~/DESCRIPTIVE_PRISMA/data/07-03-25/data/mnh13.csv")

#select variables from 09 
mnh09<- mnh09 %>% select(MOMID,PREGID,INFANTID_INF1,DELiV_DSSTDAT_INF1,DELIV_DSSTTIM_INF1,SEX_INF1,BIRTH_DSTERM_INF1,
                         INFANTID_INF2,DELiV_DSSTDAT_INF2,DELIV_DSSTTIM_INF2,SEX_INF2,BIRTH_DSTERM_INF2,
                         INFANTID_INF3, DELiV_DSSTDAT_INF3,DELIV_DSSTTIM_INF3,SEX_INF3,BIRTH_DSTERM_INF3)%>% 
  mutate(DELiV_DSSTDAT_INF1 = ymd(parse_date_time(DELiV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         DELiV_DSSTDAT_INF2 = ymd(parse_date_time(DELiV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         DELiV_DSSTDAT_INF3 = ymd(parse_date_time(DELiV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
         SEX_INF1 = as.numeric(SEX_INF1), SEX_INF2 = as.numeric(SEX_INF2), SEX_INF3 = as.numeric(SEX_INF3))%>%
  
# replace default value date with NA 
mutate(DELiV_DSSTDAT_INF1 = replace(DELiV_DSSTDAT_INF1, DELiV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
       DELiV_DSSTDAT_INF2= replace(DELiV_DSSTDAT_INF2, DELiV_DSSTDAT_INF2==ymd("1907-07-07"), NA),
       DELiV_DSSTDAT_INF3 = replace(DELiV_DSSTDAT_INF3, DELiV_DSSTDAT_INF3==ymd("1907-07-07"), NA)) %>%
 # replace default value time with NA 
  mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77", NA),  ## should be 77:77, but pak is using 07:07
         DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77", NA),
         DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77", NA)) %>%
  # Convert time to time format
  mutate(
    DELIV_DSSTTIM_INF1 = if_else(!is.na(DELIV_DSSTTIM_INF1), as.ITime(DELIV_DSSTTIM_INF1), NA),
    DELIV_DSSTTIM_INF2 = if_else(!is.na(DELIV_DSSTTIM_INF2), as.ITime(DELIV_DSSTTIM_INF2), NA),
    DELIV_DSSTTIM_INF3 = if_else(!is.na(DELIV_DSSTTIM_INF3), as.ITime(DELIV_DSSTTIM_INF3), NA)
  )%>%
 # Concatenate dates and times and convert to datetime format
  mutate(
    DELIVERY_DATETIME_INF1 = if_else(!is.na(DELiV_DSSTDAT_INF1) & !is.na(DELIV_DSSTTIM_INF1),
                                     as.POSIXct(paste(DELiV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1), format = "%Y-%m-%d %H:%M:%S"),
                                     DELiV_DSSTDAT_INF1),
    DELIVERY_DATETIME_INF2 = if_else(!is.na(DELiV_DSSTDAT_INF2) & !is.na(DELIV_DSSTTIM_INF2),
                                     as.POSIXct(paste(DELiV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2), format = "%Y-%m-%d %H:%M:%S"),
                                     DELiV_DSSTDAT_INF2),
    DELIVERY_DATETIME_INF3 = if_else(!is.na(DELiV_DSSTDAT_INF3) & !is.na(DELIV_DSSTTIM_INF3),
                                     as.POSIXct(paste(DELiV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3), format = "%Y-%m-%d %H:%M:%S"),
                                     DELiV_DSSTDAT_INF3))
# Getting the Date of Birth, Sex and Birth Outcome for Each ID
Infant_DOB <- mnh09 %>%
  # Pivot the data from wide to long format
  pivot_longer(
    # Select columns to pivot (INFANTID_INF1-4 and DELIVERY_DATETIME_INF1-4)
    cols = c(
      INFANTID_INF1, INFANTID_INF2, INFANTID_INF3,
      DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, DELIVERY_DATETIME_INF3, BIRTH_DSTERM_INF1,
      BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3,SEX_INF1, SEX_INF2, SEX_INF3),
    # Specify how to separate column names: extract suffixes and values
    names_to = c(".value", "infant_suffix"),
    # Define the pattern: splitting by "_INF" and matching the suffix
    names_pattern = "(.*)_INF(\\d)$"
  ) %>%
  # Rename the columns
  rename(
    INFANTID = INFANTID,
    DOB = DELIVERY_DATETIME
  ) %>%
  # Drop the suffix column since it was used for reshaping
  select(MOMID, PREGID, INFANTID, DOB, BIRTH_DSTERM, SEX )  %>%
  # Filter out rows where INFANTID is NA
  filter(INFANTID != "" & INFANTID != "n/a")

#Living infants
Infant_live <- Infant_DOB %>% filter (BIRTH_DSTERM == 1)

if (nrow(Infant_live) > 0) {
#select variable from 13
mnh13<- mnh13%>% 
  filter(TYPE_VISIT %in% c(7, 8, 9, 10, 11, 12) & INF_VITAL_MNH13 == 1 
         & INF_VISIT_MNH13 %in% c(1, 2, 3))
# replace default value date with NA 
mnh13$WEIGHT_PERES[mnh13$WEIGHT_PERES %in% c(-7, -5)] <- NA
mnh13$LENGTH_PERES_1[mnh13$LENGTH_PERES_1 %in% c(-7, -5)] <- NA
mnh13$LENGTH_PERES_2[mnh13$LENGTH_PERES_2 %in% c(-7, -5)] <- NA

#converting in numeric variables
mnh13$LENGTH_PERES_1 <- as.numeric(mnh13$LENGTH_PERES_1)
mnh13$LENGTH_PERES_2 <- as.numeric(mnh13$LENGTH_PERES_2)

#join Infant_DOB with mnh13
Measure_df <- left_join(mnh13, Infant_live, by = c("INFANTID", "MOMID", "PREGID")) %>% 
  mutate(
    Visit_Date = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    age_in_days = round(as.numeric(difftime(Visit_Date, DOB, units = "days")), 0),
    weight = round(WEIGHT_PERES / 1000, 2),
    lenhei = case_when(
      !is.na(LENGTH_PERES_1) & !is.na(LENGTH_PERES_2) ~ round((LENGTH_PERES_1 + LENGTH_PERES_2) / 2, 2),  # Average of LENGTH_PERES_1 and LENGTH_PERES_2
      !is.na(LENGTH_PERES_1) & is.na(LENGTH_PERES_2) ~ LENGTH_PERES_1,  # Use LENGTH_PERES_1 if LENGTH_PERES_2 is missing
      TRUE ~ NA ),
    sex = case_when(
      toupper(SEX) %in% c("M", "1") ~ 1,
      toupper(SEX) %in% c("F", "2") ~ 2,
      TRUE ~ NA_integer_))%>% 
      select(MOMID, PREGID, INFANTID, sex, Visit_Date, 
             TYPE_VISIT, DOB, age_in_days, weight, lenhei)  # Set NA if both are missing

#Filter length weight and age is not n/a 
Z_weight_Length <- Measure_df %>% filter (!is.na(lenhei) & !is.na( weight) & age_in_days > 0) %>%
  mutate(with( ., anthro_zscores(sex =  sex, age = age_in_days, 
                                 weight =  weight, lenhei = lenhei))) %>%
  select(MOMID, PREGID, INFANTID,  sex, Visit_Date, TYPE_VISIT,  DOB, age_in_days,weight, lenhei, 
         clenhei, cbmi, cmeasure, csex, zlen, flen, zwei, fwei, zwfl, fwfl) 
#Rename the Variables to understand 
Z_weight_Length<-Z_weight_Length%>% rename(Weight_for_length=zwfl,
                                           Flag_WFL=fwfl,
                                           Weight_for_age=zwei,
                                           Flag_WFA=fwei,
                                           Length_for_age=zlen,
                                           Flag_LFA=flen)
# defining the cutoff
Z_weight_Length <- Z_weight_Length %>%
  mutate(
    WFL_Cuttoff = case_when(Weight_for_length < -3 ~ "Severe Wasting"),# Assign 'Severe Wasting' if weight-for-length is less than -3
    WFA_Cuttoff = case_when( Weight_for_age < -3 ~ "Severe Underweight"),  # Assign 'Severe Underweight' if weight-for-age is less than -3
    LFA_Cuttoff = case_when(Length_for_age < -3 ~ "Severe Stunting"))  # Assign 'Severe Stunting' if length-for-age is less than -3


# Export the dataset to a CSV file
write.csv(Z_weight_Length, file = output_path, row.names = FALSE)}

#Filtered_Z_Data <- Z_weight_Length %>%
 # filter(abs(zwfl) >= 5 | abs(zwei) >= 5 | abs(zlen) >= 5)
