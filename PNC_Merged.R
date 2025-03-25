rm(list = ls())

library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)

getwd()
setwd("~/DESCRIPTIVE_PRISMA/data/05-03-25/output")

##import all data 
mnh05 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh05.csv")
mnh06 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh06.csv")
mnh04 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh04.csv")
mnh09 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh09.csv")
mnh11 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh11.csv")
mnh12 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh12.csv")
mnh13 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh13.csv")
mnh14 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh14.csv")
mnh15 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh15.csv")
mnh23 <- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh23.csv")
mnh24<- read_csv("~/DESCRIPTIVE_PRISMA/data/05-03-25/data/mnh24.csv")



#************************#
#Miscarriage 
#************************#

Miscarriage <- mnh04 %>%
  filter(PRG_DSDECOD == 2) %>%
  select(MOMID, PREGID, ANC_OBSSTDAT, PRG_DSDECOD, FETAL_LOSS_DSSTDAT) %>%
  mutate(
    ANC_OBSSTDAT = ymd(parse_date_time(ANC_OBSSTDAT, orders = c("d/m/Y", "d-m-Y", "Y-m-d", "d-b-y", "d-m-y"))),
    FETAL_LOSS_DSSTDAT = ymd(parse_date_time(FETAL_LOSS_DSSTDAT, orders = c("d/m/Y", "d-m-Y", "Y-m-d", "d-b-y", "d-m-y")))
  )



#************************#
#Close_out forms
#************************#
mnh23_filter <- mnh23 %>%
  select(MOMID, PREGID, CLOSE_DSSTDAT, CLOSE_DSDECOD) %>%
  mutate(
    Completion_Status = case_when(
      CLOSE_DSDECOD == 1 ~ "1 year complete",
      CLOSE_DSDECOD == 2 ~ "42 days complete",
      CLOSE_DSDECOD == 3 ~ "maternal death",
      CLOSE_DSDECOD == 4 ~ "withdrew consent",
      CLOSE_DSDECOD == 5 ~ "PI Terminated",
      CLOSE_DSDECOD == 6 ~ "LTFU",
      TRUE ~ NA_character_  # Keeps other values as NA
    )
  )%>%
  rename(mnh23_closeout=CLOSE_DSSTDAT,
         Mnh23_completeion=Completion_Status)

mnh24_filter <- mnh24 %>%
  select(MOMID, PREGID, INFANTID,CLOSE_DSSTDAT, CLOSE_DSDECOD) %>%
  mutate(
    Completion_Status = case_when(
      CLOSE_DSDECOD == 1 ~ "1 year complete",
      CLOSE_DSDECOD == 2 ~ "LTFU",
      CLOSE_DSDECOD == 3 ~ "Infant Died ",
      CLOSE_DSDECOD == 4 ~ "withdrew consent",
      CLOSE_DSDECOD == 9 ~ "Mother died",
      CLOSE_DSDECOD == 6 ~ "Stillbirth",
      TRUE ~ NA_character_  # Keeps other values as NA
    )
  )%>%
  rename(mnh24_closeout=CLOSE_DSSTDAT,
         Mnh24_completeion=Completion_Status)

#************************#
#IPC forms
#************************#
mnh05_IPC<-mnh05%>%
  filter(TYPE_VISIT==6)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)

mnh06_IPC<-mnh06%>%
  filter(TYPE_VISIT==6)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)

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
date_of_sharing <- as.Date("2025-02-28") 
dilvered <- mnh09 %>%
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
  filter(INFANTID != "" & INFANTID != "n/a")%>%
  mutate(
    DOB = as.Date(DOB),  # Convert DOB to Date format
    AGE_DAYS = as.numeric(date_of_sharing - DOB)  # Now subtraction works correctly
  )

IPC_visit<-mnh11%>%
select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT)%>%
  full_join(dilvered, by = c("MOMID", "PREGID","INFANTID"))%>%
full_join(mnh05_IPC, by = c("MOMID", "PREGID"))%>%
full_join(mnh06_IPC, by = c("MOMID", "PREGID"))%>%
  rename(mnh11_date=VISIT_OBSSTDAT,
         mnh05_date=ANT_PEDAT,
         mnh06_date=DIAG_VSDAT)%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh11 = ifelse(is.na(mnh11_date), "Missing in mnh11", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    )

#************************#
#PNC0 forms
#************************#

mnh05_PNC0<-mnh05%>%
  filter(TYPE_VISIT==7)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)%>%
  mutate(
    ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh06_PNC0<-mnh06%>%
  filter(TYPE_VISIT==7)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)%>%
  mutate(
    DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh12_PNC0<-mnh12%>%
  filter(TYPE_VISIT==7)%>%
  select(MOMID,PREGID,VISIT_OBSSTDAT,MAT_VISIT_MNH12)%>%
  rename(mnh12_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh12_date = ymd(parse_date_time(mnh12_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh13_PNC0<-mnh13%>%
  filter(TYPE_VISIT==7)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH13)%>%
  rename(mnh13_date=VISIT_OBSSTDAT)%>%
mutate(
  mnh13_date = ymd(parse_date_time(mnh13_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh14_PNC0<-mnh14%>%
  filter(TYPE_VISIT==7)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH14)%>%
  rename(mnh14_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh14_date = ymd(parse_date_time(mnh14_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh15_PNC0<-mnh15%>%
  filter(TYPE_VISIT==7)%>%
  select(MOMID,PREGID,INFANTID,OBSSTDAT,INF_VISIT_MNH15)%>%
  rename(mnh15_date=OBSSTDAT)%>%
  mutate(
    mnh15_date = ymd(parse_date_time(mnh15_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


# Step 1: Join maternal datasets first (without INFANTID)
PNC0_merge <- dilvered %>%
  filter(AGE_DAYS >= 3) %>%
  full_join(mnh05_PNC0, by = c("MOMID", "PREGID")) %>%
  full_join(mnh06_PNC0, by = c("MOMID", "PREGID")) %>%
  full_join(mnh12_PNC0, by = c("MOMID", "PREGID")) %>%
  left_join(mnh23_filter, by = c("MOMID", "PREGID")) %>%
  left_join(Miscarriage %>% select(MOMID, PREGID, FETAL_LOSS_DSSTDAT), by = c("MOMID", "PREGID"))

# Step 2: Now merge datasets that require INFANTID
PNC0_merge <- PNC0_merge %>%
  full_join(mnh13_PNC0, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh14_PNC0, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh15_PNC0, by = c("MOMID", "PREGID", "INFANTID")) %>%
  left_join(mnh24_filter, by = c("MOMID", "PREGID", "INFANTID")) %>%
  rename(
    mnh05_date = ANT_PEDAT,
    mnh06_date = DIAG_VSDAT
  ) %>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    missing_mnh12 = ifelse(is.na(mnh12_date), "Missing in mnh12", "Present"),
    missing_mnh13 = ifelse(is.na(mnh13_date), "Missing in mnh13", "Present"),
    missing_mnh14 = ifelse(is.na(mnh14_date), "Missing in mnh14", "Present"),
    missing_mnh15 = ifelse(is.na(mnh15_date), "Missing in mnh15", "Present")
  ) %>%
  distinct()



#************************#
#PNC1 forms
#************************#

mnh05_PNC1<-mnh05%>%
  filter(TYPE_VISIT==8)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)%>%
  mutate(
    ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh06_PNC1<-mnh06%>%
  filter(TYPE_VISIT==8)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)%>%
  mutate(
    DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh12_PNC1<-mnh12%>%
  filter(TYPE_VISIT==8)%>%
  select(MOMID,PREGID,VISIT_OBSSTDAT,MAT_VISIT_MNH12)%>%
  rename(mnh12_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh12_date = ymd(parse_date_time(mnh12_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh13_PNC1<-mnh13%>%
  filter(TYPE_VISIT==8)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH13)%>%
  rename(mnh13_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh13_date = ymd(parse_date_time(mnh13_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh14_PNC1<-mnh14%>%
  filter(TYPE_VISIT==8)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH14)%>%
  rename(mnh14_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh14_date = ymd(parse_date_time(mnh14_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh15_PNC1<-mnh15%>%
  filter(TYPE_VISIT==8)%>%
  select(MOMID,PREGID,INFANTID,OBSSTDAT,INF_VISIT_MNH15)%>%
  rename(mnh15_date=OBSSTDAT)%>%
  mutate(
    mnh15_date = ymd(parse_date_time(mnh15_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

# Step 1: Join maternal datasets first (without INFANTID)
PNC1_merge <- dilvered %>%
  filter(AGE_DAYS >= 7) %>%
  full_join(mnh05_PNC1, by = c("MOMID", "PREGID")) %>%
  full_join(mnh06_PNC1, by = c("MOMID", "PREGID")) %>%
  full_join(mnh12_PNC1, by = c("MOMID", "PREGID")) %>%
  left_join(Miscarriage %>% select(MOMID, PREGID, FETAL_LOSS_DSSTDAT), by = c("MOMID", "PREGID")) %>%
  left_join(mnh23_filter, by = c("MOMID", "PREGID"))
# Step 2: Now merge datasets that require INFANTID
PNC1_merge <- PNC1_merge %>%
  full_join(mnh13_PNC1, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh14_PNC1, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh15_PNC1, by = c("MOMID", "PREGID", "INFANTID")) %>%
  left_join(mnh24_filter, by = c("MOMID", "PREGID", "INFANTID")) %>%
  rename(
    mnh05_date = ANT_PEDAT,
    mnh06_date = DIAG_VSDAT
  ) %>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    missing_mnh12 = ifelse(is.na(mnh12_date), "Missing in mnh12", "Present"),
    missing_mnh13 = ifelse(is.na(mnh13_date), "Missing in mnh13", "Present"),
    missing_mnh14 = ifelse(is.na(mnh14_date), "Missing in mnh14", "Present"),
    missing_mnh15 = ifelse(is.na(mnh15_date), "Missing in mnh15", "Present")
  ) %>%
  distinct()
#************************#
#PNC4 forms
#************************#

mnh05_PNC4<-mnh05%>%
  filter(TYPE_VISIT==9)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)%>%
  mutate(
    ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh06_PNC4<-mnh06%>%
  filter(TYPE_VISIT==9)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)%>%
  mutate(
    DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh12_PNC4<-mnh12%>%
  filter(TYPE_VISIT==9)%>%
  select(MOMID,PREGID,VISIT_OBSSTDAT,MAT_VISIT_MNH12)%>%
  rename(mnh12_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh12_date = ymd(parse_date_time(mnh12_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh13_PNC4<-mnh13%>%
  filter(TYPE_VISIT==9)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH13)%>%
  rename(mnh13_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh13_date = ymd(parse_date_time(mnh13_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh14_PNC4<-mnh14%>%
  filter(TYPE_VISIT==9)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH14)%>%
  rename(mnh14_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh14_date = ymd(parse_date_time(mnh14_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh15_PNC4<-mnh15%>%
  filter(TYPE_VISIT==9)%>%
  select(MOMID,PREGID,INFANTID,OBSSTDAT,INF_VISIT_MNH15)%>%
  rename(mnh15_date=OBSSTDAT)%>%
  mutate(
    mnh15_date = ymd(parse_date_time(mnh15_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


# Step 1: Join maternal datasets first (without INFANTID)
PNC4_merge <- dilvered %>%
  filter(AGE_DAYS >= 28) %>%
  full_join(mnh05_PNC4, by = c("MOMID", "PREGID")) %>%
  full_join(mnh06_PNC4, by = c("MOMID", "PREGID")) %>%
  full_join(mnh12_PNC4, by = c("MOMID", "PREGID")) %>%
  left_join(mnh23_filter, by = c("MOMID", "PREGID")) %>%
  left_join(Miscarriage %>% select(MOMID, PREGID, FETAL_LOSS_DSSTDAT), by = c("MOMID", "PREGID"))

# Step 2: Now merge datasets that require INFANTID
PNC4_merge <- PNC4_merge %>%
  full_join(mnh13_PNC4, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh14_PNC4, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh15_PNC4, by = c("MOMID", "PREGID", "INFANTID")) %>%
  left_join(mnh24_filter, by = c("MOMID", "PREGID", "INFANTID")) %>%
  rename(
    mnh05_date = ANT_PEDAT,
    mnh06_date = DIAG_VSDAT
  ) %>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    missing_mnh12 = ifelse(is.na(mnh12_date), "Missing in mnh12", "Present"),
    missing_mnh13 = ifelse(is.na(mnh13_date), "Missing in mnh13", "Present"),
    missing_mnh14 = ifelse(is.na(mnh14_date), "Missing in mnh14", "Present"),
    missing_mnh15 = ifelse(is.na(mnh15_date), "Missing in mnh15", "Present")
  ) %>%
  distinct()


#************************#
#PNC 6 forms
#************************#

mnh05_PNC6<-mnh05%>%
  filter(TYPE_VISIT==10)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)%>%
  mutate(
    ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh06_PNC6<-mnh06%>%
  filter(TYPE_VISIT==10)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)%>%
  mutate(
    DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh12_PNC6<-mnh12%>%
  filter(TYPE_VISIT==10)%>%
  select(MOMID,PREGID,VISIT_OBSSTDAT,MAT_VISIT_MNH12)%>%
  rename(mnh12_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh12_date = ymd(parse_date_time(mnh12_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh13_PNC6<-mnh13%>%
  filter(TYPE_VISIT==10)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH13)%>%
  rename(mnh13_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh13_date = ymd(parse_date_time(mnh13_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh14_PNC6<-mnh14%>%
  filter(TYPE_VISIT==10)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH14)%>%
  rename(mnh14_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh14_date = ymd(parse_date_time(mnh14_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh15_PNC6<-mnh15%>%
  filter(TYPE_VISIT==10)%>%
  select(MOMID,PREGID,INFANTID,OBSSTDAT,INF_VISIT_MNH15)%>%
  rename(mnh15_date=OBSSTDAT)%>%
  mutate(
    mnh15_date = ymd(parse_date_time(mnh15_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


# Step 1: Join maternal datasets first (without INFANTID)
PNC6_merge <- dilvered %>%
  filter(AGE_DAYS >= 42) %>%
  full_join(mnh05_PNC6, by = c("MOMID", "PREGID")) %>%
  full_join(mnh06_PNC6, by = c("MOMID", "PREGID")) %>%
  full_join(mnh12_PNC6, by = c("MOMID", "PREGID")) %>%
  left_join(mnh23_filter, by = c("MOMID", "PREGID")) %>%
  left_join(Miscarriage %>% select(MOMID, PREGID, FETAL_LOSS_DSSTDAT), by = c("MOMID", "PREGID"))

# Step 2: Now merge datasets that require INFANTID
PNC6_merge <- PNC6_merge %>%
  full_join(mnh13_PNC6, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh14_PNC6, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh15_PNC6, by = c("MOMID", "PREGID", "INFANTID")) %>%
  left_join(mnh24_filter, by = c("MOMID", "PREGID", "INFANTID")) %>%
  rename(
    mnh05_date = ANT_PEDAT,
    mnh06_date = DIAG_VSDAT
  ) %>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    missing_mnh12 = ifelse(is.na(mnh12_date), "Missing in mnh12", "Present"),
    missing_mnh13 = ifelse(is.na(mnh13_date), "Missing in mnh13", "Present"),
    missing_mnh14 = ifelse(is.na(mnh14_date), "Missing in mnh14", "Present"),
    missing_mnh15 = ifelse(is.na(mnh15_date), "Missing in mnh15", "Present")
  ) %>%
  distinct()

#************************#
#PNC26 forms
#************************#

mnh05_PNC26<-mnh05%>%
  filter(TYPE_VISIT==11)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)%>%
  mutate(
    ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh06_PNC26<-mnh06%>%
  filter(TYPE_VISIT==11)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)%>%
  mutate(
    DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh12_PNC26<-mnh12%>%
  filter(TYPE_VISIT==11)%>%
  select(MOMID,PREGID,VISIT_OBSSTDAT,MAT_VISIT_MNH12)%>%
  rename(mnh12_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh12_date = ymd(parse_date_time(mnh12_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh13_PNC26<-mnh13%>%
  filter(TYPE_VISIT==11)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH13)%>%
  rename(mnh13_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh13_date = ymd(parse_date_time(mnh13_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh14_PNC26<-mnh14%>%
  filter(TYPE_VISIT==11)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH14)%>%
  rename(mnh14_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh14_date = ymd(parse_date_time(mnh14_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh15_PNC26<-mnh15%>%
  filter(TYPE_VISIT==11)%>%
  select(MOMID,PREGID,INFANTID,OBSSTDAT,INF_VISIT_MNH15)%>%
  rename(mnh15_date=OBSSTDAT)%>%
  mutate(
    mnh15_date = ymd(parse_date_time(mnh15_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


# Step 1: Join maternal datasets first (without INFANTID)
PNC26_merge <- dilvered %>%
  filter(AGE_DAYS >= 182) %>%
  full_join(mnh05_PNC26, by = c("MOMID", "PREGID")) %>%
  full_join(mnh06_PNC26, by = c("MOMID", "PREGID")) %>%
  full_join(mnh12_PNC26, by = c("MOMID", "PREGID")) %>%
  left_join(mnh23_filter, by = c("MOMID", "PREGID"))

# Step 2: Now merge datasets that require INFANTID
PNC26_merge <- PNC26_merge %>%
  full_join(mnh13_PNC26, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh14_PNC26, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh15_PNC26, by = c("MOMID", "PREGID", "INFANTID")) %>%
  left_join(mnh24_filter, by = c("MOMID", "PREGID", "INFANTID")) %>%
  rename(
    mnh05_date = ANT_PEDAT,
    mnh06_date = DIAG_VSDAT
  ) %>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    missing_mnh12 = ifelse(is.na(mnh12_date), "Missing in mnh12", "Present"),
    missing_mnh13 = ifelse(is.na(mnh13_date), "Missing in mnh13", "Present"),
    missing_mnh14 = ifelse(is.na(mnh14_date), "Missing in mnh14", "Present"),
    missing_mnh15 = ifelse(is.na(mnh15_date), "Missing in mnh15", "Present")
  ) %>%
  distinct()

#************************#
#PNC52 forms
#************************#

mnh05_PNC52<-mnh05%>%
  filter(TYPE_VISIT==12)%>%
  select(MOMID,PREGID,ANT_PEDAT,MAT_VISIT_MNH05)%>%
  mutate(
    ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh06_PNC52<-mnh06%>%
  filter(TYPE_VISIT==12)%>%
  select(MOMID,PREGID,DIAG_VSDAT,MAT_VISIT_MNH06)%>%
  mutate(
    DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))

mnh12_PNC52<-mnh12%>%
  filter(TYPE_VISIT==12)%>%
  select(MOMID,PREGID,VISIT_OBSSTDAT,MAT_VISIT_MNH12)%>%
  rename(mnh12_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh12_date = ymd(parse_date_time(mnh12_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh13_PNC52<-mnh13%>%
  filter(TYPE_VISIT==12)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH13)%>%
  rename(mnh13_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh13_date = ymd(parse_date_time(mnh13_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh14_PNC52<-mnh14%>%
  filter(TYPE_VISIT==12)%>%
  select(MOMID,PREGID,INFANTID,VISIT_OBSSTDAT,INF_VISIT_MNH14)%>%
  rename(mnh14_date=VISIT_OBSSTDAT)%>%
  mutate(
    mnh14_date = ymd(parse_date_time(mnh14_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


mnh15_PNC52<-mnh15%>%
  filter(TYPE_VISIT==12)%>%
  select(MOMID,PREGID,INFANTID,OBSSTDAT,INF_VISIT_MNH15)%>%
  rename(mnh15_date=OBSSTDAT)%>%
  mutate(
    mnh15_date = ymd(parse_date_time(mnh15_date, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))


# Step 1: Join maternal datasets first (without INFANTID)
PNC52_merge <- dilvered %>%
  filter(AGE_DAYS >= 364) %>%
  full_join(mnh05_PNC26, by = c("MOMID", "PREGID")) %>%
  full_join(mnh06_PNC26, by = c("MOMID", "PREGID")) %>%
  full_join(mnh12_PNC26, by = c("MOMID", "PREGID")) %>%
  left_join(mnh23_filter, by = c("MOMID", "PREGID"))

# Step 2: Now merge datasets that require INFANTID
PNC52_merge <- PNC52_merge %>%
  full_join(mnh13_PNC26, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh14_PNC26, by = c("MOMID", "PREGID", "INFANTID")) %>%
  full_join(mnh15_PNC26, by = c("MOMID", "PREGID", "INFANTID")) %>%
  left_join(mnh24_filter, by = c("MOMID", "PREGID", "INFANTID")) %>%
  rename(
    mnh05_date = ANT_PEDAT,
    mnh06_date = DIAG_VSDAT
  ) %>%
  mutate(
    missing_mnh05 = ifelse(is.na(mnh05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(mnh06_date), "Missing in mnh06", "Present"),
    missing_mnh09 = ifelse(is.na(DOB), "Missing in mnh09", "Present"),
    missing_mnh12 = ifelse(is.na(mnh12_date), "Missing in mnh12", "Present"),
    missing_mnh13 = ifelse(is.na(mnh13_date), "Missing in mnh13", "Present"),
    missing_mnh14 = ifelse(is.na(mnh14_date), "Missing in mnh14", "Present"),
    missing_mnh15 = ifelse(is.na(mnh15_date), "Missing in mnh15", "Present")
  ) %>%
  distinct()

# Export multiple sheets in one Excel file
write.xlsx(list(
  IPC = IPC_visit,
  PNC_0 = PNC0_merge,
  PNC_1 = PNC1_merge,
  PNC_4 = PNC4_merge,
  PNC_6 = PNC6_merge,
  PNC_26 = PNC26_merge,
  PNC_52 = PNC52_merge
), file = "PNC_forms.xlsx", overwrite = TRUE)


