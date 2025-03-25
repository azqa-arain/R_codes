rm(list = ls())

# load packages 

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
setwd("~/DESCRIPTIVE_PRISMA/data/03-03-25/output")

##import all data 
library(readr)
mnh00 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh00.csv")
mnh01 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh01.csv")
mnh02 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh02.csv")
mnh03 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh03.csv")
mnh04 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh04.csv")
mnh05 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh05.csv")
mnh06 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh06.csv")
mnh07 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh07.csv")
mnh08 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh08.csv")
mnh09 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh09.csv")
mnh23 <- read_csv("~/DESCRIPTIVE_PRISMA/data/03-03-25/data/mnh23.csv")


mnh01_visit01 <- mnh01 %>%
  filter(TYPE_VISIT == 1) %>%
  select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT, US_FAC_SPFY_OHOLOC,
         US_GA_WKS_AGE_FTS1, US_GA_DAYS_AGE_FTS1) %>%
  mutate(
    GA_ENROLL = (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1) / 7,
    US_OHOSTDAT = dmy(US_OHOSTDAT),  # Convert to date format
    LMP = US_OHOSTDAT - GA_ENROLL * 7 )

mnh02<-mnh02%>% select(SCRNID,SCRN_OBSSTDAT,SCRN_FAC_SPFY_OBSLOC,CONSENT_IEORRES,MOMID,PREGID)%>%
             filter(CONSENT_IEORRES==1)%>%
  mutate(
    SCRN_OBSSTDAT = dmy(SCRN_OBSSTDAT))

#enrollment and consent GA
Consent_GA <- mnh01_visit01 %>%
  inner_join(mnh02, by = c("MOMID", "PREGID")) %>%
  mutate(
    GA_CONSENT = as.numeric((SCRN_OBSSTDAT-LMP)/7)  # Calculate GA difference
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
  )



#************************#
#Missing SD forms 
#************************#
mnh03<-mnh03%>%select(MOMID,PREGID,SD_OBSSTDAT)
Missing_SD<- mnh02 %>%
  left_join(mnh03, by = c("MOMID", "PREGID"))%>%
  mutate(
    SD_OBSSTDAT = dmy(SD_OBSSTDAT))%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
mutate(
  missing_mnh03 = ifelse(is.na(SD_OBSSTDAT), "Missing in mnh03", "Present"))%>%
  select(MOMID,PREGID,SCRN_FAC_SPFY_OBSLOC,SCRN_OBSSTDAT,SD_OBSSTDAT,CLOSE_DSSTDAT,
         missing_mnh03,Completion_Status)%>%
  rename(Site=SCRN_FAC_SPFY_OBSLOC,
         Enrollment_date=SCRN_OBSSTDAT,
         SD_DATE=SD_OBSSTDAT,
         Closeout_date=CLOSE_DSSTDAT)


#************************#
#filter ALL ENROLLMENT VISITS
#************************#
mnh01_ENROLL<-mnh01%>% filter(TYPE_VISIT ==1)%>%
  select(MOMID,PREGID,US_OHOSTDAT,US_FAC_SPFY_OHOLOC,TYPE_VISIT,US_GA_WKS_AGE_FTS1,US_GA_DAYS_AGE_FTS1)%>%
  mutate(US_OHOSTDAT = dmy(US_OHOSTDAT))

mnh04_ENROLL<-mnh04%>% filter(TYPE_VISIT ==1)%>%
  select(MOMID,PREGID,ANC_OBSSTDAT,ANC_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH04)
#%>%
 # mutate(ANC_OBSSTDAT = as.Date(ANC_OBSSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh05_ENROLL<-mnh05%>% filter(TYPE_VISIT ==1)%>%
  select(MOMID,PREGID,ANT_PEDAT,ANT_OTHR_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH05)
#%>%
 # mutate(VISIT_DATE = as.Date(ANT_PEDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh06_ENROLL<-mnh06%>% filter(TYPE_VISIT ==1)%>%
  select(MOMID,PREGID,DIAG_VSDAT,DIAG_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH06)#%>%
  #mutate(VISIT_DATE = as.Date(DIAG_VSDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh07_ENROLL<-mnh07%>% filter(TYPE_VISIT ==1)%>%
  select(MOMID,PREGID,MAT_SPEC_COLLECT_DAT,TYPE_VISIT,
         MAT_VISIT_MNH07)#%>%
  #mutate(VISIT_DATE = as.Date(MAT_SPEC_COLLECT_DAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh08_ENROLL<-mnh08%>% filter(TYPE_VISIT ==1)%>%
  select(MOMID,PREGID,LBSTDAT,TYPE_VISIT,
         MAT_VISIT_MNH08)#%>%
  #mutate(VISIT_DATE = as.Date(LBSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh09_delivery<-mnh09 %>%
  select(MOMID,PREGID,DELiV_DSSTDAT_INF1)%>%
  left_join(Consent_GA, by = c("MOMID", "PREGID"))%>%
  mutate(
  Dilvery_date = dmy(DELiV_DSSTDAT_INF1),
    GA_dilvery=(Dilvery_date-LMP)/7)%>%
  select(MOMID,PREGID,DELiV_DSSTDAT_INF1,GA_dilvery)

# Merge all datasets sequentially
ENROLLMENT <- mnh01_ENROLL %>%
  full_join(mnh04_ENROLL, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh05_ENROLL, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh06_ENROLL, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh07_ENROLL, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh08_ENROLL, by = c("MOMID", "PREGID", "TYPE_VISIT"))%>%
  select(MOMID,PREGID,TYPE_VISIT,US_OHOSTDAT,ANC_OBSSTDAT,MAT_VISIT_MNH04,ANT_PEDAT,MAT_VISIT_MNH05,
         DIAG_VSDAT,MAT_VISIT_MNH06,MAT_SPEC_COLLECT_DAT,MAT_VISIT_MNH07,LBSTDAT,MAT_VISIT_MNH08,
         DIAG_FAC_SPFY_OBSLOC)%>%
  rename(SITE=DIAG_FAC_SPFY_OBSLOC,
         MNH01_date=US_OHOSTDAT,
         MNH04_date=ANC_OBSSTDAT,
         MNH05_date=ANT_PEDAT,
         MNH06_date=DIAG_VSDAT,
         MNH07_date=MAT_SPEC_COLLECT_DAT,
         MNH08_date=LBSTDAT)%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
  left_join(Consent_GA %>% select(MOMID, PREGID,GA_ENROLL,SCRN_OBSSTDAT,GA_CONSENT), 
            by = c("MOMID", "PREGID")) %>%
  mutate(
    missing_mnh01 = ifelse(is.na(MNH01_date), "Missing in mnh01", "Present"),
    missing_mnh04 = ifelse(is.na(MNH04_date), "Missing in mnh04", "Present"),
    missing_mnh05 = ifelse(is.na(MNH05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(MNH06_date), "Missing in mnh06", "Present"),
    missing_mnh07 = ifelse(is.na(MNH07_date), "Missing in mnh07", "Present"),
    missing_mnh08 = ifelse(is.na(MNH08_date), "Missing in mnh08", "Present"),
    
  )
#************************#
#filter ALL ANC20 VISITS
#************************#
mnh01_ANC20<-mnh01%>% filter(TYPE_VISIT ==2)%>%
  select(MOMID,PREGID,US_OHOSTDAT,US_FAC_SPFY_OHOLOC,TYPE_VISIT,US_GA_WKS_AGE_FTS1,US_GA_DAYS_AGE_FTS1)%>%
  mutate(US_OHOSTDAT = dmy(US_OHOSTDAT))

mnh04_ANC20<-mnh04%>% filter(TYPE_VISIT ==2)%>%
select(MOMID,PREGID,ANC_OBSSTDAT,ANC_FAC_SPFY_OBSLOC,TYPE_VISIT,
       MAT_VISIT_MNH04)#%>%
  #mutate(VISIT_DATE = as.Date(ANC_OBSSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh05_ANC20<-mnh05%>% filter(TYPE_VISIT ==2)%>%
  select(MOMID,PREGID,ANT_PEDAT,ANT_OTHR_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH05)#%>%
  #mutate(VISIT_DATE = as.Date(ANT_PEDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh06_ANC20<-mnh06%>% filter(TYPE_VISIT ==2)%>%
  select(MOMID,PREGID,DIAG_VSDAT,DIAG_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH06)#%>%
  #mutate(VISIT_DATE = as.Date(DIAG_VSDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh07_ANC20<-mnh07%>% filter(TYPE_VISIT ==2)%>%
  select(MOMID,PREGID,MAT_SPEC_COLLECT_DAT,TYPE_VISIT,
         MAT_VISIT_MNH07)#%>%
  #mutate(VISIT_DATE = as.Date(MAT_SPEC_COLLECT_DAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh08_ANC20<-mnh08%>% filter(TYPE_VISIT ==2)%>%
  select(MOMID,PREGID,LBSTDAT,TYPE_VISIT,
         MAT_VISIT_MNH08)#%>%
  #mutate(VISIT_DATE = as.Date(LBSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

# Merge all datasets sequentially
ANC_20 <- mnh01_ANC20 %>%
  full_join(mnh04_ANC20, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh05_ANC20, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh06_ANC20, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh07_ANC20, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh08_ANC20, by = c("MOMID", "PREGID", "TYPE_VISIT"))%>%
  select(MOMID,PREGID,TYPE_VISIT,US_OHOSTDAT,ANC_OBSSTDAT,MAT_VISIT_MNH04,ANT_PEDAT,MAT_VISIT_MNH05,
         DIAG_VSDAT,MAT_VISIT_MNH06,MAT_SPEC_COLLECT_DAT,MAT_VISIT_MNH07,LBSTDAT,MAT_VISIT_MNH08,
         DIAG_FAC_SPFY_OBSLOC)%>%
  rename(SITE=DIAG_FAC_SPFY_OBSLOC,
         MNH01_date=US_OHOSTDAT,
         MNH04_date=ANC_OBSSTDAT,
         MNH05_date=ANT_PEDAT,
         MNH06_date=DIAG_VSDAT,
         MNH07_date=MAT_SPEC_COLLECT_DAT,
         MNH08_date=LBSTDAT)%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
  left_join(Consent_GA %>% select(MOMID, PREGID,GA_ENROLL,SCRN_OBSSTDAT,GA_CONSENT), 
            by = c("MOMID", "PREGID")) %>%
  mutate(
    missing_mnh01 = ifelse(is.na(MNH01_date), "Missing in mnh01", "Present"),
    missing_mnh04 = ifelse(is.na(MNH04_date), "Missing in mnh04", "Present"),
    missing_mnh05 = ifelse(is.na(MNH05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(MNH06_date), "Missing in mnh06", "Present"),
    missing_mnh07 = ifelse(is.na(MNH07_date), "Missing in mnh07", "Present"),
    missing_mnh08 = ifelse(is.na(MNH08_date), "Missing in mnh08", "Present"),
    
  )

  
#************************#
#filter ALL ANC28 VISITS
#************************#

mnh01_ANC28<-mnh01%>% filter(TYPE_VISIT ==3)%>%
  select(MOMID,PREGID,US_OHOSTDAT,US_FAC_SPFY_OHOLOC,TYPE_VISIT,US_GA_WKS_AGE_FTS1,US_GA_DAYS_AGE_FTS1)%>%
  mutate(US_OHOSTDAT = dmy(US_OHOSTDAT))

mnh04_ANC28<-mnh04%>% filter(TYPE_VISIT ==3)%>%
  select(MOMID,PREGID,ANC_OBSSTDAT,ANC_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH04)#%>%
  #mutate(VISIT_DATE = as.Date(ANC_OBSSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh05_ANC28<-mnh05%>% filter(TYPE_VISIT ==3)%>%
  select(MOMID,PREGID,ANT_PEDAT,ANT_OTHR_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH05)#%>%
  #mutate(VISIT_DATE = as.Date(ANT_PEDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh06_ANC28<-mnh06%>% filter(TYPE_VISIT ==3)%>%
  select(MOMID,PREGID,DIAG_VSDAT,DIAG_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH06)#%>%
  #mutate(VISIT_DATE = as.Date(DIAG_VSDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh07_ANC28<-mnh07%>% filter(TYPE_VISIT ==3)%>%
  select(MOMID,PREGID,MAT_SPEC_COLLECT_DAT,TYPE_VISIT,
         MAT_VISIT_MNH07)#%>%
 # mutate(VISIT_DATE = as.Date(MAT_SPEC_COLLECT_DAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh08_ANC28<-mnh08%>% filter(TYPE_VISIT ==3)%>%
  select(MOMID,PREGID,LBSTDAT,TYPE_VISIT,
         MAT_VISIT_MNH08)#%>%
 # mutate(VISIT_DATE = as.Date(LBSTDAT, format = "%d-%m-%Y"))  # Convert to Date type

# Merge all datasets sequentially
ANC_28 <- mnh01_ANC28 %>%
  full_join(mnh04_ANC28, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh05_ANC28, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh06_ANC28, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh07_ANC28, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh08_ANC28, by = c("MOMID", "PREGID", "TYPE_VISIT"))%>%
  select(MOMID,PREGID,TYPE_VISIT,US_OHOSTDAT,ANC_OBSSTDAT,MAT_VISIT_MNH04,ANT_PEDAT,MAT_VISIT_MNH05,
         DIAG_VSDAT,MAT_VISIT_MNH06,MAT_SPEC_COLLECT_DAT,MAT_VISIT_MNH07,LBSTDAT,MAT_VISIT_MNH08,
         DIAG_FAC_SPFY_OBSLOC)%>%
 rename(SITE=DIAG_FAC_SPFY_OBSLOC,
         MNH01_date=US_OHOSTDAT,
         MNH04_date=ANC_OBSSTDAT,
         MNH05_date=ANT_PEDAT,
         MNH06_date=DIAG_VSDAT,
         MNH07_date=MAT_SPEC_COLLECT_DAT,
         MNH08_date=LBSTDAT)%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
  mutate(
    missing_mnh01 = ifelse(is.na(MNH01_date), "Missing in mnh01", "Present"),
    missing_mnh04 = ifelse(is.na(MNH04_date), "Missing in mnh04", "Present"),
    missing_mnh05 = ifelse(is.na(MNH05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(MNH06_date), "Missing in mnh06", "Present"),
    missing_mnh07 = ifelse(is.na(MNH07_date), "Missing in mnh07", "Present"),
    missing_mnh08 = ifelse(is.na(MNH08_date), "Missing in mnh08", "Present"),
    
  )
#************************#
#filter ALL ANC32 VISITS
#************************#
mnh01_ANC32<-mnh01%>% filter(TYPE_VISIT ==4)%>%
  select(MOMID,PREGID,US_OHOSTDAT,US_FAC_SPFY_OHOLOC,TYPE_VISIT,US_GA_WKS_AGE_FTS1,US_GA_DAYS_AGE_FTS1)%>%
  mutate(US_OHOSTDAT = dmy(US_OHOSTDAT))

mnh04_ANC32<-mnh04%>% filter(TYPE_VISIT ==4)%>%
  select(MOMID,PREGID,ANC_OBSSTDAT,ANC_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH04)#%>%
  #mutate(VISIT_DATE = as.Date(ANC_OBSSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh05_ANC32<-mnh05%>% filter(TYPE_VISIT ==4)%>%
  select(MOMID,PREGID,ANT_PEDAT,ANT_OTHR_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH05)#%>%
 # mutate(VISIT_DATE = as.Date(ANT_PEDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh06_ANC32<-mnh06%>% filter(TYPE_VISIT ==4)%>%
  select(MOMID,PREGID,DIAG_VSDAT,DIAG_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH06)#%>%
  #mutate(VISIT_DATE = as.Date(DIAG_VSDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh07_ANC32<-mnh07%>% filter(TYPE_VISIT ==4)%>%
  select(MOMID,PREGID,MAT_SPEC_COLLECT_DAT,TYPE_VISIT,
         MAT_VISIT_MNH07)#%>%
  #mutate(VISIT_DATE = as.Date(MAT_SPEC_COLLECT_DAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh08_ANC32<-mnh08%>% filter(TYPE_VISIT ==4)%>%
  select(MOMID,PREGID,LBSTDAT,TYPE_VISIT,
         MAT_VISIT_MNH08)#%>%
  #mutate(VISIT_DATE = as.Date(LBSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

# Merge all datasets sequentially
ANC_32 <- mnh01_ANC32 %>%
  full_join(mnh04_ANC32, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh05_ANC32, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh06_ANC32, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh07_ANC32, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh08_ANC32, by = c("MOMID", "PREGID", "TYPE_VISIT"))%>%
  select(MOMID,PREGID,TYPE_VISIT,US_OHOSTDAT,ANC_OBSSTDAT,MAT_VISIT_MNH04,ANT_PEDAT,MAT_VISIT_MNH05,
         DIAG_VSDAT,MAT_VISIT_MNH06,MAT_SPEC_COLLECT_DAT,MAT_VISIT_MNH07,LBSTDAT,MAT_VISIT_MNH08,
         DIAG_FAC_SPFY_OBSLOC)%>%
  rename(SITE=DIAG_FAC_SPFY_OBSLOC,
         MNH01_date=US_OHOSTDAT,
         MNH04_date=ANC_OBSSTDAT,
         MNH05_date=ANT_PEDAT,
         MNH06_date=DIAG_VSDAT,
         MNH07_date=MAT_SPEC_COLLECT_DAT,
         MNH08_date=LBSTDAT)%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
  left_join(mnh09_delivery, by = c("MOMID", "PREGID"))%>%
  mutate(
    missing_mnh01 = ifelse(is.na(MNH01_date), "Missing in mnh01", "Present"),
    missing_mnh04 = ifelse(is.na(MNH04_date), "Missing in mnh04", "Present"),
    missing_mnh05 = ifelse(is.na(MNH05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(MNH06_date), "Missing in mnh06", "Present"),
    missing_mnh07 = ifelse(is.na(MNH07_date), "Missing in mnh07", "Present"),
    missing_mnh08 = ifelse(is.na(MNH08_date), "Missing in mnh08", "Present"),
    
  )

#************************#
#filter ALL ANC36 VISITS
#************************#
mnh01_ANC36<-mnh01%>% filter(TYPE_VISIT ==5)%>%
  select(MOMID,PREGID,US_OHOSTDAT,US_FAC_SPFY_OHOLOC,TYPE_VISIT,US_GA_WKS_AGE_FTS1,US_GA_DAYS_AGE_FTS1)%>%
  mutate(US_OHOSTDAT = dmy(US_OHOSTDAT))

mnh04_ANC36<-mnh04%>% filter(TYPE_VISIT ==5)%>%
  select(MOMID,PREGID,ANC_OBSSTDAT,ANC_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH04)#%>%
 # mutate(VISIT_DATE = as.Date(ANC_OBSSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh05_ANC36<-mnh05%>% filter(TYPE_VISIT ==5)%>%
  select(MOMID,PREGID,ANT_PEDAT,ANT_OTHR_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH05)#%>%
  #mutate(VISIT_DATE = as.Date(ANT_PEDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh06_ANC36<-mnh06%>% filter(TYPE_VISIT ==5)%>%
  select(MOMID,PREGID,DIAG_VSDAT,DIAG_FAC_SPFY_OBSLOC,TYPE_VISIT,
         MAT_VISIT_MNH06)#%>%
  #mutate(VISIT_DATE = as.Date(DIAG_VSDAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh07_ANC36<-mnh07%>% filter(TYPE_VISIT ==5)%>%
  select(MOMID,PREGID,MAT_SPEC_COLLECT_DAT,TYPE_VISIT,
         MAT_VISIT_MNH07)#%>%
 # mutate(VISIT_DATE = as.Date(MAT_SPEC_COLLECT_DAT, format = "%Y-%m-%d"))  # Convert to Date type

mnh08_ANC36<-mnh08%>% filter(TYPE_VISIT ==5)%>%
  select(MOMID,PREGID,LBSTDAT,TYPE_VISIT,
         MAT_VISIT_MNH08)#%>%
  #mutate(VISIT_DATE = as.Date(LBSTDAT, format = "%Y-%m-%d"))  # Convert to Date type

# Merge all datasets sequentially
ANC_36 <- mnh01_ANC36 %>%
  full_join(mnh04_ANC36, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh05_ANC36, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh06_ANC36, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh07_ANC36, by = c("MOMID", "PREGID", "TYPE_VISIT")) %>%
  full_join(mnh08_ANC36, by = c("MOMID", "PREGID", "TYPE_VISIT"))%>%
  select(MOMID,PREGID,TYPE_VISIT,US_OHOSTDAT,ANC_OBSSTDAT,MAT_VISIT_MNH04,ANT_PEDAT,MAT_VISIT_MNH05,
         DIAG_VSDAT,MAT_VISIT_MNH06,MAT_SPEC_COLLECT_DAT,MAT_VISIT_MNH07,LBSTDAT,MAT_VISIT_MNH08,
         DIAG_FAC_SPFY_OBSLOC)%>%
  rename(SITE=DIAG_FAC_SPFY_OBSLOC,
         MNH01_date=US_OHOSTDAT,
         MNH04_date=ANC_OBSSTDAT,
         MNH05_date=ANT_PEDAT,
         MNH06_date=DIAG_VSDAT,
         MNH07_date=MAT_SPEC_COLLECT_DAT,
         MNH08_date=LBSTDAT)%>%
  left_join(mnh23_filter,by =c("MOMID","PREGID"))%>%
  left_join(mnh09_delivery, by = c("MOMID", "PREGID"))%>%
  mutate(
    missing_mnh01 = ifelse(is.na(MNH01_date), "Missing in mnh01", "Present"),
    missing_mnh04 = ifelse(is.na(MNH04_date), "Missing in mnh04", "Present"),
    missing_mnh05 = ifelse(is.na(MNH05_date), "Missing in mnh05", "Present"),
    missing_mnh06 = ifelse(is.na(MNH06_date), "Missing in mnh06", "Present"),
    missing_mnh07 = ifelse(is.na(MNH07_date), "Missing in mnh07", "Present"),
    missing_mnh08 = ifelse(is.na(MNH08_date), "Missing in mnh08", "Present"),
    
  )



# Export multiple sheets in one Excel file
write.xlsx(list(
  ENROLLMENT = ENROLLMENT,
  SD=Missing_SD,
  ANC20 = ANC_20,
  ANC28 = ANC_28,
  ANC32 = ANC_32,
  ANC36 = ANC_36
), file = "ANC_forms.xlsx", overwrite = TRUE)
