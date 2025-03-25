#SGA Codes
#Azqa MAzhar

#install library and packages
rm(list = ls())
install.packages("remotes")
library(remotes)
remotes::install_github("ki-tools/growthstandards")
library(growthstandards)
library(readr)
library(dplyr)
library(lubridate)

output_path <- "~/DESCRIPTIVE_PRISMA/data/07-03-25/output/SGA.csv"
mnh01 <- read_csv("~/DESCRIPTIVE_PRISMA/data/07-03-25/data/mnh01.csv")
mnh09<- read_csv("~/DESCRIPTIVE_PRISMA/data/07-03-25/data/mnh09.csv")
mnh11<- read_csv("~/DESCRIPTIVE_PRISMA/data/07-03-25/data/mnh11.csv")


#Filter just TYPE-VISIT=1 for GA calculation
mnh01 <- mnh01 %>% filter(TYPE_VISIT == 1)
mnh01<- mnh01 %>% select(MOMID,PREGID,US_OHOSTDAT,TYPE_VISIT,
                         US_GA_WKS_AGE_FTS1,US_GA_DAYS_AGE_FTS1)
mnh01 <- mnh01 %>% rename(MOMID_mnh01 = MOMID)
mnh01<-mnh01 %>% mutate(GA_Days= (US_GA_WKS_AGE_FTS1*7)+US_GA_DAYS_AGE_FTS1)
mnh01 <- mnh01 %>% mutate(US_OHOSTDAT = dmy(US_OHOSTDAT))
#LMP Based on 1st Ultrasound
mnh01<-mnh01 %>% mutate(LMP=US_OHOSTDAT-GA_Days)
#Select Data from MNH09 for GA at delivery Calculation
mnh09 <- mnh09 %>% rename(MOMID_mnh09 = MOMID)
mnh09<- mnh09 %>% select(MOMID_mnh09,PREGID,DELiV_DSSTDAT_INF1,INFANTID_INF1,
                         SEX_INF1,INFANTID_INF2,SEX_INF2,INFANTID_INF3,
                         SEX_INF3,INFANTID_INF4,SEX_INF4)

mnh09 <- mnh09 %>% mutate(DELiV_DSSTDAT_INF1 = dmy(DELiV_DSSTDAT_INF1)) #format Date

#merging MNH01 with MNH09
merged_data <- mnh01 %>% right_join(mnh09, by = "PREGID")
merged_data<-merged_data %>%mutate(GA_dilvery=DELiV_DSSTDAT_INF1-LMP)
merged_data<-merged_data %>%mutate(GA_dilvery_weeks=GA_dilvery/7)

GA_dilvery<-merged_data%>% select(MOMID_mnh09,PREGID,GA_dilvery,GA_dilvery_weeks)
mnh11<-mnh11%>% filter(INF_DSTERM == 1)
mnh11<-mnh11%>% select(MOMID,PREGID,VISIT_OBSSTDAT,INFANTID,INF_VISIT_72HR_MNH11,SEX_INF,
                       BW_FAORRES_REPORT,BW_FAORRES)
mnh11 <- mnh11%>% rename(MOMID_mnh11 = MOMID)
mnh11 <- mnh11 %>%
  mutate(
    Birthweight = case_when(
      # If PRISMA BW is available and taken <72 hours → Use PRISMA BW
      BW_FAORRES != -5 & !is.na(INF_VISIT_72HR_MNH11) &INF_VISIT_72HR_MNH11 < 72 ~ BW_FAORRES,
      # If PRISMA BW is available but hours are missing → Use PRISMA BW
      BW_FAORRES != -5 & is.na(INF_VISIT_72HR_MNH11) ~ INF_VISIT_72HR_MNH11,
      # If PRISMA BW is available but hours ≥72 → Use Facility BW
      BW_FAORRES != -5 & !is.na(INF_VISIT_72HR_MNH11) &
        INF_VISIT_72HR_MNH11 >= 72 ~ BW_FAORRES_REPORT,
      # If PRISMA BW is unavailable → Use Facility BW
      BW_FAORRES == -5 ~ BW_FAORRES_REPORT,
      # Default to NA if none of the above apply
      TRUE ~ NA_real_
    )
  )
mnh11 <- mnh11 %>%
  mutate(Birthweight_kg = Birthweight / 1000)
# Recode sex variable
mnh11 <- mnh11 %>%
  mutate(SEX_INF = case_when(SEX_INF == 1 ~ "Male",
                             SEX_INF == 2 ~ "Female",TRUE ~ NA_character_))

merged_GA<- mnh11 %>% right_join(GA_dilvery, by = "PREGID")

merged_GA <-merged_GA %>% rename(gestational_age = GA_dilvery,
                                  MOMID= MOMID_mnh11,
                                  birthweight=Birthweight_kg,sex=SEX_INF)

merged_GA <- merged_GA%>% select(MOMID,INFANTID,INF_VISIT_72HR_MNH11,
                                 birthweight,gestational_age,sex)
merged_GA <- merged_GA %>% 
  filter(!is.na(sex))

merged_GA <- merged_GA %>%
  mutate(
    SGA_CENTILE = case_when(
      is.na(gestational_age) ~ -5,   # Missing gestational age
      TRUE ~ suppressWarnings(round(igb_wtkg2centile(gestational_age, 
                                                     birthweight, sex), 3))
    )
  )
merged_GA <- merged_GA %>%
  mutate(
    SGA_CAT = case_when(
     SGA_CENTILE >= 0 & SGA_CENTILE < 3 ~ 11,   # SGA_CENTILE < 3rd
     SGA_CENTILE >= 3 & SGA_CENTILE < 10 ~ 12,  # SGA_CENTILE < 10th
     SGA_CENTILE >= 10 & SGA_CENTILE < 90 ~ 13, # AGA 10th to <90th 
     SGA_CENTILE >= 90 ~ 14,                   # LGA >= 90th
    TRUE ~ 55                                 # Catch-all for unexpected values
  )
  )

# Export the dataset to a CSV file
write.csv(merged_GA, file = output_path, row.names = FALSE)
