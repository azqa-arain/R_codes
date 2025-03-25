# Load necessary libraries
library(readr)
library(dplyr)

# Get the current date in the format "dd-mm-yy"
#current_date <- format(Sys.Date(), "%d-%m-%y")

# Set the base path with the current date
base_path <- paste0("~/DESCRIPTIVE_PRISMA/data/15-11-24/data")

# List of CSV files you want to import
files <- c("mnh04.csv","mnh05.csv", "mnh06.csv","mnh07.csv","mnh08.csv", "mnh09.csv", "mnh10.csv", 
           "mnh11.csv","mnh12.csv","mnh13.csv", "mnh14.csv","mnh15.csv", "mnh19.csv", "mnh20.csv",
           "mnh25.csv","mnh26.csv","mnh23.csv","mnh24.csv")

# Loop to read files
for (file in files) {
  file_path <- file.path(base_path, file)  # Construct full file path
  assign(file, read_csv(file_path))  # Assign data to the variable name
}

# set the Path to export the Data 
getwd()
setwd("~/DESCRIPTIVE_PRISMA/data/15-11-24/output")


# Convert CLOSE_OUT_DATE to Date format if it's not already in Date format
mnh23.csv$CLOSE_DSSTDAT <- lubridate::dmy(mnh23.csv$CLOSE_DSSTDAT)


# Filter mnh04 based on MOMID and CLOSE_DSSTDAT from mnh23.csv
mnh04<- mnh04.csv %>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  # Add condition to filter ANC_OBSSTDAT based on CLOSE_DSSTDAT for the corresponding PREGID
  filter(ANC_OBSSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID, ANC_OBSSTDAT, ANC_FAC_SPFY_OBSLOC, TYPE_VISIT,CLOSE_DSSTDAT)


mnh05<- mnh05.csv %>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(ANT_PEDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID, ANT_PEDAT,ANT_FAC_SPFY_OBSLOC, TYPE_VISIT,CLOSE_DSSTDAT)

mnh06 <- mnh06.csv %>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(DIAG_VSDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,DIAG_VSDAT,DIAG_FAC_SPFY_OBSLOC, TYPE_VISIT,CLOSE_DSSTDAT)

mnh07 <- mnh07.csv %>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(MAT_SPEC_COLLECT_DAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,MAT_SPEC_COLLECT_DAT,MAT_SPEC_FAC_SPFY, TYPE_VISIT,CLOSE_DSSTDAT)

mnh08<- mnh08.csv %>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(LBSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,LBSTDAT, TYPE_VISIT,CLOSE_DSSTDAT)

mnh09<- mnh09.csv %>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(FORMCOMPLDAT_MNH09 > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,FORMCOMPLDAT_MNH09,DELiV_DSSTDAT_INF1,CLOSE_DSSTDAT)

mnh10<-mnh10.csv%>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(VISIT_OBSSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,VISIT_OBSSTDAT,CLOSE_DSSTDAT)

mnh11<-mnh11.csv%>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(VISIT_OBSSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,INFANTID,VISIT_OBSSTDAT,CLOSE_DSSTDAT)

mnh12<-mnh12.csv%>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(VISIT_OBSSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,VISIT_OBSSTDAT,TYPE_VISIT,CLOSE_DSSTDAT)

mnh19<-mnh19.csv%>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(OBSSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,OBSSTDAT,CLOSE_DSSTDAT)

mnh25<-mnh25.csv%>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(OBSSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,OBSSTDAT,TYPE_VISIT,CLOSE_DSSTDAT)

mnh26<-mnh26.csv%>%
  filter(PREGID %in% mnh23.csv$PREGID) %>%
  filter(FTGE_OBSTDAT > mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh23.csv$CLOSE_DSSTDAT[match(PREGID, mnh23.csv$PREGID)]) %>%
  select(MOMID, PREGID,FTGE_OBSTDAT,TYPE_VISIT,CLOSE_DSSTDAT)


# Remove the old workbook object
rm(wb) 
#create workbook again
wb <- createWorkbook()

# Add data frames to the workbook as sheets (if not empty)
if (nrow(mnh04) > 0) {
  addWorksheet(wb, "mnh04")
  writeData(wb, "mnh04", mnh04)
}
if (nrow(mnh05) > 0) {
  addWorksheet(wb, "mnh05")
  writeData(wb, "mnh05", mnh05)
}

if (nrow(mnh06) > 0) {
  addWorksheet(wb, "mnh06")
  writeData(wb, "mnh06", mnh06)
}
if (nrow(mnh07) > 0) {
  addWorksheet(wb, "mnh07")
  writeData(wb, "mnh07", mnh07)
}
if (nrow(mnh08) > 0) {
  addWorksheet(wb, "mnh08")
  writeData(wb, "mnh08", mnh08)
}
if (nrow(mnh09) > 0) {
  addWorksheet(wb, "mnh09")
  writeData(wb, "mnh09", mnh09)
}
if (nrow(mnh10) > 0) {
  addWorksheet(wb, "mnh10")
  writeData(wb, "mnh10", mnh10)
}
if (nrow(mnh11) > 0) {
  addWorksheet(wb, "mnh11")
  writeData(wb, "mnh11", mnh11)
}
if (nrow(mnh12) > 0) {
  addWorksheet(wb, "mnh12")
  writeData(wb, "mnh12", mnh12)
}
if (nrow(mnh19) > 0) {
  addWorksheet(wb, "mnh19")
  writeData(wb, "mnh19", mnh19)
}
if (nrow(mnh25) > 0) {
  addWorksheet(wb, "mnh25")
  writeData(wb, "mnh25", mnh25)
}
if (nrow(mnh26) > 0) {
  addWorksheet(wb, "mnh26")
  writeData(wb, "mnh26", mnh26)
}
# Save the workbook
saveWorkbook(wb, "MNH23_ERROR7_IDS.xlsx", overwrite = TRUE)
message("Data exported to multiple sheets successfully.")


##----------MNH24------------##

mnh11_IDS<-mnh11.csv%>%
  filter(INFANTID %in% mnh24.csv$INFANTID) %>%
  filter(VISIT_OBSSTDAT > mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  select(MOMID, PREGID,INFANTID,VISIT_OBSSTDAT,CLOSE_DSSTDAT)

mnh13_IDS<-mnh13.csv%>%
  filter(INFANTID %in% mnh24.csv$INFANTID) %>%
  filter(VISIT_OBSSTDAT > mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  select(MOMID, PREGID,INFANTID,VISIT_OBSSTDAT,TYPE_VISIT,CLOSE_DSSTDAT)

mnh14_IDS<-mnh14.csv%>%
  filter(INFANTID %in% mnh24.csv$INFANTID) %>%
  filter(VISIT_OBSSTDAT > mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  select(MOMID, PREGID,INFANTID,VISIT_OBSSTDAT,TYPE_VISIT,CLOSE_DSSTDAT)

mnh15_IDS<-mnh15.csv%>%
  filter(INFANTID %in% mnh24.csv$INFANTID) %>%
  filter(OBSSTDAT > mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  mutate(CLOSE_DSSTDAT = mnh24.csv$CLOSE_DSSTDAT[match(INFANTID, mnh24.csv$INFANTID)]) %>%
  select(MOMID, PREGID,INFANTID,OBSSTDAT,TYPE_VISIT,CLOSE_DSSTDAT)
# Remove the old workbook object
rm(wb) 
#create workbook again
wb <- createWorkbook()

# Add data frames to the workbook as sheets (if not empty)
if (nrow(mnh11_IDS) > 0) {
  addWorksheet(wb, "mnh11")
  writeData(wb, "mnh11", mnh11_IDS)
}
if (nrow(mnh13_IDS) > 0) {
  addWorksheet(wb, "mnh13")
  writeData(wb, "mnh13", mnh13_IDS)
}

if (nrow(mnh14_IDS) > 0) {
  addWorksheet(wb, "mnh14")
  writeData(wb, "mnh14", mnh14_IDS)
}

if (nrow(mnh15_IDS) > 0) {
  addWorksheet(wb, "mnh15")
  writeData(wb, "mnh15", mnh15_IDS)
}

# Save the workbook
saveWorkbook(wb, "MNH24_ERROR7_IDS.xlsx", overwrite = TRUE)
message("Data exported to multiple sheets successfully.")
