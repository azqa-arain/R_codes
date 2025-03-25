library(dplyr)
ultrasound_data <- read.csv("C:/Users/azqa.mazhar/OneDrive - Aga Khan University (1)/Documents/DESCRIPTIVE_PRISMA/data/01-03-24/data/mnh01.csv", header = TRUE)
delivery_data <- read.csv("C:/Users/azqa.mazhar/OneDrive - Aga Khan University (1)/Documents/DESCRIPTIVE_PRISMA/data/01-03-24/data/mnh09.csv", header = TRUE)

ultrasound_data <- ultrasound_data %>%
  filter(TYPE_VISIT == 1)

# Step 2: Convert date columns to consistent yyyy-mm-dd format
ultrasound_data <- ultrasound_data %>%
  mutate(
    US_OHOSTDAT = as.Date(US_OHOSTDAT, format = "%d/%m/%Y")
  )

delivery_data <- delivery_data %>%
  mutate(
    DELiV_DSSTDAT_INF1 = as.Date(DELiV_DSSTDAT_INF1, format = "%d-%m-%Y")
  )

# Step 3: Left join with delivery_data
GA_data <- delivery_data %>%
  left_join(ultrasound_data, by = c("MOMID", "PREGID")) %>%
  mutate(
    Gestational_Age_Days = US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1,
    LMP_Date = US_OHOSTDAT - Gestational_Age_Days,
    GA = as.numeric(difftime(DELiV_DSSTDAT_INF1, LMP_Date, units = "days")) / 7
  ) %>%
  select(MOMID, PREGID, US_OHOSTDAT, DELiV_DSSTDAT_INF1, LMP_Date, GA)


filtered_GA_data <- GA_data %>%
  filter(GA < 37)

# Calculate the percentage
percentage <- nrow(filtered_GA_data) / nrow(GA_data) * 100

# Print the percentage
cat("Percentage of Preterm:", percentage, "%\n")