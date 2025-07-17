# Load necessary libraries
library(readr)
library(dplyr)

# Load the dataset
nfhs <- read_csv("datafile.csv")

# Check structure and preview
glimpse(nfhs)
head(nfhs, 10)
library(tidyverse)
library(janitor)
nfhs_clean <- nfhs %>% 
  clean_names()

colnames(nfhs_clean)[1:5]  # Show first 5 column names
nfhs_clean <- nfhs %>%
  clean_names() %>%
  filter(area == "Total") %>%
  mutate(states = str_replace(states_u_ts, "†", ""))  # fixed name here



colnames(nfhs_clean)[grepl("female.*school", colnames(nfhs_clean), ignore.case = TRUE)]
nfhs_clean <- nfhs_clean %>%
  mutate(female_school_attendance = as.numeric(female_population_age_6_years_and_above_who_ever_attended_school_percent))

nfhs_total <- nfhs_clean %>% 
  filter(area == "Total")




library(ggplot2)

# Create a new column for highlighting India
nfhs_total <- nfhs_total %>%
  mutate(highlight_india = ifelse(states_u_ts == "India", "India", "Other"))

# Plot with conditional fill
ggplot(nfhs_total, aes(x = reorder(states_u_ts, female_school_attendance), 
                       y = female_school_attendance, 
                       fill = highlight_india)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Female School Attendance by State/UT (NFHS-5)",
       x = "State/UT",
       y = "Percentage of Females (6+ years) Who Attended School") +
  scale_fill_manual(values = c("India" = "#d73027", "Other" = "#2c7fb8")) +
  theme_minimal() +
  theme(legend.position = "none")  # optional: remove legend




colnames(nfhs_clean)[grepl("sex.*ratio", colnames(nfhs_clean), ignore.case = TRUE)]

nfhs_clean <- nfhs_clean %>%
  mutate(sex_ratio = as.numeric(sex_ratio_of_the_total_population_females_per_1_000_males))

# Create a new column to identify if it's India
nfhs_total <- nfhs_clean %>%
  filter(area == "Total") %>%
  mutate(highlight_india = ifelse(states_u_ts == "India", "India", "Other"))

# Plot with conditional fill
ggplot(nfhs_total, aes(x = reorder(states_u_ts, sex_ratio), y = sex_ratio, fill = highlight_india)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sex Ratio (Females per 1000 Males) by State/UT",
       x = "State/UT",
       y = "Sex Ratio") +
  scale_fill_manual(values = c("India" = "#d73027", "Other" = "#66c2a5")) +  # Red for India
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend (optional)

