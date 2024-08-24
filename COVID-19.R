# COVID-19 Data Analysis: South Korea vs. United States

# This project, implemented in R, analyzes COVID-19 data to compare the impact of the pandemic between South Korea and the United States. 
# The analysis focuses on death percentages, median age, hospital beds per thousand, and vaccination rates over time.

# Libraries Used
  # readr: For reading CSV files.
  # dplyr: For data manipulation.
  # ggplot2: For data visualization.
  # tidyverse: For a collection of R packages designed for data science.

# Install required libraries (uncomment and run if not already installed)
# install.packages(c("readr", "dplyr", "ggplot2", "tidyverse"))

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load COVID-19 dataset
covid_data <- read_csv("~/Documents/Portfolio/COVID-19/owid-covid-data.csv")

# Filter data for South Korea and United States
korea_us_data <- covid_data %>%
  filter(location %in% c("South Korea", "United States"))

# Ensure the data contains 'total_deaths' and 'population' columns, and filter NA values
korea_us_data <- korea_us_data %>%
  select(location, date, total_deaths, population, median_age, hospital_beds_per_thousand, total_vaccinations) %>%
  filter(!is.na(total_deaths), !is.na(population), !is.na(hospital_beds_per_thousand), !is.na(total_vaccinations))

# Calculate death percentage
korea_us_data <- korea_us_data %>%
  mutate(death_percentage = (total_deaths / population) * 100)

# Convert date to Date type
korea_us_data$date <- as.Date(korea_us_data$date)

# Plot the data: Death Percentage Comparison between South Korea and US
ggplot(korea_us_data, aes(x = date, y = death_percentage, color = location)) +
  geom_line(size = 1) +
  labs(title = "COVID-19 Death Percentage Comparison: South Korea vs. United States",
       x = "Date",
       y = "Death Percentage (%)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot the data: Median Age Over Time Comparison between South Korea and US
korea_us_data %>%
  ggplot(mapping = aes(
    x = date,
    y = median_age,
    color = location
  )) +
  geom_line() +
  theme_classic() +
  labs(title = "Median Age Comparison: South Korea vs. United States",
       x = "Date",
       y = "Median Age",
       color = "Location")

# Plot the data: Hospital Beds Over Time Comparison between South Korea and US
korea_us_data %>%
  ggplot(mapping = aes(
    x = date,
    y = hospital_beds_per_thousand,
    color = location
  )) +
  geom_line() +
  theme_classic() +
  labs(title = "Hospital Beds Comparison: South Korea vs. United States",
       x = "Date",
       y = "Hospital beds per thousand",
       color = "Location")

# Calculate vaccination rate (%) per capita
korea_us_data <- korea_us_data %>%
  mutate(vaccination_rate = (total_vaccinations / population) * 100)

# Plot the data: Total vaccinations Over Time Comparison between South Korea and US
korea_us_data %>%
  ggplot(mapping = aes(
    x = date,
    y = vaccination_rate,
    color = location
  )) +
  geom_line() +
  theme_classic() +
  labs(title = "COVID-19 Vaccination Rate (%) Comparison: South Korea vs. United States",
       x = "Date",
       y = "Vaccination Rate (%)",
       color = "Location")

# Insights

# Differences in Healthcare Systems and Pandemic Response:
  # Healthcare Infrastructure: South Korea has a robust healthcare system with universal coverage, 
  #     while the U.S. has a more fragmented system with varying levels of access to healthcare.
  # Pandemic Response: South Korea implemented aggressive testing, contact tracing, and isolation measures early on, 
  #     whereas the U.S. faced challenges with testing capacity and response uniformity.
  # Hospital Capacity and ICU Availability: South Korea may have better hospital capacity management, 
  #     whereas the U.S. faced periods of overwhelming demand, affecting death rates.
  # Public Health Interventions: South Korea’s strict public health interventions, such as mask mandates and quarantine measures, 
  #     may have contributed to fewer deaths relative to infections.

# Additional Factors
  # Population Density and Urbanization: South Korea's higher population density and urbanization might necessitate more hospital beds.
  # Healthcare Policies and Funding: Differences in policies, funding, and system organization can influence hospital bed availability.
  # Historical and Cultural Factors: South Korea's historical emphasis on public health and healthcare investments might lead to higher hospital bed availability.

