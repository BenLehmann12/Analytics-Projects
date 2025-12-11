library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(sf)
library(maps)
library(lubridate)


#Read the Data

us_data <- read.csv("UnemploymentRate.csv", stringsAsFactors = FALSE)


#Work with the Dates
date_cols <- grep("^X?\\d+\\.\\d+\\.\\d+", names(us_data), value = TRUE)

names(us_data) <- gsub("^X", "", names(us_data))

metro_long <- us_data %>%
  pivot_longer(
    cols = matches("^\\d+\\.\\d+\\.\\d+"),
    names_to = "Date",
    values_to = "Unemployment_Rate"
  ) %>%
  filter(!is.na(Unemployment_Rate) & Unemployment_Rate != "") %>%
  mutate(
    Date = mdy(Date),
    Unemployment_Rate = as.numeric(Unemployment_Rate),
    Year = year(Date),
    Month = month(Date, label = TRUE),
    Quarter = quarter(Date)
  )
#Statistical Analysis
metro_summary <- metro_long %>%
  group_by(Series.ID, Region.Name, Region.Code) %>%
  summarise(
    Mean_Rate = mean(Unemployment_Rate, na.rm = TRUE),
    Median_Rate = median(Unemployment_Rate, na.rm = TRUE),
    SD_Rate = sd(Unemployment_Rate, na.rm = TRUE),
    Min_Rate = min(Unemployment_Rate, na.rm = TRUE),
    Max_Rate = max(Unemployment_Rate, na.rm = TRUE),
    Range_Rate = Max_Rate - Min_Rate,
    CV = (SD_Rate / Mean_Rate) * 100,  # Coefficient of variation
    N_Observations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Rate))


#National Trends
national_trends <- metro_long %>%
  group_by(Date, Year, Month) %>%
  summarise(
    Avg_National_Rate = mean(Unemployment_Rate, na.rm = TRUE),
    Median_National_Rate = median(Unemployment_Rate, na.rm = TRUE),
    N_Metro = n_distinct(Region.Name),
    .groups = "drop"
  )

metro_summary <- metro_summary %>%
  mutate(State = str_extract(Region.Name, "[A-Z]{2}$"))

#State Stats
state_summary <- metro_summary %>%
  filter(!is.na(State)) %>%
  group_by(State) %>%
  summarise(
    Avg_Unemployment = mean(Mean_Rate, na.rm = TRUE),
    N_Metro_Areas = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Unemployment))
