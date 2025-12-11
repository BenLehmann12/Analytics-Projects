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
