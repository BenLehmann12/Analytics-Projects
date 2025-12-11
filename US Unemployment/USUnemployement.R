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

#Trend Analysis
metro_trends <- metro_long %>%
  arrange(Region.Name, Date) %>%
  group_by(Region.Name) %>%
  mutate(
    Rate_Change = Unemployment_Rate - lag(Unemployment_Rate),
    Pct_Change = (Rate_Change / lag(Unemployment_Rate)) * 100,
    Trend = case_when(
      Rate_Change > 0.1 ~ "Increasing",
      Rate_Change < -0.1 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  ) %>%
  ungroup()

recent_trends <- metro_trends %>%
  filter(Date == max(Date)) %>%
  select(Region.Name, Unemployment_Rate, Rate_Change, Pct_Change, Trend) %>%
  arrange(desc(abs(Rate_Change)))

#Add Regression
metro_regression <- metro_long %>%
  group_by(Region.Name, Region.Code) %>%
  filter(n() >= 5) %>%  # Need at least 5 observations
  do({
    model <- lm(Unemployment_Rate ~ as.numeric(Date), data = .)
    data.frame(
      Slope = coef(model)[2],
      P_Value = summary(model)$coefficients[2, 4],
      R_Squared = summary(model)$r.squared
    )
  }) %>%
  ungroup() %>%
  mutate(
    Significant_Trend = ifelse(P_Value < 0.05, "Yes", "No"),
    Trend_Direction = case_when(
      Slope > 0 & Significant_Trend == "Yes" ~ "Increasing",
      Slope < 0 & Significant_Trend == "Yes" ~ "Decreasing",
      TRUE ~ "No Significant Trend"
    )
  )


#####-------Add Shiny Dashboard --------------- ####
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Metro Unemployment Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Time Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Geographic Map", tabName = "map", icon = icon("map")),
      menuItem("Metro Comparison", tabName = "compare", icon = icon("balance-scale")),
      menuItem("Statistical Analysis", tabName = "stats", icon = icon("calculator")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("avg_rate_box"),
          valueBoxOutput("highest_metro_box"),
          valueBoxOutput("lowest_metro_box")
        ),
        fluidRow(
          box(
            title = "National Unemployment Trend",
            width = 12,
            plotlyOutput("national_trend_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "Distribution of Unemployment Rates",
            width = 6,
            plotlyOutput("distribution_plot", height = 350)
          ),
          box(
            title = "Top 15 Metros by Average Rate",
            width = 6,
            plotlyOutput("top_metros_plot", height = 350)
          )
        )
      ),
      
      # Time Trends Tab
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Select Metro Areas",
            width = 12,
            selectizeInput(
              "selected_metros",
              "Choose metros to compare:",
              choices = unique(metro_long$Region.Name),
              multiple = TRUE,
              selected = head(unique(metro_long$Region.Name), 5),
              options = list(maxItems = 10)
            )
          )
        ),
        fluidRow(
          box(
            title = "Unemployment Rate Trends",
            width = 12,
            plotlyOutput("metro_trends_plot", height = 500)
          )
        ),
        fluidRow(
          box(
            title = "Month-over-Month Change",
            width = 12,
            plotlyOutput("mom_change_plot", height = 400)
          )
        )
      ),
      
      # Geographic Map Tab
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Select Date",
            width = 12,
            sliderInput(
              "map_date",
              "Choose date:",
              min = min(metro_long$Date),
              max = max(metro_long$Date),
              value = max(metro_long$Date),
              timeFormat = "%b %Y",
              animate = TRUE
            )
          )
        ),
        fluidRow(
          box(
            title = "Unemployment Rate by State (Average)",
            width = 12,
            plotlyOutput("state_map", height = 600)
          )
        )
      ),
      
      # Metro Comparison Tab
      tabItem(
        tabName = "compare",
        fluidRow(
          box(
            title = "Select Metros to Compare",
            width = 12,
            selectizeInput(
              "compare_metros",
              "Choose metros:",
              choices = unique(metro_long$Region.Name),
              multiple = TRUE,
              selected = c("New York-Newark-Jersey City, NY-NJ-PA", 
                           "Los Angeles-Long Beach-Anaheim, CA",
                           "Chicago-Naperville-Elgin, IL-IN-WI"),
              options = list(maxItems = 6)
            )
          )
        ),
        fluidRow(
          box(
            title = "Box Plot Comparison",
            width = 6,
            plotlyOutput("compare_box_plot", height = 400)
          ),
          box(
            title = "Summary Statistics",
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Time Series Comparison",
            width = 12,
            plotlyOutput("compare_time_plot", height = 400)
          )
        )
      ),
      
      # Statistical Analysis Tab
      tabItem(
        tabName = "stats",
        fluidRow(
          box(
            title = "Metros with Significant Trends",
            width = 6,
            plotlyOutput("trend_direction_plot", height = 300)
          ),
          box(
            title = "Volatility Analysis (Coefficient of Variation)",
            width = 6,
            plotlyOutput("volatility_plot", height = 300)
          )
        ),
        fluidRow(
          box(
            title = "State-Level Summary",
            width = 12,
            plotlyOutput("state_summary_plot", height = 500)
          )
        ),
        fluidRow(
          box(
            title = "Correlation Analysis (Latest Month)",
            width = 12,
            p("Correlation between unemployment rate and metro characteristics"),
            verbatimTextOutput("correlation_analysis")
          )
        )
      ),
      
      # Data Table Tab
      tabItem(
        tabName = "table",
        fluidRow(
          box(
            title = "Metro Summary Statistics",
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Recent Trends",
            width = 12
          )
        )
      )
    )
  )
)
