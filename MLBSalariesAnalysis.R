library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

# Read the CSV data
mlb_data <- read.csv("mlb_salaries.csv", stringsAsFactors = FALSE)

mlb_data$salary <- as.numeric(mlb_data$salary)
mlb_data$season <- as.integer(mlb_data$season)

ui <- dashboardPage(
  dashboardHeader(title = "MLB Salary Analytics (1985-2016)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Team Analysis", tabName = "team_analysis", icon = icon("users")),
      menuItem("Player Analysis", tabName = "player_analysis", icon = icon("user")),
      menuItem("Statistical Summary", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    ),
    
    hr(),
    
    selectInput("year_filter", "Select Year:",
                choices = c("All Years", sort(unique(mlb_data$season))),
                selected = "All Years"),
    
    selectInput("league_filter", "Select League:",
                choices = c("All Leagues", "AL", "NL"),
                selected = "All Leagues"),
    
    selectInput("team_filter", "Select Team:",
                choices = c("All Teams", sort(unique(mlb_data$team))),
                selected = "All Teams")
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_players"),
                valueBoxOutput("avg_salary"),
                valueBoxOutput("max_salary")
              ),
              fluidRow(
                box(
                  title = "Salary Trends Over Years",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("salary_trend", height = 400)
                )
              ),
              fluidRow(
                box(
                  title = "League Comparison",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("league_comparison")
                ),
                box(
                  title = "Top 10 Franchises by Avg Salary",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("top_franchises")
                )
              )
      ),
      
      # Team Analysis Tab
      tabItem(tabName = "team_analysis",
              fluidRow(
                box(
                  title = "Highest Paid Player by Team & Year",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("highest_paid_table")
                )
              ),
              fluidRow(
                box(
                  title = "Team Salary Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("team_salary_dist", height = 500)
                )
              )
      ),
      
      # Player Analysis Tab
      tabItem(tabName = "player_analysis",
              fluidRow(
                box(
                  title = "Top 20 Highest Paid Players",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("top_players_chart", height = 500)
                )
              ),
              fluidRow(
                box(
                  title = "Player Career Earnings",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("career_earnings_table")
                )
              )
      ),
      
      # Statistical Summary Tab
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  title = "Salary Statistics by Year",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("stats_by_year")
                )
              ),
              fluidRow(
                box(
                  title = "Salary Distribution",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("salary_histogram")
                ),
                box(
                  title = "Salary Box Plot by League",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("salary_boxplot")
                )
              )
      ),
      
      # Data Table Tab
      tabItem(tabName = "data_table",
              fluidRow(
                box(
                  title = "Complete MLB Salary Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("full_data_table")
                )
              )
      )
    )
  )
)

