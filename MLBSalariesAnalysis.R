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

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- mlb_data
    
    if (input$year_filter != "All Years") {
      data <- data %>% filter(season == as.integer(input$year_filter))
    }
    
    if (input$league_filter != "All Leagues") {
      data <- data %>% filter(league == input$league_filter)
    }
    
    if (input$team_filter != "All Teams") {
      data <- data %>% filter(team == input$team_filter)
    }
    
    return(data)
  })
  
  # Value Boxes
  output$total_players <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Records",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$avg_salary <- renderValueBox({
    valueBox(
      paste0("$", format(round(mean(filtered_data()$salary, na.rm = TRUE)), big.mark = ",")),
      "Average Salary",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$max_salary <- renderValueBox({
    valueBox(
      paste0("$", format(max(filtered_data()$salary, na.rm = TRUE), big.mark = ",")),
      "Maximum Salary",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  # Salary Trends Over Years
  output$salary_trend <- renderPlotly({
    trend_data <- filtered_data() %>%
      group_by(season) %>%
      summarise(
        avg_salary = mean(salary, na.rm = TRUE),
        median_salary = median(salary, na.rm = TRUE),
        max_salary = max(salary, na.rm = TRUE)
      )
    
    plot_ly(trend_data, x = ~season) %>%
      add_lines(y = ~avg_salary, name = "Average Salary", line = list(color = "blue")) %>%
      add_lines(y = ~median_salary, name = "Median Salary", line = list(color = "green")) %>%
      add_lines(y = ~max_salary, name = "Max Salary", line = list(color = "red")) %>%
      layout(
        yaxis = list(title = "Salary ($)"),
        xaxis = list(title = "Year"),
        hovermode = "x unified"
      )
  })
  
  # League Comparison
  output$league_comparison <- renderPlotly({
    league_data <- filtered_data() %>%
      group_by(league) %>%
      summarise(avg_salary = mean(salary, na.rm = TRUE))
    
    plot_ly(league_data, x = ~league, y = ~avg_salary, type = "bar",
            marker = list(color = c("lightblue", "lightcoral"))) %>%
      layout(
        yaxis = list(title = "Average Salary ($)"),
        xaxis = list(title = "League")
      )
  })
  
  # Top Franchises
  output$top_franchises <- renderPlotly({
    franchise_data <- filtered_data() %>%
      group_by(franchise) %>%
      summarise(avg_salary = mean(salary, na.rm = TRUE)) %>%
      arrange(desc(avg_salary)) %>%
      head(10)
    
    plot_ly(franchise_data, y = ~reorder(franchise, avg_salary), x = ~avg_salary, 
            type = "bar", orientation = "h",
            marker = list(color = "steelblue")) %>%
      layout(
        xaxis = list(title = "Average Salary ($)"),
        yaxis = list(title = ""),
        margin = list(l = 200)
      )
  })
  
  # Highest Paid Player by Team & Year
  output$highest_paid_table <- renderDT({
    highest_paid <- filtered_data() %>%
      group_by(season, team) %>%
      arrange(desc(salary)) %>%
      slice(1) %>%
      ungroup() %>%
      select(season, team, player_name, salary, league) %>%
      arrange(desc(season), team)
    
    datatable(highest_paid, 
              options = list(pageLength = 20, scrollX = TRUE),
              colnames = c("Year", "Team", "Player", "Salary", "League")) %>%
      formatCurrency("salary", "$")
  })
  
  # Team Salary Distribution
  output$team_salary_dist <- renderPlotly({
    team_data <- filtered_data() %>%
      group_by(team) %>%
      summarise(
        total_salary = sum(salary, na.rm = TRUE),
        avg_salary = mean(salary, na.rm = TRUE)
      ) %>%
      arrange(desc(total_salary)) %>%
      head(20)
    
    plot_ly(team_data, x = ~reorder(team, total_salary), y = ~total_salary, 
            type = "bar",
            marker = list(color = "darkgreen")) %>%
      layout(
        xaxis = list(title = "Team", tickangle = -45),
        yaxis = list(title = "Total Salary ($)"),
        margin = list(b = 150)
      )
  })
  
  # Top Players Chart
  output$top_players_chart <- renderPlotly({
    top_players <- filtered_data() %>%
      arrange(desc(salary)) %>%
      head(20) %>%
      select(player_name, salary, team, season)
    
    plot_ly(top_players, y = ~reorder(paste(player_name, "-", season), salary), 
            x = ~salary, type = "bar", orientation = "h",
            marker = list(color = "crimson"),
            text = ~team, hoverinfo = "text+x") %>%
      layout(
        xaxis = list(title = "Salary ($)"),
        yaxis = list(title = ""),
        margin = list(l = 250)
      )
  })
  
  # Career Earnings Table
  output$career_earnings_table <- renderDT({
    career_data <- filtered_data() %>%
      group_by(player_name) %>%
      summarise(
        total_earnings = sum(salary, na.rm = TRUE),
        avg_salary = mean(salary, na.rm = TRUE),
        years_played = n(),
        teams = paste(unique(team), collapse = ", ")
      ) %>%
      arrange(desc(total_earnings)) %>%
      head(50)
    
    datatable(career_data, 
              options = list(pageLength = 15, scrollX = TRUE),
              colnames = c("Player", "Total Earnings", "Avg Salary", "Records", "Teams")) %>%
      formatCurrency(c("total_earnings", "avg_salary"), "$")
  })
  
  # Statistics by Year
  output$stats_by_year <- renderDT({
    stats_data <- filtered_data() %>%
      group_by(season) %>%
      summarise(
        players = n(),
        avg_salary = mean(salary, na.rm = TRUE),
        median_salary = median(salary, na.rm = TRUE),
        min_salary = min(salary, na.rm = TRUE),
        max_salary = max(salary, na.rm = TRUE),
        total_payroll = sum(salary, na.rm = TRUE)
      )
    
    datatable(stats_data, 
              options = list(pageLength = 15, scrollX = TRUE),
              colnames = c("Year", "Players", "Avg Salary", "Median", "Min", "Max", "Total Payroll")) %>%
      formatCurrency(c("avg_salary", "median_salary", "min_salary", "max_salary", "total_payroll"), "$")
  })
  
  # Salary Histogram
  output$salary_histogram <- renderPlotly({
    plot_ly(filtered_data(), x = ~salary, type = "histogram", nbinsx = 50,
            marker = list(color = "steelblue", line = list(color = "white", width = 1))) %>%
      layout(
        xaxis = list(title = "Salary ($)"),
        yaxis = list(title = "Frequency")
      )
  })
  
  # Salary Box Plot
  output$salary_boxplot <- renderPlotly({
    plot_ly(filtered_data(), y = ~salary, x = ~league, type = "box",
            color = ~league) %>%
      layout(
        yaxis = list(title = "Salary ($)"),
        xaxis = list(title = "League"),
        showlegend = FALSE
      )
  })
  
  # Full Data Table
  output$full_data_table <- renderDT({
    display_data <- filtered_data() %>%
      select(season, player_name, team, league, salary, height, weight, bats, throws)
    
    datatable(display_data, 
              options = list(pageLength = 25, scrollX = TRUE),
              colnames = c("Year", "Player", "Team", "League", "Salary", "Height", "Weight", "Bats", "Throws"),
              filter = "top") %>%
      formatCurrency("salary", "$")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
