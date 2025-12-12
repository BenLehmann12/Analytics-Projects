library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(sf)
library(DT)
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


print(summary(national_trends$Avg_National_Rate))

print(head(metro_summary %>% select(Region.Name, Mean_Rate, SD_Rate), 10))

print(tail(metro_summary %>% select(Region.Name, Mean_Rate, SD_Rate), 10))

print(table(metro_regression$Trend_Direction))





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

server <- function(input, output, session) {
  
  # Value Boxes
  output$avg_rate_box <- renderValueBox({
    latest_avg <- national_trends %>%
      filter(Date == max(Date)) %>%
      pull(Avg_National_Rate)
    
    valueBox(
      sprintf("%.2f%%", latest_avg),
      "National Average Rate",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$highest_metro_box <- renderValueBox({
    highest <- metro_summary %>%
      slice_max(Mean_Rate, n = 1)
    
    valueBox(
      sprintf("%.2f%%", highest$Mean_Rate),
      paste("Highest:", str_trunc(highest$Region.Name, 30)),
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$lowest_metro_box <- renderValueBox({
    lowest <- metro_summary %>%
      slice_min(Mean_Rate, n = 1)
    
    valueBox(
      sprintf("%.2f%%", lowest$Mean_Rate),
      paste("Lowest:", str_trunc(lowest$Region.Name, 30)),
      icon = icon("arrow-down"),
      color = "green"
    )
  })
  
  # National Trend Plot
  output$national_trend_plot <- renderPlotly({
    p <- ggplot(national_trends, aes(x = Date, y = Avg_National_Rate)) +
      geom_line(color = "#2c3e50", size = 1.2) +
      geom_point(color = "#3498db", size = 2) +
      geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "#e74c3c") +
      labs(
        title = "National Average Unemployment Rate Over Time",
        x = "Date",
        y = "Average Unemployment Rate (%)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Distribution Plot
  output$distribution_plot <- renderPlotly({
    latest_data <- metro_long %>%
      filter(Date == max(Date))
    
    p <- ggplot(latest_data, aes(x = Unemployment_Rate)) +
      geom_histogram(bins = 30, fill = "#3498db", alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean(Unemployment_Rate)), 
                 color = "#e74c3c", linetype = "dashed", size = 1) +
      labs(
        title = "Distribution of Current Unemployment Rates",
        x = "Unemployment Rate (%)",
        y = "Number of Metros"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Top Metros Plot
  output$top_metros_plot <- renderPlotly({
    top_15 <- metro_summary %>%
      slice_max(Mean_Rate, n = 15) %>%
      mutate(Region.Name = str_trunc(Region.Name, 30))
    
    p <- ggplot(top_15, aes(x = reorder(Region.Name, Mean_Rate), y = Mean_Rate)) +
      geom_col(fill = "#e74c3c", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Top 15 Metros by Average Unemployment Rate",
        x = "",
        y = "Average Unemployment Rate (%)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Metro Trends Plot
  output$metro_trends_plot <- renderPlotly({
    req(input$selected_metros)
    
    filtered_data <- metro_long %>%
      filter(Region.Name %in% input$selected_metros)
    
    p <- ggplot(filtered_data, aes(x = Date, y = Unemployment_Rate, 
                                   color = Region.Name, group = Region.Name)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Unemployment Rate Trends by Metro",
        x = "Date",
        y = "Unemployment Rate (%)",
        color = "Metro Area"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Month-over-Month Change Plot
  output$mom_change_plot <- renderPlotly({
    req(input$selected_metros)
    
    filtered_data <- metro_trends %>%
      filter(Region.Name %in% input$selected_metros, !is.na(Rate_Change))
    
    p <- ggplot(filtered_data, aes(x = Date, y = Rate_Change, 
                                   color = Region.Name, group = Region.Name)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Month-over-Month Rate Change",
        x = "Date",
        y = "Change in Unemployment Rate (pp)",
        color = "Metro Area"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # State Map
  output$state_map <- renderPlotly({
    map_data <- metro_long %>%
      mutate(State = str_extract(Region.Name, "[A-Z]{2}$")) %>%
      filter(!is.na(State), Date == input$map_date) %>%
      group_by(State) %>%
      summarise(Avg_Rate = mean(Unemployment_Rate, na.rm = TRUE), .groups = "drop")
    
    # Create choropleth map
    plot_geo(map_data, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Avg_Rate,
        locations = ~State,
        color = ~Avg_Rate,
        colors = "YlOrRd",
        text = ~paste(State, ":", round(Avg_Rate, 2), "%"),
        hoverinfo = "text"
      ) %>%
      layout(
        geo = list(scope = 'usa'),
        title = paste("Average Unemployment Rate by State -", format(input$map_date, "%B %Y"))
      )
  })
  
  # Comparison Plots
  output$compare_box_plot <- renderPlotly({
    req(input$compare_metros)
    
    compare_data <- metro_long %>%
      filter(Region.Name %in% input$compare_metros) %>%
      mutate(Region.Name = str_trunc(Region.Name, 25))
    
    p <- ggplot(compare_data, aes(x = Region.Name, y = Unemployment_Rate, fill = Region.Name)) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      labs(
        title = "Unemployment Rate Distribution Comparison",
        x = "",
        y = "Unemployment Rate (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$compare_stats_table <- renderDT({
    req(input$compare_metros)
    
    metro_summary %>%
      filter(Region.Name %in% input$compare_metros) %>%
      select(Region.Name, Mean_Rate, SD_Rate, Min_Rate, Max_Rate, Range_Rate) %>%
      rename(
        "Metro" = Region.Name,
        "Mean" = Mean_Rate,
        "Std Dev" = SD_Rate,
        "Min" = Min_Rate,
        "Max" = Max_Rate,
        "Range" = Range_Rate
      ) %>%
      datatable(options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatRound(columns = 2:6, digits = 2)
  })
  
  output$compare_time_plot <- renderPlotly({
    req(input$compare_metros)
    
    compare_data <- metro_long %>%
      filter(Region.Name %in% input$compare_metros)
    
    p <- ggplot(compare_data, aes(x = Date, y = Unemployment_Rate, 
                                  color = Region.Name, group = Region.Name)) +
      geom_line(size = 1) +
      facet_wrap(~Region.Name, ncol = 2) +
      labs(
        title = "Individual Time Series",
        x = "Date",
        y = "Unemployment Rate (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Statistical Analysis Plots
  output$trend_direction_plot <- renderPlotly({
    trend_summary <- metro_regression %>%
      count(Trend_Direction)
    
    p <- ggplot(trend_summary, aes(x = Trend_Direction, y = n, fill = Trend_Direction)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5) +
      labs(
        title = "Metro Areas by Trend Direction",
        x = "",
        y = "Number of Metros"
      ) +
      scale_fill_manual(values = c("Decreasing" = "#27ae60", 
                                   "Increasing" = "#e74c3c",
                                   "No Significant Trend" = "#95a5a6")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$volatility_plot <- renderPlotly({
    top_volatile <- metro_summary %>%
      filter(!is.na(CV)) %>%
      slice_max(CV, n = 15) %>%
      mutate(Region.Name = str_trunc(Region.Name, 30))
    
    p <- ggplot(top_volatile, aes(x = reorder(Region.Name, CV), y = CV)) +
      geom_col(fill = "#9b59b6", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Most Volatile Metro Areas (Top 15)",
        x = "",
        y = "Coefficient of Variation (%)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$state_summary_plot <- renderPlotly({
    p <- ggplot(state_summary, aes(x = reorder(State, Avg_Unemployment), 
                                   y = Avg_Unemployment,
                                   fill = Avg_Unemployment)) +
      geom_col(alpha = 0.8) +
      scale_fill_gradient(low = "#27ae60", high = "#e74c3c") +
      coord_flip() +
      labs(
        title = "Average Unemployment Rate by State",
        x = "State",
        y = "Average Unemployment Rate (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$correlation_analysis <- renderPrint({
    cat("Basic descriptive statistics:\n\n")
    print(summary(metro_summary[, c("Mean_Rate", "SD_Rate", "Range_Rate", "CV")]))
    
    cat("\n\nCorrelation matrix:\n\n")
    cor_matrix <- cor(metro_summary[, c("Mean_Rate", "SD_Rate", "Range_Rate", "CV")], 
                      use = "complete.obs")
    print(round(cor_matrix, 3))
  })
  
  # Data Tables
  output$summary_table <- renderDT({
    metro_summary %>%
      select(Region.Name, State, Mean_Rate, SD_Rate, Min_Rate, Max_Rate, N_Observations) %>%
      rename(
        "Metro Area" = Region.Name,
        "State" = State,
        "Mean Rate" = Mean_Rate,
        "Std Dev" = SD_Rate,
        "Min" = Min_Rate,
        "Max" = Max_Rate,
        "# Observations" = N_Observations
      ) %>%
      datatable(
        filter = 'top',
        options = list(pageLength = 25, scrollX = TRUE)
      ) %>%
      formatRound(columns = 3:6, digits = 2)
  })
  
  output$recent_trends_table <- renderDT({
    recent_trends %>%
      mutate(Region.Name = str_trunc(Region.Name, 40)) %>%
      rename(
        "Metro Area" = Region.Name,
        "Current Rate" = Unemployment_Rate,
        "Change" = Rate_Change,
        "% Change" = Pct_Change,
        "Trend" = Trend
      ) %>%
      datatable(
        filter = 'top',
        options = list(pageLength = 25, scrollX = TRUE)
      ) %>%
      formatRound(columns = 2:4, digits = 2)
  })
}

shinyApp(ui, server)
