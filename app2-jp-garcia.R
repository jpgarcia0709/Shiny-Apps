library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(reshape2)

# Defining UI 
ui <- fluidPage(
  titlePanel("Retirement Visualizer"),
  #Widgets 
  sidebarLayout(
    sidebarPanel(
      numericInput("initial_salary", "Initial Salary:", value = 80000),
      numericInput("growth_rate", "Rate of Growth (%):", value = 0.02, min = 0, max = 1, step = 0.01),
      numericInput("contribution_percentage", "Contribution Percentage (%):", value = 0.15, min = 0, max = 1, step = 0.01),
      selectInput("number_of_periods", "Number of Periods:",
                  choices = c("Annually" = 1, "Semi-Annually" = 2, "Quarterly" = 4, "Bimonthly" = 6, "Monthly" = 12, "Weekly" = 52),
                  selected = 12),
      numericInput("years_invested", "Years Invested:", value = 5),
      numericInput("annual_rate_of_return", "Annual Rate of Return (%):", value = 0.08, min = 0, max = 1, step = 0.01),
      numericInput("target_amount", "Target Amount:", value = 35000),
      checkboxInput("show_target", "Show Target on Plots", value = FALSE)
    ),
    
    mainPanel(
      plotlyOutput("plot1"),
      plotlyOutput("plot2"),
      dataTableOutput("table")
    )
  )
)

# Server
server <- function(input, output) {
  # Calculate balances using reactive 
  balances <- reactive({
    years <- 1:input$years_invested
    salary <- rep(input$initial_salary, input$years_invested) * (1 + input$growth_rate) ^ (years - 1)
    contributions <- salary * input$contribution_percentage
    balance <- cumsum(contributions)
    growth <- balance - cumsum(contributions) #growth calculation
    own_pct <- (cumsum(contributions) / balance) * 100
    growth_pct <- (growth / balance) * 100
    hit_target <- if_else(balance >= input$target_amount, "Yes", "No")
    #Creating a data frame where we essentially define everything and assign values to our labels that the user will see
    data.frame(
      Year = years,
      Salary = salary,
      Contribution = contributions,
      Balance = balance,
      Own = cumsum(contributions),
      Growth = growth,
      Own_pct = own_pct,
      Growth_pct = growth_pct,
      Hit_target = hit_target
    )
  })
  # Plot 1: Balance timeline
  output$plot1 <- renderPlotly({
    df <- balances()
    p <- ggplot(df, aes(x = Year, y = Balance)) + 
      geom_line() + 
      geom_point() +
      theme_minimal()
    ggplotly(p)
  })
  
  # Plot 2: Composition of retirement balance
  #Stacked bar chart
  output$plot2 <- renderPlotly({
    df <- balances()
    melted_df <- melt(df, id.vars = "Year", measure.vars = c("Own", "Growth")) #I got my desired output with a melt but I began having troubles with it
    p <- ggplot(melted_df, aes(x = factor(Year), y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Table: Summary of balances
  #We format the table in an interactive way for the user to sift through it 
  output$table <- renderDataTable({
    df <- balances()
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE)) |>
      formatRound(columns = 3:8, digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

