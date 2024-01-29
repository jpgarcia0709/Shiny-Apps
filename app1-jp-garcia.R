
#Loading Necessary Libraries 
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(shinymaterial)

# ======================================================
# Auxiliary objects (that don't depend on input widgets)
# ======================================================
#Retrieving map data of world countries
world_countries = ne_countries(returnclass = "sf")
#Retrieving ggplot2 map of North Atlantic per Professor Sanchez's Textbook
atlantic = ggplot(data = world_countries) +
  #color picking: My preference for maps is grey!
  geom_sf(fill = "grey") +
  #setting up North Atlantic coordinates
  coord_sf(xlim = c(-110, 0), ylim = c(5, 65))

# ===========================================================
# Defining our UI 
# ===========================================================
#Titling Shiny App
ui <- fluidPage(
  titlePanel("North Atlantic Storm Paths"),
#Combining inputs and outputs
  sidebarLayout(
    sidebarPanel(
      #Slider Input for year, where range is 1975-2021 (DEFAULT = 2000)
      sliderInput(inputId = "year",
                  #The slider label being set
                  label = "Choose a Year!",
                  #slider minimum and maximum, and default
                  min = 1975, max = 2021, value = 2000),
      #Dropdown (As opposed to slider) to select one of the months, or ALL 
      #(Default)
      selectInput(inputId = "month",
                  #Dropdown label, what user reads
                  label = "Select a Month!",
                  #Vector of choices of months, or ALL of the months
                  choices = c("All", "January", "February", "March", "April", "May", 
                              "June", "July", "August", "September", "October", "November", 
                              "December"),
                  #Our default value 
                  selected = "All"),
      #Slider for wind speed, ranging from dataset minimum and maximum
      sliderInput(inputId = "wind_speed",
                  #Slider label, what the user will read
                  label = "Pick Wind Speed Range:",
                  #The min and max windspeed in "storms" dataset
                  min = 10, max = 165, value = c(10, 165)),
      #Checkbox user selects to filter out to see only major hurricanes
      checkboxInput(inputId = "major_hurricanes",
                    #Our label id 
                    label = "Only View Major Hurricanes",
                    #Default Value is that the user does NOT only want major
                    #hurricanes
                    value = FALSE)
    ),
    
    mainPanel(
      plotOutput(outputId = "plot_map"),
      dataTableOutput(outputId = "summary_table")
    )
  )
)

# ======================================================
# Server logic 
# ======================================================
#Initializing server
server <- function(input, output) {
 #Initializing a reactive value that changes in accordance to input 
  tbl = reactive({
    #Filtering data via pipe
    filtered_data <- storms %>%
      #filtering actions for user actions
      filter(year == input$year,
             #In case the user does not select All, the month displayed will match 
             #the one that the user selected. if else, and All is selected,
             #All choices will be displayed
             if (input$month != "All") month == input$month else TRUE,
             #Line refers to the input 
             #The first (min) number selected by user is selected
             #The second (max) number by user is selected
             #Then, the data will be filtered to include storms with 
             #wind speeds greater or equal to min value selected,
             #As well as less than or equal max value selected by user
             wind >= input$wind_speed[1] & wind <= input$wind_speed[2],
             #Line refers back to the input, whether or not the major hurricanes
             #checkbox is selected, then data will be filtered to 
             #include only category 3,4, and 5, checking  the original 
             #dataset column
             #If the checkbox is not selected, the line will return TRUE, 
             #meaning that there are no filters, so all of the storms will show
             if (input$major_hurricanes) category %in% c(3, 4, 5) else TRUE)
    
    return(filtered_data)
  })
  #Generating Plot
  output$plot_map <- renderPlot({
    #Plotting on North Atlantic Region
    atlantic +
      #Referring back to the tbl that we manipulated above,
      #we plot the storm path using geom_path, mapping the longitude on the x,
      # and latitude on the y, and each name will be colored differently  
      #The thickness of the line will correspond to the wind, showing 
      #Thicker lines for greater wind speed
      geom_path(data = tbl(), aes(x=long, y=lat, color=name, size = wind)) 
    
  })
  #Generating our data summary
  output$summary_table <- renderDataTable({
    #Using the pipe to manipulate the tbl we made
    tbl() |>
      #We group by name to gain a different entry for each storm
      group_by(name) |>
      #We use the summarise function to generate our table of summaries
      summarise(start_date = paste(first(month), "/", first(day)),
                #We use the paste function to paste together the last 
                #month and last day so that it takes a format of 
                #MM-DD in our table 
                end_date = paste(last(month), "/", last(day)),
                #We gain the max_wind entry by selecting the maximum value for 
                #wind speed
                max_wind = max(wind),
                #We select the minimum pressure for each entry using the min 
                #function
                min_pressure = min(pressure))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

