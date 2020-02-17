# This app is designed to produce an interface where users can create a variety of visualization
# tools from data collected as part of the Flathead Lake Monitoring Program
# This particular version of the app was written by Esther Lyon Delsordo in 2019
# as part of an internship with Dr. Shawn Devlin at the Flathead Lake Biological Station
# of the University of Montana

# Load ui packages
library(shiny)
library(dygraphs)
library(shinythemes)

# Load server packages
library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)

# Define parameters
lims_vars <- lims$Param
hydrolab_vars <- c('Temperature', 'pH', 'DO (mg/L)', 'DO (%sat)', 'Conductivity', 
                   'ORP', 'Turbidity', 'CHLa (RFU)', 'CHLa (Volts)')
y_options <- c(lims_vars, hydrolab_vars)
x_options <- y_options
depth <- hydrolab$Depth

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("FMP Visualizer"),
   
   # Sidebar with tabs for each plot type 
   sidebarLayout(
     sidebarPanel(
       width = 4,
       conditionalPanel(condition = "input.tabs = c('Times Series', 'X-Y Plot', 'Depth Profile')",
                        tabsetPanel(
                          tabPanel(
                            'Overview',
                            p(
                              "This app is designed to produce an interface where users can create a variety of visualization
                              tools from data collected as part of the Flathead Lake Monitoring Program (FMP). Learn more about station 
                              monitoring at:",
                              a('FLBS Monitoring', href = 'https://flbs.umt.edu/newflbs/research/lake-monitoring/')
                            ),
                            br(),
                            p(
                              em("This application was built by Esther Lyon Delsordo under the supervision of Shawn Devlin of FLBS."
                                 )
                            )
                          )
                        )
                        ),
       conditionalPanel(condition = "input.tabs = 'Time Series'",
                        tabsetPanel(
                          tabPanel(
                            'Input',
                            dateRangeInput('dateRange',
                                           label = 'Date Range: yyyy-mm-dd',
                                           start = as.Date('1984-01-01'), end = Sys.Date()
                            ),
                            selectInput("y_options", label = "Parameter")
                          )
                        )),
       conditionalPanel(condition = "input.tabs = 'X-Y Plot'",
                        tabsetPanel(
                          tabPanel(
                            'Input',
                            selectInput("x_options", label = "X Axis Parameter"),
                            selectInput("y_options", label = "Y Axis Parameter")
                          )
                        )),
       conditionalPanel(condition = "input.tabs = 'X-Y Plot'",
                        tabsetPanel(
                          tabPanel(
                            'Input',
                            sliderInput("depth", label = "Depth Range"),
                            selectInput("y_options", label = "Parameter")
                          )
                        ))
     ),

  #setup main display
  mainPanel(
    width = 8,
    tabsetPanel(
      id = 'tabs', 
      tabPanel(
        
      )
      ))
     
     
))

# Define server function
server <- function(input, output) {
  
#Data Wrangling:
   
  # Load data as tibbles
  lims <- as_tibble(
    read.csv('lims.csv', 
             stringsAsFactors = F))
  hydrolab <- as_tibble(
    read.csv('hydrolab.csv', 
             stringsAsFactors = F))
  
  # Rename columns
  names(lims)[names(lims) == 'CorrectedReportedResult'] <- 'CRR'
  lims$CRR <- as.numeric(lims$CRR)
  
  # Remove NPP from lims test column, remove hydrolab unit row
  lims <- lims[!(lims$Test=="NPP"),]
  hydrolab <- hydrolab[-1,]
  
  # Convert hydrolab test colums to numerics
  hydrolab %>%
    mutate_all(type.convert) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, as.numeric)
  
  # Lubridate
  lims <- lims %>%
    mutate(Date = mdy_hms(CollectDate),
           Year = year(Date),
           Month = month(Date)
    )
  
  hydrolab <- hydrolab %>%
    mutate(Date = mdy(Date),
           Year = year(Date),
           Month = month(Date)
    )
  
# Begin the output
  output$tabPlot <- renderPlot({
      # if statements to make plot reactive to input
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

