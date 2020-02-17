library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)

##ui

tabsetPanel(
  tabPanel("Time Series",
           dateRangeInput('dateRange',
                          label = 'Date range input: yyyy-mm-dd',
                          start = as.Date('1984-01-01'), end = Sys.Date()
           ),
           selectInput("y_options", label = "Parameter")
  ),
  tabPanel("X-Y Plot",
           selectInput("x_options", label = "X-Axis"),
           selectInput("y_options", label = "Y-Axis")
  ),
  tabPanel("Depth Profile",
           rangeInput("depth", label = "Depth Range"),
           selectInput("hydrolab_vars", label = "Parameter")
  ),
  tabPanel('Download',
           downloadButton('downloadData', 'Download CSV'),
           br(),
           downloadButton('downloadManual', 'Download HTML Manual')
  )
)

# Show a plot of the generated distribution
mainPanel(
  plotOutput("tabPlot")
)

##end ui


##server

##read in lims and hydrolab as tibbles
lims <- as_tibble(read.csv('lims.csv', stringsAsFactors = FALSE))
hydrolab <- as_tibble(read.csv('hydrolab.csv', stringsAsFactors = FALSE))

##rename columns
names(lims)[names(lims) == 'CorrectedReportedResult'] <- 'CRR'
lims$CRR <- as.numeric(lims$CRR)

##remove NPP from lims test column, remove hydrolab unit row
lims <- lims[!(lims$Test=="NPP"),]
hydrolab <- hydrolab[-1,]

##lubridate
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
  
##convert hydrolab test colums to numerics
hydrolab %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.numeric)

##spread lims by Param
lims.Wide <- spread(lims, Param, CRR)

##Shawn's code to convert various chem names to a single consistent name
lims.Nfix <- lims.Wide %>%
  select(everything(), -StartDepth) %>%
  rename(Chl = "Chl-a") %>%
  mutate(Chla = coalesce(Chl, CHLa)) %>%
  select(everything(), -Chl, -CHLa) %>%
  rename(NH4N = "NH4-N",
         NO23 = "NO2/3",
         NO3NO2 = "NO3/NO2",
         NO3N = "NO3-N",
         NO2N ="NO2-N") %>%
  mutate(NHx = rowSums(cbind (NH4N, NH3), na.rm=TRUE),
         NOx = rowSums(cbind (NO2N, NO23, NO3, NO3N, NO3NO2), na.rm=T),
         DIN = rowSums(cbind(NOx, NHx), na.rm=T),
         ArcTN = ifelse(is.na(TKN), NA, TKN + DIN),
         TotalN = coalesce(ArcTN, TN, TPN)) %>%
  select(SRP, TotalN, TP, Chla)%>%
  distinct()


##grouped means by aggregation, make reactive

# Subset data
selected_trends <- reactive({
  req(input$date)
  validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
  validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
  trend_data %>%
    filter(
      type == input$type,
      date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
      ))
})






##end server