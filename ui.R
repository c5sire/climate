setwd("D:\\apps\\Shiny example\\climate")

data = read.csv("climate_data.csv", stringsAsFactors=F)

minDate = data[1,1]
maxDate = data[nrow(data),1]
print(maxDate)




shinyUI(pageWithSidebar(
  headerPanel("Climate Data (Burundi-Gisozi)"),
  
  sidebarPanel(
    wellPanel(
      p(strong("Variables")),
      checkboxInput(inputId = "temp", label = "Temperature", value = TRUE),
      checkboxInput(inputId = "prec", label = "Precipitation", value = FALSE),
      checkboxInput(inputId = "rh", label = "RH", value = FALSE)
    ),
    
    wellPanel(
      dateRangeInput("dateRange","Date range",min = minDate, max = maxDate, start=minDate, end=maxDate)

     )
    
  ),
  
  mainPanel(
    conditionalPanel(condition = "input.temp",
                     br(),
                     div(plotOutput(outputId = "plot_temp"))),
    
    conditionalPanel(condition = "input.prec",
                     br(),
                     div(plotOutput(outputId = "plot_prec"))),
    
    conditionalPanel(condition = "input.rh",
                     br(),
                     div(plotOutput(outputId = "plot_rh")))
  )
))