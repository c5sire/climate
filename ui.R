setwd("D:\\apps\\Shiny example")
library(stringr)

data = read.csv("climate//climate_data.csv", stringsAsFactors=F, sep="\t")

selCountries = sort(unique(data$CNTRY))

shinyUI(pageWithSidebar(
  headerPanel("Climate Data"),
  
  sidebarPanel(
    wellPanel(
      selectInput("country","Country:",selCountries),
      uiOutput("location")
    ),
    
    wellPanel(
      p(strong("Variables")),
      checkboxInput(inputId = "TMEAN", label = "TMEAN", value = TRUE),
      checkboxInput(inputId = "TMIN", label = "TMIN", value = TRUE),
      checkboxInput(inputId = "TMAX", label = "TMAX", value = TRUE)
    ),
    
    wellPanel(
      uiOutput("year")
    )
    
  ),
  
  mainPanel(
    br(),
    div(plotOutput(outputId = "plot_temp"))
  )
))