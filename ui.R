setwd("D:\\apps\\Shiny example")
library(stringr)

months = c("Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep", "Oct", "Nov", "Dec")

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
      checkboxInput(inputId = "TMEAN", label = "T mean", value = TRUE),
      checkboxInput(inputId = "TMIN", label = "T minimum", value = TRUE),
      checkboxInput(inputId = "TMAX", label = "T maximum", value = TRUE),
      
      checkboxInput(inputId = "RAIN", label = "Precipitation", value = TRUE),
      
      checkboxInput(inputId = "RHMEAN", label = "Rel. Humiditiy mean", value = TRUE),
      checkboxInput(inputId = "RHMIN", label = "Rel. Humiditiy min", value = TRUE),
      checkboxInput(inputId = "RHMAX", label = "Rel. Humiditiy max", value = TRUE)
    ),
    
    wellPanel(
      selectInput("fromMonth","From month",months),
      #selectInput("tillMonth","Till month",months, "Dec")
      uiOutput("tillMonth")
    ),
    
    
    wellPanel(
      uiOutput("year")
    )
    
    
  ),
  
  mainPanel(
    br(),
    div(plotOutput(outputId = "plot_temp")),
    br(),
    div(plotOutput(outputId = "plot_rain")),
    br(),
    div(plotOutput(outputId = "plot_rh"))
    
  )
))