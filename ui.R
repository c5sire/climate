setwd("D:\\apps\\Shiny example")
library(stringr)

data = read.csv("climate//climate_data.csv", stringsAsFactors=F, sep="\t")

#minDate = data[1,"Date"]
#maxDate = data[nrow(data),"Date"]

selCountries = sort(unique(data$Country))
#selYears = sort(unique(str_sub(data$Date, 1, 4)))



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
      #checkboxGroupInput("year","Year:",selYears, selYears)#,
      #selectInput("minMonth", "from month",1:12,1),
      #uiOutput("maxMonth")
    )
    
  ),
  
  mainPanel(
    br(),
    div(plotOutput(outputId = "plot_temp"))
  )
))