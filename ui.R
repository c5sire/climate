setwd("D:\\apps\\Shiny example")
library(stringr)

months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data = read.csv("climate//climate_data.csv", stringsAsFactors=F, sep="\t")

# ##########add
# filterLocality <- function (data, country2=NA, locality2=NA) {
#   if(!is.na(country2)) data = data[data$CNTRY == country2,]
#   if(length(locality2)>0) {
#     if(!is.na(locality2)) data = data[data$ADMIN4 == locality2,]
#   }
#   data
# }

#data = filterLocality(data, "Peru", "La Molina")
# minDate = data[1,6]
# maxDate = data[nrow(data),6]
##########

selCountries = sort(unique(data$CNTRY))

shinyUI(pageWithSidebar(
  
  headerPanel("Climate Data: SSA Region"),
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     wellPanel(
                       h4("Step 1: Select your location"),
                       br(),
                       selectInput("country","Country:",selCountries),
                       uiOutput("uilocation")
                     ),
                     
                     wellPanel(
                       h4("Step 2: Select climate variables"),
                       br(),
                       p(strong("Temperature:")),
                       checkboxInput(inputId = "TMEAN", label = "T mean", value = TRUE),
                       checkboxInput(inputId = "TMIN", label = "T minimum", value = TRUE),
                       checkboxInput(inputId = "TMAX", label = "T maximum", value = TRUE),
                       br(),
                       p(strong("Precipitation:")),
                       checkboxInput(inputId = "RAIN", label = "Precipitation", value = TRUE),
                       br(),
                       p(strong("Relative humidit:")),
                       checkboxInput(inputId = "RHMEAN", label = "Rel. Humiditiy mean", value = TRUE),
                       checkboxInput(inputId = "RHMIN", label = "Rel. Humiditiy min", value = TRUE),
                       checkboxInput(inputId = "RHMAX", label = "Rel. Humiditiy max", value = TRUE)
                     ),
                     
                     wellPanel(
                       h4("Step 3: Select your filter months"),
                       br(),
                       selectInput("fromMonth","From month",months),
                       uiOutput("uitillMonth")
                     ),
                     
                     wellPanel(
                       uiOutput("uiyear")
                     )
    ),
    
    conditionalPanel(condition="input.conditionedPanels==2",
                     wellPanel(
                       h4("Step 1: Select your location"),
                       br(),
                       selectInput("country2","Country:",selCountries),
                       uiOutput("uilocation2")
                     ),
                     
                     wellPanel(
                       #dateRangeInput("dateRange","Date range",min = minDate, max = maxDate, start=minDate, end=maxDate)
                       uiOutput("uiDateRange")
                     )
    ),
    
    conditionalPanel(condition="input.conditionedPanels==3",
                     helpText("Content Panel 3")
    )
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Plot 1: Julian day", value=1,
               p("Walter-Lieth climate chart"),
               div(plotOutput(outputId = "plot_wl")),
               br(),
               div(plotOutput(outputId = "plot_temp")),
               br(),
               div(plotOutput(outputId = "plot_rain")),
               br(),
               div(plotOutput(outputId = "plot_rh"))
      ), 
      tabPanel("Plot 2: Time series", value=2,
               div(plotOutput(outputId = "test"))),
      tabPanel("Table", value=3),
      id = "conditionedPanels"
    )
  )
))