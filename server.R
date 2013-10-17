#library(quantmod)
setwd("D:\\apps\\Shiny example")

data = read.csv('climate/climate_data.csv', stringsAsFactors = FALSE, sep="\t")

trange = c(min(data$TMIN, na.rm=T), max(data$TMAX, na.rm=T))

#' Get ticks
#' 
#' @param data the data table
#' @return vector of tick mark locations
getTicks <- function (data) {
  tickN = 5
  ticks = integer(tickN)
  ticks[1] = 1
  ticks[5] = nrow(data)
  ticks[3] = ticks[1] + round(((ticks[5]-ticks[1])/2),0)
  ticks[2] = ticks[1] + round(((ticks[3]-ticks[1])/2),0)
  ticks[4] = ticks[3] + round(((ticks[5]-ticks[3])/2),0)
  ticks
}

plotTimeSeries <-function(data, TMEAN=TRUE, TMIN=TRUE, TMAX=TRUE) {
 # make sure to have a complete range of days between start and end date
 dr = as.Date(c(data[1,"Date"], data[nrow(data),"Date"]),"%Y-%m-%d")
 Date = seq.Date(from=as.Date(dr[1]), to = as.Date(dr[2]), by='day')
 xdata = as.data.frame(Date, stringsAsF=F)
 xdata[,1]= as.character(xdata[,1])
 data = merge(xdata, data, by="Date", all=T)
 
 #print(data)

 #print(data1)
 x = 1:nrow(data)
 y = rep(NA,length(x))
 plot(x,y, ylim=trange, xlab="Date", ylab="degrees C", xaxt="n", main="Temperature", sub="years")
 abline(h=5, col="grey90")
 abline(h=15, col="grey90")
 if(TMEAN) lines(data[,"TMEAN"],col="red")
 if(TMIN) lines(data[,"TMIN"],col="blue")
 if(TMAX) lines(data[,"TMAX"],col="darkgreen")

 # for the time being just 5 tick marks
 ticks = getTicks(data)
 axis(1, labels=data$Date[ticks], at=ticks)
}




shinyServer(function(input, output) {

  output$location <- renderUI({
    data = data[data$Country == input$country,"Locality"]
    #print(data)
    locations = sort(unique(data))
    #print(locations)
    selectInput("locs", "Locations:", locations)
  })
  
  output$year <- renderUI({
    data = data[data$Country == input$country & data$Locality == input$locs,]
    selYears = sort(unique(str_sub(data$Date, 1, 4)))
    checkboxGroupInput("year","Year:",selYears, selYears)
  })
  
  output$maxMonth <- renderUI({
    mm = input$minMonth
    selectInput("maxMonth", "till month", mm:12, 12)
  })

  output$plot_temp <- renderPlot({
    plotTimeSeries(data, input$TMEAN, input$TMIN, input$TMAX)
   })
})


