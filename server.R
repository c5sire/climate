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

addGrid <- function (data) {
  trange = c(min(data$TMIN, na.rm=T), max(data$TMAX, na.rm=T))
  x = round(trange[1],0) : round(trange[2],0)
  x = x[x %% 5==0]
  abline(h=x, col="grey90")
}

filterLocality <- function (data, country=NA, locality=NA) {
  print(country)
  if(!is.na(country)) data = data[data$CNTRY == country,] 
  #print(head(data))
  if(!is.na(locality)) data = data[data$ADMIN4 == locality,] 
  data
}

fillMissingDays <- function (data) {
  dr = as.Date(c(data[1,"Date"], data[nrow(data),"Date"]),"%Y-%m-%d")
  Date = seq.Date(from=as.Date(dr[1]), to = as.Date(dr[2]), by='day')
  xdata = as.data.frame(Date, stringsAsF=F)
  xdata[,1]= as.character(xdata[,1])
  data = merge(xdata, data, by="Date", all=T)
  data
}

plotTimeSeriesTemp <-function(data, TMEAN=TRUE, TMIN=TRUE, TMAX=TRUE, country=NA, locality = NA) {
 data = filterLocality(data, country, locality)
 print(head(data))
 data = fillMissingDays(data)
 #print(head(data))
 
 
 
 trange = c(min(data$TMIN, na.rm=T), max(data$TMAX, na.rm=T))
 x = 1:nrow(data)
 y = rep(NA,length(x))
 plot(x,y, ylim=trange, xlab="Date", ylab="degrees C", xaxt="n", main="Temperature", sub="years")
 addGrid(data)
 
 if(TMEAN) lines(data[,"TMEAN"],col="red")
 if(TMIN) lines(data[,"TMIN"],col="blue")
 if(TMAX) lines(data[,"TMAX"],col="darkgreen")

 # for the time being just 5 tick marks
 ticks = getTicks(data)
 axis(1, labels=data$Date[ticks], at=ticks)
}




shinyServer(function(input, output) {

  output$location <- renderUI({
    data = data[data$CNTRY == input$country,"ADMIN4"]
    locations = sort(unique(data))
    selectInput("locs", "Locations:", locations)
  })
  
  output$year <- renderUI({
    #print(input$country)
    data = data[data$CNTRY == input$country & data$ADMIN4 == input$locs,]
    #print(head(data))
    selYears = sort(unique(str_sub(data$Date, 1, 4)))
    checkboxGroupInput("year","Year:",selYears, selYears)
  })
  
#   output$maxMonth <- renderUI({
#     mm = input$minMonth
#     selectInput("maxMonth", "till month", mm:12, 12)
#   })

  output$plot_temp <- renderPlot({
    plotTimeSeriesTemp(data, input$TMEAN, input$TMIN, input$TMAX, input$country, input$locs)
   })
})


