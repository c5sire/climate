library(quantmod)
setwd("D:\\apps\\Shiny example\\climate")
toDate <- function(x) as.Date(x, origin = "1899-12-30")
z <- read.zoo("climate_data.csv", header = TRUE, sep = ",", FUN = toDate)
x <- as.xts(z)

xx = read.csv('climate_data.csv', stringsAsFactors = FALSE)

trange = c(min(xx$TMIN), max(xx$TMAX))
#trange = c(min(xxMin[,c(4:ncol(xxMin)]), max(xxMax[,c(4:ncol(xxMax))]))

#Temperature
TMEAN <- x[,1]
TMIN <- x[,2]
TMAX <- x[,3]


#Precipitation
Precipitation <- x[,4]

#RH
RH <- x[,5]


timeSeries <- function(x, Temp, dateRange) {
{
   chartSeries(Temp,
                #TA = x[,2:3],
                type = input$chart_type,
		subset = dateRange,
		yrange=c(0,30),
                theme = "white")
   addTA(x[,2], on=1)
}
}


plotTimeSeries <-function(data, dateRange) {
 #print(dateRange)

 data=data[data$Index >= dateRange[1] & data$Index <= dateRange[2],] 

 # make sure to have a complete range of days between start and end date
 dr = as.Date(dateRange,"%Y-%m-%d")
 Index = seq.Date(from=as.Date(dr[1]), to = as.Date(dr[2]), by='day')
 xdata = as.data.frame(Index, stringsAsF=F)
 xdata[,1]= as.character(xdata[,1])
 data = merge(xdata, data, by="Index", all=T)


 #print(data1)
 x = 1:nrow(data)
 #data[10, 'TMEAN']=NA
 plot(x,data[,"TMEAN"], type="l", col="red", ylim=trange, xlab="Date", ylab="degrees C", xaxt="n",
         main="Temperature", sub="years")
 abline(h=5, col="grey90")
 abline(h=15, col="grey90")
 lines(data[,"TMEAN"],col="red")
 #plot(x,data[,"TMEAN"], type="l", col="red", ylim=trange, xlab="Date", ylab="degrees C", xaxt="n", add=T)
 lines(data[,"TMIN"],col="blue")
 lines(data[,"TMAX"],col="darkgreen")

 # for the time being just 5 tick marks
 tickN = 5
 ticks = integer(tickN)

 ticks[1] = 1
 ticks[5] = nrow(data)
 ticks[3] = ticks[1] + round(((ticks[5]-ticks[1])/2),0)
 ticks[2] = ticks[1] + round(((ticks[3]-ticks[1])/2),0)
 ticks[4] = ticks[3] + round(((ticks[5]-ticks[3])/2),0)
 
 
 axis(1, labels=data$Index[ticks], at=ticks)
 
 #text(30,10,"some text")

}




shinyServer(function(input, output) {



  output$plot_temp <- renderPlot({
    minDate = input$dateRange[1]
    maxDate = input$dateRange[2]
    dateRange = paste(minDate,"::", maxDate,sep="")
    plotTimeSeries(xx, input$dateRange)

   })
  output$plot_prec <- renderPlot({
    chartSeries(Precipitation,
                type = input$chart_type,
                subset = paste("last", input$time_num, input$time_unit),
                theme = "white")
  })
  output$plot_rh <- renderPlot({
    chartSeries(RH,
                type = input$chart_type,
                subset = paste("last", input$time_num, input$time_unit),
                theme = "white")
  })
})