library(stringr)
library(climatol)
library(climclass)
setwd("D:\\apps\\Shiny example")
months = c("Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug","Sep", "Oct", "Nov", "Dec")

data = read.csv('climate/climate_data.csv', stringsAsFactors = FALSE, sep="\t")

trange = c(min(data$TMIN, na.rm=T), max(data$TMAX, na.rm=T))

getTicks <- function (yearRg) {
  s = yearRg$JulDay
  days = str_sub(s, 4, 5)  
  which(days=="01")
}

addGrid <- function (data, varName, ticks) {
  varName = varName[varName %in% names(data)]
  if(!all(is.na(data[,varName]))){
    trange = c(min(data[,varName], na.rm=T), max(data[,varName], na.rm=T))
    x = round(trange[1],0) : round(trange[2],0)
    #print(x)
    x = x[x %% 5==0]
    abline(h=x, col="grey90")
    abline(v=ticks, col="grey90")
  }
}

filterLocality <- function (data, country=NA, locality=NA) {
  #print(country)
  if(!is.na(country)) data = data[data$CNTRY == country,] 
  #print(head(data))
  if(length(locality)>0)  {
    if(!is.na(locality)) data = data[data$ADMIN4 == locality,] 
  }
    
  data
}

# fillMissingDays <- function (data) {
#   dr = as.Date(c(data[1,"Date"], data[nrow(data),"Date"]),"%Y-%m-%d")
#   Date = seq.Date(from=as.Date(dr[1]), to = as.Date(dr[2]), by='day')
#   xdata = as.data.frame(Date, stringsAsF=F)
#   xdata[,1]= as.character(xdata[,1])
#   data = merge(xdata, data, by="Date", all=T)
#   data
# }

makeRefDayList <- function(month=c("Jan","Dec")){
  dmonths = c(1,31, 2,28, 3,31, 4,30, 5, 31, 6, 30, 7,31, 8,31, 9,30, 10,31, 11,30, 12,31)
  dmonths = matrix(dmonths, nrow=12, byrow=T)
  out=NULL
  ii = which(months %in% month)
  if(length(ii)==1) ii=c(ii,ii)
  #print(ii)
  for(i in ii[1]:ii[2]) out=c(out, paste(str_pad(i,2,pad="0"),"-",str_pad(1:dmonths[i,2],2,pad="0"),sep=""))
  out = as.data.frame(out, stringsAsFactors = FALSE)
  names(out)[1] = "JulDay"
  out
}

getY <- function(data, yearRg=NA){
  yer = str_sub(data$Date,1,4)
  JulDay = str_sub(data$Date,6,10)
  
  dat = cbind(yer,JulDay, data)
  dat[,1] = as.character(dat[,1])
  dat[,2] = as.character(dat[,2])
  yrs = unique(yer)
  vrs = 7:ncol(data)
  nms = names(data)[vrs]
  
  m = length(vrs)
  res = list()
  #rdl = makeRefDayList()
  rdl = yearRg
  
#   j = 1
#   i = 1
  n = length(yrs)
  for(j in 1:m){
    tmp = rdl
    for(i in 1:n){
      
      sdt = dat[dat$yer == yrs[i],]
      sdt = sdt[,c(2,9:ncol(sdt))]
      dps = duplicated(sdt$JulDay)
      sdt = sdt[!dps,]
      #print(vrs[j])
      
      tmp = merge(tmp, sdt[,c("JulDay",nms[j])], by="JulDay", all.x = TRUE)
      if(length(yrs[i]) > 0)  names(tmp)[i+1] = yrs[i]
      
    }
    res[[names(data)[vrs[j]]]] = tmp    
  }
  res
}

varLine <- function (Y,var, color, year) {
  n = length(year)
  if(n>0){
    for(i in 1:n){
      yy = Y[[var]]
      #print(yy)
      if(year[i] %in% names(yy)) lines(yy[,year[i]],col=color)  
    }
    
  }
}


#' plot a time series of weather variables
#' 
#' Time is a Julian year (without Feb. 29th)
#' 
#' 
plotTimeSeries <-function(data, main, ylab, varName, varState, 
                          varColor = c("red","blue","darkgreen"),
                              country=NA, locality = NA, year=NA, month=NA) {
try({
 data = filterLocality(data, country, locality)
 vn = varName %in% names(data)
 varName = varName[vn]
 varState = varState[vn]
 varColor = varColor[vn]
 #prepare some helper variables: the reference Julian days; variable range, 
 # temp database by Julian day
 yearRg = makeRefDayList(month)
 x = 1:nrow(yearRg)
 y = rep(NA,length(x))
 Y = getY(data, yearRg)
 if(!all(is.na(data[,varName]))){
 trange = c(round(min(data[,varName],na.rm=T),0), round(max(data[,varName], na.rm=T),0))
 ticks = getTicks(yearRg)
 
 if(!is.infinite(trange[1]))  plot(x,y, ylim=trange, xlab="Julian day", ylab=ylab,  main=main, xaxt="n")
 addGrid(data, varName, ticks)
 wm = which(months %in% month)
 if(length(wm)==1) wm = c(wm, wm)
 axis(1, labels=months[wm[1]:wm[2]], at=ticks)
 m = length(varName)
 for(i in 1:m){
   if(varState[i]) varLine(Y,varName[i], varColor[i], year)
 }
 }
})
}




shinyServer(function(input, output) {

  output$uilocation <- renderUI({
    data = data[data$CNTRY == input$country,"ADMIN4"]
    #print(data)
    locations = sort(unique(data))
    selectInput("locs", "Locations:", locations)
  })
  
  output$uiyear <- renderUI({
    selYears = ""
    data = data[data$CNTRY == input$country & data$ADMIN4 == input$locs,]
    selYears = sort(unique(str_sub(data$Date, 1, 4)))
    #print(selYears)
    if(length(selYears>0))     checkboxGroupInput("year","Year:",selYears, selYears)
  })
  
  output$uitillMonth <- renderUI({
    mm = input$fromMonth
    wm = which(months %in% mm)
    #print(wm)
    selectInput("tillMonth", "Till month", months[wm:12], "Dec")
  })
  
  output$plot_wl <- renderPlot({
    data = filterLocality(data, input$country, input$locs)
    #print(str(data))
    if(nrow(data)>1) chartWL(data)
  })

  output$plot_temp <- renderPlot({
    
    plotTimeSeries(data, "Temperature", "degrees C",
                       varName = c("TMEAN","TMIN","TMAX" ),
                       varState = c(input$TMEAN, input$TMIN, input$TMAX), 
                       country = input$country, 
                       locality = input$locs, 
                       year = input$year,
                   month = c(input$fromMonth, input$tillMonth))
   })
  
  output$plot_rain <- renderPlot({
    plotTimeSeries(data, "Precipitation", "mm",
                   varName = c("RAIN"),
                   varState = c(input$RAIN), 
                   varColor = c("blue"), 
                   country = input$country, 
                   locality = input$locs, 
                   year = input$year,
                   month = c(input$fromMonth, input$tillMonth))
  })
  
  output$plot_rh <- renderPlot({
    plotTimeSeries(data, "Relative humidity", "%",
                   varName = c("RHMEAN","RHMIN", "RHMAX"),
                   varState = c(input$RHMEAN, input$RHMIN, input$RHMAX), 
                   country = input$country, 
                   locality = input$locs, 
                   year = input$year,
                   month = c(input$fromMonth, input$tillMonth))
  })
})


