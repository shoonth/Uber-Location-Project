# import data
junjuly14 <- read.csv("~/Desktop/junjuly14.csv");

#convert to time series
junjuly14$date_time <- as.POSIXct(as.character(junjuly14$date_time), format = "%Y-%m-%d %H:%M")

uniquePlaces<-unique(junjuly14$zone)
uniqueTime <- unique(subset(junjuly14$date_time, junjuly14$date_time < '2014-07-02 00:00:00' & junjuly14$date_time >= '2014-07-01 00:00:00'))
forecastresult <- data.frame()
for(place in uniquePlaces){
  for(time in uniqueTime) {
  #select a subset
    airport <- subset(junjuly14, junjuly14$zone == place & junjuly14$ date_time < time)
    if (length(airport$zone) > 1) {
      x <- xts(airport$count, airport$date_time, order.by = airport$date_time)
      fitresult = auto.arima(x)
      y <- forecast(fitresult, h = 1)
      newrow <- data.frame(zone = place, forcasttime = as.POSIXct(time, origin="1970-01-01"), forcastcount = round(as.numeric(y$mean), digits = 0))
      forecastresult <- rbind(forecastresult, newrow)
      print(newrow)
    }
  }
}
  
  # print(summary(Hourly.auto.arima.fit))
  

  

