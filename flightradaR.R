# activate some packages
library(rvest)
library(stringr)
library(jsonlite)
library(dplyr)
library(readxl)

gabung <- data.frame(bandara = c("mlg", "bwx", "sub", "jbb", "sup"))

# making json links
link <- data.frame()
for(i in gabung$bandara){
  x <- paste0("https://api.flightradar24.com/common/v1/airport.json?code=",i)
  link <- rbind(link, data.frame(x))
}
# generating json file 
dptdata <- function(x){
  x <- fromJSON(x)
  return(x)
}

# joining airport's name with links
gabung <- data.frame(bandara = gabung$bandara, link)
data <- sapply(gabung$x, FUN = dptdata, USE.NAMES = F)

datagabung <- data.frame()
for(i in seq(1,length(data))){
  if (!is.null(data[[i]]$response$airport$pluginData$schedule$departures$data$flight$airport$destination$position)) {
    # jumlah perulangan
    j <- nrow(data[[i]]$response$airport$pluginData$schedule$departures$data$flight$airport$destination$position)
    tanggal <- data[[i]]$response$airport$pluginData$schedule$arrivals$timestamp %>% as.Date.POSIXct(format = "%Y-%m-%d H:M:S")
    #perulangan asal penerbangan ke tujuan
    okeasal <- data.frame(latasal = rep(data[[i]]$response$airport$pluginData$details$position$latitude, j),
                          lonasal = rep(data[[i]]$response$airport$pluginData$details$position$longitude, j),
                          asal = rep(data[[i]]$response$airport$pluginData$details$position$region$city, j),
                          iata = rep(data[[i]]$response$airport$pluginData$details$code$iata, j))
    
    oketujuan <- data.frame(lattujuan = data[[i]]$response$airport$pluginData$schedule$departures$data$flight$airport$destination$position$latitude,
                            lontujuan = data[[i]]$response$airport$pluginData$schedule$departures$data$flight$airport$destination$position$longitude,
                            tujuan = data[[i]]$response$airport$pluginData$schedule$departures$data$flight$airport$destination$position$region$city)
    tanggal <- data.frame(tanggal = rep(data[[i]]$response$airport$pluginData$schedule$arrivals$timestamp %>% as.Date.POSIXct(), j))
    # Menggabungkan data ke dalam data frame hasil
    datagabung <- rbind(datagabung, data.frame(okeasal, oketujuan, tanggal))
  }
}

# cleaning NA from data frame
flightradar <- na.omit(datagabung)

# save the result as csv
write.csv(flightradar, "myflight.csv")

