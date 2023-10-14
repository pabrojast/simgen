###################################################
#
# Genera un dataset de pp desde un punto CHIRPS
#
###################################################


#variables
setwd("C:/Users/Pablo-AMD/Desktop/simgen_timeseries")
estaciones <- read.csv("Selected Areas/Groot Berg_res_1.txt",sep=";")
#cuenca
cuenca <- "Groot Berg_res_1"

for (k in 1:length(estaciones$ID)) {
  coordenaday <- c(estaciones$lat[k]) #lat
  coordenadax <- c(estaciones$lon[k]) #lon
  
  #descargamos
  test <- tempfile()
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily/.global/.0p05/.prcp/T/(1%20Jan%201981)/(30%20Nov%202015)/RANGEEDGES/X/",round(coordenadax,1),"/VALUES/Y/",round(coordenaday,1),"/VALUES/T/exch/table:/text/text/:table/R.tsv", sep=""),test)
  sim <- NULL
  sim <- read.table(test, header = TRUE, sep="\t")
  
  #sim2$T <-as.Date(as.character(sim2$T), format = "%d %B %Y")
  sim$T <-seq(as.Date("1981-01-01"), as.Date("2015-11-30"), by="days")
  write.csv(sim, paste(getwd(),"/",cuenca,"/",estaciones$ID[k],"_19812015_CHIRPSonly.csv", sep=""), row.names = FALSE)

}




estaciones <- read.csv("Selected Areas/Limpopo 3_res_1.txt",sep=";")
#cuenca
cuenca <- "Limpopo 3_res_1"

for (k in 1:length(estaciones$ID)) {
  coordenaday <- c(estaciones$lat[k]) #lat
  coordenadax <- c(estaciones$lon[k]) #lon
  
  #descargamos
  test <- tempfile()
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily/.global/.0p05/.prcp/T/(1%20Jan%201981)/(30%20Nov%202015)/RANGEEDGES/X/",round(coordenadax,1),"/VALUES/Y/",round(coordenaday,1),"/VALUES/T/exch/table:/text/text/:table/R.tsv", sep=""),test)
  sim <- NULL
  sim <- read.table(test, header = TRUE, sep="\t")
  
  #sim2$T <-as.Date(as.character(sim2$T), format = "%d %B %Y")
  sim$T <-seq(as.Date("1981-01-01"), as.Date("2015-11-30"), by="days")
  write.csv(sim, paste(getwd(),"/",cuenca,"/",estaciones$ID[k],"_19812015_CHIRPSonly.csv", sep=""), row.names = FALSE)
  
}



estaciones <- read.csv("Selected Areas/Okavango_res_1.txt",sep=";")
#cuenca
cuenca <- "Okavango_res_1"

for (k in 1:length(estaciones$ID)) {
  coordenaday <- c(estaciones$lat[k]) #lat
  coordenadax <- c(estaciones$lon[k]) #lon
  
  #descargamos
  test <- tempfile()
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily/.global/.0p05/.prcp/T/(1%20Jan%201981)/(30%20Nov%202015)/RANGEEDGES/X/",round(coordenadax,1),"/VALUES/Y/",round(coordenaday,1),"/VALUES/T/exch/table:/text/text/:table/R.tsv", sep=""),test)
  sim <- NULL
  sim <- read.table(test, header = TRUE, sep="\t")
  
  #sim2$T <-as.Date(as.character(sim2$T), format = "%d %B %Y")
  sim$T <-seq(as.Date("1981-01-01"), as.Date("2015-11-30"), by="days")
  write.csv(sim, paste(getwd(),"/",cuenca,"/",estaciones$ID[k],"_19812015_CHIRPSonly.csv", sep=""), row.names = FALSE)
  
}




estaciones <- read.csv("Selected Areas/Zambezi 4_res_1.txt",sep=";")
#cuenca
cuenca <- "Zambezi 4_res_1"

for (k in 1:length(estaciones$ID)) {
  coordenaday <- c(estaciones$lat[k]) #lat
  coordenadax <- c(estaciones$lon[k]) #lon
  
  #descargamos
  test <- tempfile()
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily/.global/.0p05/.prcp/T/(1%20Jan%201981)/(30%20Nov%202015)/RANGEEDGES/X/",round(coordenadax,1),"/VALUES/Y/",round(coordenaday,1),"/VALUES/T/exch/table:/text/text/:table/R.tsv", sep=""),test)
  sim <- NULL
  sim <- read.table(test, header = TRUE, sep="\t")
  
  #sim2$T <-as.Date(as.character(sim2$T), format = "%d %B %Y")
  sim$T <-seq(as.Date("1981-01-01"), as.Date("2015-11-30"), by="days")
  write.csv(sim, paste(getwd(),"/",cuenca,"/",estaciones$ID[k],"_19812015_CHIRPSonly.csv", sep=""), row.names = FALSE)
  
}