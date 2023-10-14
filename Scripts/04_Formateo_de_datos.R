################
###Formateo de los datos
################

setwd("C:/Users/pablo/Desktop/Koen/Cuencas/Groot Berg_res_1")


metadadata <- "C:/Users/pablo/Desktop/Koen/Cuencas/Metadata/Groot Berg_res_1.txt"
estaciones <- read.csv(metadadata,sep=";")

#nombres
tmax <- "_19802018_TMAX.csv"
tmin <- "_19802018_TMIN.csv"
pp <- "_19812015_CHIRPSonly.csv"

for (k in 1:length(estaciones)) {

  
  #read the data
  tmax.sta <- read.csv(paste(estaciones$ID[k],tmax,sep=""))
  tmin.sta <- read.csv(paste(estaciones$ID[k],tmin,sep=""))
  pp.sta <- read.csv(paste(estaciones$ID[k],pp,sep=""))
  
  #same timeseries
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)>as.Date("1980-12-31"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)>as.Date("1980-12-31"),]
  
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)<as.Date("2015-01-01"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)<as.Date("2015-01-01"),]
  pp.sta <- pp.sta[as.Date(pp.sta$T) < as.Date("2015-01-01"),]
  
  #-999 to NA
  tmax.sta$a[is.na(tmax.sta$a)] <- -999
  tmin.sta$a[is.na(tmin.sta$a)] <- -999
  pp.sta$prcp[is.na(pp.sta$prcp)] <- -999
  
  #generate a df
  data <- data.frame(
    year = format(as.Date(pp.sta$T),"%Y"),
    month = format(as.Date(pp.sta$T),"%m"),
    day = format(as.Date(pp.sta$T),"%d"),
    precip=round(pp.sta$prcp,2),
    tmax=round(tmax.sta$a,2),
    tmin=round(tmin.sta$a,2),
    stringsAsFactors=FALSE
  )
  
  write.table(data, paste(getwd(),"/Simgen/",estaciones$ID[k],"_19812014.csv", sep=""), row.names = FALSE, sep=";")
  
}







#########Segunda Estacion#############

################
###Formateo de los datos
################

setwd("C:/Users/pablo/Desktop/Koen/Cuencas/Limpopo 3_res_1")


metadadata <- "C:/Users/pablo/Desktop/Koen/Cuencas/Metadata/Limpopo 3_res_1.txt"
estaciones <- read.csv(metadadata,sep=";")

#nombres
tmax <- "_19802018_TMAX.csv"
tmin <- "_19802018_TMIN.csv"
pp <- "_19812015_CHIRPSonly.csv"

for (k in 1:length(estaciones$ID)) {
  
  
  #read the data
  tmax.sta <- read.csv(paste(estaciones$ID[k],tmax,sep=""))
  tmin.sta <- read.csv(paste(estaciones$ID[k],tmin,sep=""))
  pp.sta <- read.csv(paste(estaciones$ID[k],pp,sep=""))
  
  #same timeseries
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)>as.Date("1980-12-31"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)>as.Date("1980-12-31"),]
  
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)<as.Date("2015-01-01"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)<as.Date("2015-01-01"),]
  pp.sta <- pp.sta[as.Date(pp.sta$T) < as.Date("2015-01-01"),]
  
  #-999 to NA
  tmax.sta$a[is.na(tmax.sta$a)] <- -999
  tmin.sta$a[is.na(tmin.sta$a)] <- -999
  pp.sta$prcp[is.na(pp.sta$prcp)] <- -999
  
  #generate a df
  data <- data.frame(
    year = format(as.Date(pp.sta$T),"%Y"),
    month = format(as.Date(pp.sta$T),"%m"),
    day = format(as.Date(pp.sta$T),"%d"),
    precip=round(pp.sta$prcp,2),
    tmax=round(tmax.sta$a,2),
    tmin=round(tmin.sta$a,2),
    stringsAsFactors=FALSE
  )
  
  write.table(data, paste(getwd(),"/Simgen/",estaciones$ID[k],"_19812014.csv", sep=""), row.names = FALSE, sep=";")
  
}






#########Tercera Estacion#############

################
###Formateo de los datos
################

setwd("C:/Users/pablo/Desktop/Koen/Cuencas/Okavango_res_1")


metadadata <- "C:/Users/pablo/Desktop/Koen/Cuencas/Metadata/Okavango_res_1.txt"
estaciones <- read.csv(metadadata,sep=";")

#nombres
tmax <- "_19802018_TMAX.csv"
tmin <- "_19802018_TMIN.csv"
pp <- "_19812015_CHIRPSonly.csv"

for (k in 1:length(estaciones$ID)) {
  
  
  #read the data
  tmax.sta <- read.csv(paste(estaciones$ID[k],tmax,sep=""))
  tmin.sta <- read.csv(paste(estaciones$ID[k],tmin,sep=""))
  pp.sta <- read.csv(paste(estaciones$ID[k],pp,sep=""))
  
  #same timeseries
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)>as.Date("1980-12-31"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)>as.Date("1980-12-31"),]
  
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)<as.Date("2015-01-01"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)<as.Date("2015-01-01"),]
  pp.sta <- pp.sta[as.Date(pp.sta$T) < as.Date("2015-01-01"),]
  
  #-999 to NA
  tmax.sta$a[is.na(tmax.sta$a)] <- -999
  tmin.sta$a[is.na(tmin.sta$a)] <- -999
  pp.sta$prcp[is.na(pp.sta$prcp)] <- -999
  
  #generate a df
  data <- data.frame(
    year = format(as.Date(pp.sta$T),"%Y"),
    month = format(as.Date(pp.sta$T),"%m"),
    day = format(as.Date(pp.sta$T),"%d"),
    precip=round(pp.sta$prcp,2),
    tmax=round(tmax.sta$a,2),
    tmin=round(tmin.sta$a,2),
    stringsAsFactors=FALSE
  )
  
  write.table(data, paste(getwd(),"/Simgen/",estaciones$ID[k],"_19812014.csv", sep=""), row.names = FALSE, sep=";")
  
}




#########Cuarta Estacion#############

################
###Formateo de los datos
################

setwd("C:/Users/pablo/Desktop/Koen/Cuencas/Zambezi 4_res_1")


metadadata <- "C:/Users/pablo/Desktop/Koen/Cuencas/Metadata/Zambezi 4_res_1.txt"
estaciones <- read.csv(metadadata,sep=";")

#nombres
tmax <- "_19802018_TMAX.csv"
tmin <- "_19802018_TMIN.csv"
pp <- "_19812015_CHIRPSonly.csv"

for (k in 1:length(estaciones$ID)) {
  
  
  #read the data
  tmax.sta <- read.csv(paste(estaciones$ID[k],tmax,sep=""))
  tmin.sta <- read.csv(paste(estaciones$ID[k],tmin,sep=""))
  pp.sta <- read.csv(paste(estaciones$ID[k],pp,sep=""))
  
  #same timeseries
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)>as.Date("1980-12-31"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)>as.Date("1980-12-31"),]
  
  tmax.sta <- tmax.sta[as.Date(tmax.sta$ID)<as.Date("2015-01-01"),]
  tmin.sta <- tmin.sta[as.Date(tmin.sta$ID)<as.Date("2015-01-01"),]
  pp.sta <- pp.sta[as.Date(pp.sta$T) < as.Date("2015-01-01"),]
  
  #-999 to NA
  tmax.sta$a[is.na(tmax.sta$a)] <- -999
  tmin.sta$a[is.na(tmin.sta$a)] <- -999
  pp.sta$prcp[is.na(pp.sta$prcp)] <- -999
  
  #generate a df
  data <- data.frame(
    year = format(as.Date(pp.sta$T),"%Y"),
    month = format(as.Date(pp.sta$T),"%m"),
    day = format(as.Date(pp.sta$T),"%d"),
    precip=round(pp.sta$prcp,2),
    tmax=round(tmax.sta$a,2),
    tmin=round(tmin.sta$a,2),
    stringsAsFactors=FALSE
  )
  
  write.table(data, paste(getwd(),"/Simgen/",estaciones$ID[k],"_19812014.csv", sep=""), row.names = FALSE, sep=";")
  
}