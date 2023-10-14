library(data.table)

prenombre <- "tmax" # filename, this case tmax or tmin
dname <- "tmax"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Groot Berg_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Groot Berg_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
#limpiamos en caso de
DT <- NULL
DT2 <- NULL
DT3 <- NULL
DT4 <- NULL


for (j in 1979:2018) {
  ncname <- paste(prenombre,".",j,sep="")
  print(ncname)
  ncfname <- paste(ncname," .csv", sep="")
  anual <- read.csv(ncfname)
  #eliminamos lat y long fuera de rango 10 grados
  anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
  anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
  #eliminamos vacios
  #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
  #anual2 <- na.omit(anual)
  #quick and dirty
  #anual2$minVar2 <- abs(anual2$Var2-Var2)
  #anual2$minVar1 <- abs(anual2$Var1-Var1)
  #anual2$cercano <- anual2$minVar2  + anual2$minVar1
  
  anual$minVar2 <- abs(anual$Var2-Var2)
  anual$minVar1 <- abs(anual$Var1-Var1)
  anual$cercano <- anual$minVar2  + anual$minVar1
  serie <- anual[which.min(anual$cercano),]
  
  
  #serie <- anual2[which.min(anual2$cercano),]
  #de esta manera mantenemos los na.omit
  serie <- anual[anual$X == serie$X,]
  #writemetada
  #dont forget
  
  serie$X <- NULL
  serie$Var1 <- NULL
  serie$Var2 <- NULL
  serie$minVar1 <- NULL
  serie$minVar2 <- NULL
  serie$cercano <- NULL
  
  serie <- t(serie)
  serie2 <- as.vector(serie)
 
  if(j == 1979){
    DT = data.table(
      ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
      a = as.vector(serie)
    )
  }else{
    DT2 = data.table(
      ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
      a = as.vector(serie)
    )
  if(j == 1980){
    DT3 <- dt.append(DT,DT2)  
  }
  if(j > 1980){
    DT3 <- dt.append(DT3,DT2)
  }
    
  
  }
}
write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMAX.csv", sep=""), row.names = FALSE)
}







prenombre <- "tmax" # filename, this case tmax or tmin
dname <- "tmax"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Limpopo 3_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Limpopo 3_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMAX.csv", sep=""), row.names = FALSE)
}




prenombre <- "tmax" # filename, this case tmax or tmin
dname <- "tmax"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Okavango_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Okavango_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMAX.csv", sep=""), row.names = FALSE)
}





prenombre <- "tmax" # filename, this case tmax or tmin
dname <- "tmax"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Zambezi 4_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Zambezi 4_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMAX.csv", sep=""), row.names = FALSE)
}



#############
############tmin
############

prenombre <- "tmin" # filename, this case tmax or tmin
dname <- "tmin"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Groot Berg_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Groot Berg_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMIN.csv", sep=""), row.names = FALSE)
}







prenombre <- "tmin" # filename, this case tmax or tmin
dname <- "tmin"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Limpopo 3_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Limpopo 3_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMIN.csv", sep=""), row.names = FALSE)
}




prenombre <- "tmin" # filename, this case tmax or tmin
dname <- "tmin"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Okavango_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Okavango_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMIN.csv", sep=""), row.names = FALSE)
}





prenombre <- "tmin" # filename, this case tmax or tmin
dname <- "tmin"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#variables
output <- "C:/Users/Pablo-AMD/Desktop/simgen_timeseries"
estaciones <- read.csv(paste(output,"/Selected Areas/Zambezi 4_res_1.txt",sep=""),sep=";")
#cuenca
cuenca <- "Zambezi 4_res_1"

dt.append <- function(x1, x2) {
  obj <- deparse(substitute(x1)) # get the actual object name as a string
  assign(obj, value = data.table::rbindlist(list(x1, x2)), envir = .GlobalEnv)
  
}

for (k in 1:length(estaciones$ID)) {
  Var2 <- c(estaciones$lat[k]) #lat
  Var1 <- c(estaciones$lon[k]) #lon corx
  
  #limpiamos en caso de
  DT <- NULL
  DT2 <- NULL
  DT3 <- NULL
  DT4 <- NULL
  
  
  for (j in 1979:2018) {
    ncname <- paste(prenombre,".",j,sep="")
    print(ncname)
    ncfname <- paste(ncname," .csv", sep="")
    anual <- read.csv(ncfname)
    #eliminamos lat y long fuera de rango 10 grados
    anual <- anual[as.numeric(anual$Var2)<as.numeric(Var2+10) & as.numeric(anual$Var2)>as.numeric(Var2-10),]
    anual <- anual[as.numeric(anual$Var1)<as.numeric(Var1+10) & as.numeric(anual$Var1)>as.numeric(Var1-10),]
    #eliminamos vacios
    #eliminar vacios resulta en series con 0 datos en algunos años, no se recmienda.
    #anual2 <- na.omit(anual)
    #quick and dirty
    #anual2$minVar2 <- abs(anual2$Var2-Var2)
    #anual2$minVar1 <- abs(anual2$Var1-Var1)
    #anual2$cercano <- anual2$minVar2  + anual2$minVar1
    
    anual$minVar2 <- abs(anual$Var2-Var2)
    anual$minVar1 <- abs(anual$Var1-Var1)
    anual$cercano <- anual$minVar2  + anual$minVar1
    serie <- anual[which.min(anual$cercano),]
    
    
    #serie <- anual2[which.min(anual2$cercano),]
    #de esta manera mantenemos los na.omit
    serie <- anual[anual$X == serie$X,]
    #writemetada
    #dont forget
    
    serie$X <- NULL
    serie$Var1 <- NULL
    serie$Var2 <- NULL
    serie$minVar1 <- NULL
    serie$minVar2 <- NULL
    serie$cercano <- NULL
    
    serie <- t(serie)
    serie2 <- as.vector(serie)
    
    if(j == 1979){
      DT = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
    }else{
      DT2 = data.table(
        ID = seq(as.Date(paste("",j,"-01-01",sep="")), as.Date(paste("",j,"-12-31",sep="")), by="days"),
        a = as.vector(serie)
      )
      if(j == 1980){
        DT3 <- dt.append(DT,DT2)  
      }
      if(j > 1980){
        DT3 <- dt.append(DT3,DT2)
      }
      
      
    }
  }
  write.csv(DT3, paste(output,"/",cuenca,"/",estaciones$ID[k],"_19802018_TMIN.csv", sep=""), row.names = FALSE)
}