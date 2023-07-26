#SIMGEN MENSUAL A PERIODO ESTACIONAL
#22/10/2017 Primera Versión

#install.packages("magrittr")
library(magrittr)
library(stringr)
library(lubridate)
library(xts)
#ejemplo
#workdir <- "C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen/output_sim/anual"
#nums <- c(90,15,10)
#2030 - 2060 porque está como > y <
promedios <- function(workdir, nums, anhoinicio = 2029, anhofinal = 2061){
  directorioactual <- getwd()
  #listamos carpetas de percentiles
  #archivos <- list.files(workdir)
  #obtenemos percentiales numericos
  #en caso de querer todos los percentiles -->>>
  #nums <- str_match_all(archivos, "[0-9]+") %>% unlist %>% unique %>% as.numeric
  setwd(workdir)
  temperatura <- list()
  precipitacion <- list()
  #precipitacion
  for (percentil in nums) {
    #solo en caso...
    temperatura <- list()
    precipitacion <- list()
    #listamos archivos para obtener simix
    archivos <- list.files(paste(workdir,"/",percentil,"/",sep=""))
    #filtramos por simix y unicos
    simixs <- str_match_all(archivos, "[0-9]+") %>% unlist %>% unique %>% as.numeric
    
    #precipitacion
    for (nsimix in 1:length(simixs)) {
      #pp
      data <- read.table(paste(workdir,"/",percentil,"/","pp_",simixs[nsimix],".csv",sep=""), quote="\"",sep=",",header=TRUE)
      names(data) <- str_extract_all(names(data), '[0-9]+')
      names(data)[1] <- c("fecha")
      FECHA <- seq(as.Date(data[1,1]), by = "year", length.out = length(data[[1]]))
      data$fecha <- NULL
      ts <- xts(data, FECHA)
      ts <- ts[(year(ts)<anhofinal) & (year(ts)>anhoinicio)]
      ts <- ts[,as.character(names(data))]
      precipitacion <- c(precipitacion, mean(ts))
    }
    #temperatura
    for (nsimix in 1:length(simixs)) {
      #pp
      data <- read.table(paste(workdir,"/",percentil,"/","tprom_",simixs[nsimix],".csv",sep=""), quote="\"",sep=",",header=TRUE)
      names(data) <- str_extract_all(names(data), '[0-9]+')
      names(data)[1] <- c("fecha")
      FECHA <- seq(as.Date(data[1,1]), by = "year", length.out = length(data[[1]]))
      data$fecha <- NULL
      ts <- xts(data, FECHA)
      ts <- ts[(year(ts)<anhofinal) & (year(ts)>anhoinicio)]
      ts <- ts[,as.character(names(data))]
      temperatura <- c(temperatura, mean(ts))
    }
    
    names(precipitacion) <- simixs
    names(temperatura) <- simixs
    resultado <- list(precipitacion, temperatura)
    setwd(directorioactual)
    return(resultado)
  }
}



