###################################
#
# Script final adaptación datos Huasco a Simgen.
#
###########################################
library(missForest)
setwd("D:/_UNESCO/MWAR_LAC/CAZALC-RALCEA-MWAR-LAC Simgen/DatosHuasco_prep/selectedstations_Huasco")

##############
#PRIMERA PARTE
##############

#Esta primera parte es útil para determinar qué estaciones se van
# a utilizar, haciendo un análisis subjetivo y visual.
#--------------------------------------------------------

#Archivos en directorio
archivos <- list.files("Input_Datos", pattern = ".csv") 

#Indicar fecha donde aparece el primer dato observado (de T y Pp)
#---------------------------------------------------------------

#listar código de estaciones.
st_codes <- strsplit(archivos, split = "_")
st_codes <- sapply(st_codes, function(x) x[2])
st_codes <- levels(as.factor(st_codes)) #okay

#crear un Data Frame que almacene la información de cada estación (fecha que aparece el primer valor)
st_datainfo <- data.frame(matrix(nrow = length(st_codes), ncol = 7))
colnames(st_datainfo) <- c("station", "precip_first", "precip_last", "tmin_first", "tmin_last", "tmax_first", "tmax_last")
st_datainfo$station <- st_codes


for (i in 1:nrow(st_datainfo)){
  
  #Pp análisis
  #-----------
  analisis <- read.delim(paste(getwd(), "/Input_Datos/", "st_", st_datainfo[i,1], "_precip", ".csv", sep=""), sep = ",")
  #analisis[,1] <- gsub("-", "/", as.character(analisis[,1]))
  
  
  #posición de primer y last dato
  lugar <- as.character(analisis[,2] == -999)
  lugarmin <- min(which(lugar == "FALSE"))
  lugarmax <- max(which(lugar == "FALSE"))
  
  
  st_datainfo[i,2] <- as.character(analisis[lugarmin,1])
  st_datainfo[i,3] <- as.character(analisis[lugarmax,1])
  
  #analisis <- analisis[lugar:nrow(analisis),] 
  
  #Tmin análisis
  #-------------
  analisis <- read.delim(paste(getwd(), "/Input_Datos/", "st_", st_datainfo[i,1], "_tmin", ".csv", sep=""), sep = ";")
  
  #posición de primer y last dato
  lugar <- as.character(analisis[,2] == -999)
  lugarmin <- min(which(lugar == "FALSE"))
  lugarmax <- max(which(lugar == "FALSE"))
  
  st_datainfo[i,4] <- as.character(analisis[lugarmin,1])
  st_datainfo[i,5] <- as.character(analisis[lugarmax,1])
  
  #Tmax análisis
  #-------------
  analisis <- read.delim(paste(getwd(), "/Input_Datos/", "st_", st_datainfo[i,1], "_tmax", ".csv", sep=""), sep = ";")
  
  #posición de primer y last dato
  lugar <- as.character(analisis[,2] == -999)
  lugarmin <- min(which(lugar == "FALSE"))
  lugarmax <- max(which(lugar == "FALSE"))
  
  st_datainfo[i,6] <- as.character(analisis[lugarmin,1])
  st_datainfo[i,7] <- as.character(analisis[lugarmax,1])
  
}

#Establecimiento de aquellas estaciones que se van a conservar dada su longitud de registro.
#El análisis se hace observando "st_datainfo"
filtro <- c("03802005", "03820004") #esto es manual!!!!, hay que revisar el data


#hasta acá es útil la primera parte
############################################


##############
#SEGUNDA PARTE
##############

#Pegar los archivos en un solo data frame
#----------------------------------------
Estaciones <- list()

for (i in 1:length(st_codes)){
  
  tmin <- read.delim(paste(getwd(), "/Input_Datos/", "st_", st_codes[i], "_tmin", ".csv", sep=""), sep = ";")
  tmin$indicador <- 1:nrow(tmin)
  
  tmax <- read.delim(paste(getwd(), "/Input_Datos/", "st_", st_codes[i], "_tmax", ".csv", sep=""), sep = ";")
  tmax$indicador <- 1:nrow(tmax)
  
  pp <- read.delim(paste(getwd(), "/Input_Datos/", "st_", st_codes[i], "_precip", ".csv", sep=""), sep = ",")
  pp$indicador <- 1:nrow(pp)
  
  #uniendo todos los valores
  #-------------------------
  estacion <- merge(tmin, tmax, all = TRUE, by = "indicador")
  estacion <- merge(estacion, pp, all = TRUE, by = "indicador") #si faltan datos, se completan con NA
  
  estacion <- estacion[,c(2,3,5,7)]
  colnames(estacion) <- c("date", "tmin", "tmax", "pp")
  
  #Período común (de las variables por estación)
  #----------------------------------------------
  #Se busca la fecha en donde las tres variables tienen un INICIO común.
  ini <- max(apply(estacion[2:4], 2, function(x) min(which(x > 0))))
  
  #Se busca la fecha en donde las tres variables tienen un TÉRMINO común.
  fin <- min(apply(estacion[2:4], 2, function(x) max(which(x > 0))))
  
  
  #Cambio de -999 por NA
  estacion[estacion == -999]<-NA
  

  #Finalmente, se recorta la estación.
  #-----------------------------------
  estacion <- estacion[ini:fin,]
  
  Estaciones[[i]] <- estacion
  names(Estaciones)[i] <- st_codes[i]
}


#Fin segunda parte
##################


###############
# TERCERA PARTE
################

#IMPUTE


#Extracción de aquellas estaciones que se van a utilizar
#Impute y tabla resumen con % de NA
#-------------------------------------------------------
ordest <- match(filtro, names(Estaciones))


#data frame donde se almacena la info de NA por estacion y variable
GapsEst <- data.frame(matrix(nrow = length(filtro), ncol = 4))
GapsEst[,1] <- filtro
colnames(GapsEst) <- c("Station", "Tmin", "Tmax", "Pp")


EstFiltroImp <- list()
for (j in 1:length(ordest)){
  
  impest <- data.frame(Estaciones[[ordest[j]]][,1])
  impest$year <- as.numeric(sapply(strsplit(as.character(impest[,1]), "/"), function(x) x[3]))
  impest$yday <- (as.POSIXlt(as.character(impest[,1]), format = "%d/%m/%Y"))$yday + 1
  
  #Seleciono todas las variables que serán imputadas
  #-------------------------------------------------
  datosimp <- Estaciones[[ordest[j]]][,2:4]
  colnames(datosimp) <- c("t_min", "t_max", "precip")
  
  #Agrego info de porcentaje de NA
  #-------------------------------
  GapsEst[j, 2:4]<- apply(datosimp, 2, function(x) sum(is.na(x)/length(x)*100))
  
  #Se realiza el impute
  #--------------------
  datosimp <- missForest(datosimp)
  
  #Extracción de la serie rellenada
  datosimp <- datosimp$ximp
  
  #cbind de impest con datosimp
  impest <- cbind(impest, datosimp)
  
  #solar
  #----
  
  #ingreso de solar al data frame final
  impest$solar <- 8
  
  #date
  #----
  date <- data.frame(as.character(as.POSIXlt(as.character(impest[,1]), format = "%d/%m/%Y")), stringsAsFactors = FALSE)
  
  #ingreso de date al data frame final
  impest <- cbind(impest,date)
  names(impest)[8] <- "date"

  
  #Formato final dat
  #-----------------
  impest <- impest[,2:ncol(impest)]
  
  
  #Almacenamiento en lista
  #-----------------------
  EstFiltroImp[[j]] <- impest
  names(EstFiltroImp[j]) <- filtro[j]
  
  
  #exportar como csv, en carpeta daily
  #-----------------------------------
  write.table(impest, paste(getwd(), "/daily/", filtro[j], "_completo", ".csv", sep ="" ), sep =";")
  
}



