###################################
## Script final adaptaci�n datos PAis a simgen.
############################################
library(missForest)

######################
#Config
######################

#archivos_pais en directorio
setwd("C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen")
archivos_pais <- list.files("Input_Datos", pattern = ".csv") 


##############
#
#PRIMERA PARTE - ANÁLISIS DE DATOS.
#
##############
#Esta primera parte es útil para determinar qué estaciones se van
# a utilizar, haciendo un análisis subjetivo y visual.
#--------------------------------------------------------


#Indicar fecha donde aparece el primer dato observado (de T y Pp)
#---------------------------------------------------------------

#crear un Data Frame que almacene la información de cada estación (fecha en que aparece el primer valor)
st_datainfo <- data.frame(matrix(nrow = length(archivos_pais), ncol = 7))
colnames(st_datainfo) <- c("station", "precip_first", "precip_last", "tmin_first", "tmin_last", "tmax_first", "tmax_last")
st_datainfo$station <- archivos_pais

for (i in 1:nrow(st_datainfo)){
  
  #Data analisys
  #-------------
  analisis <- read.delim(paste(getwd(), "/Input_Datos/", st_datainfo[i,1], sep=""), sep = ";")
  
  #Posición primer y last dato meteo
  #---------------------------------
  
  #Pp
  #--
  lugar <- as.character(!is.na(analisis[,4]))
  lugarmin <- min(which(lugar == "FALSE"))
  lugarmax <- max(which(lugar == "FALSE"))
  
  st_datainfo[i,2] <- ifelse(is.infinite(lugarmin), "Completo",
                             as.character(paste(analisis[lugarmin,1],"-", analisis[lugarmin,2],"-", analisis[lugarmin,3],sep = "")))
  
  st_datainfo[i,3] <- ifelse(is.infinite(lugarmax), "Completo",
                             as.character(paste(analisis[lugarmax,1],"-", analisis[lugarmax,2],"-", analisis[lugarmax,3],sep = "")))

  #Tmax
  #----
  lugar <- as.character(!is.na(analisis[,5]))
  lugarmin <- min(which(lugar == "FALSE"))
  lugarmax <- max(which(lugar == "FALSE"))
  
  st_datainfo[i,4] <- ifelse(is.infinite(lugarmin), "Completo",
                             as.character(paste(analisis[lugarmin,1],"-", analisis[lugarmin,2],"-", analisis[lugarmin,3],sep = "")))
  
  st_datainfo[i,5] <- ifelse(is.infinite(lugarmax), "Completo",
                             as.character(paste(analisis[lugarmax,1],"-", analisis[lugarmax,2],"-", analisis[lugarmax,3],sep = "")))
  
  #Tmin
  #----
  lugar <- as.character(!is.na(analisis[,6]))
  lugarmin <- min(which(lugar == "FALSE"))
  lugarmax <- max(which(lugar == "FALSE"))
  
  st_datainfo[i,6] <- ifelse(is.infinite(lugarmin), "Completo",
                             as.character(paste(analisis[lugarmin,1],"-", analisis[lugarmin,2],"-", analisis[lugarmin,3],sep = "")))
  
  st_datainfo[i,7] <- ifelse(is.infinite(lugarmax), "Completo",
                             as.character(paste(analisis[lugarmax,1],"-", analisis[lugarmax,2],"-", analisis[lugarmax,3],sep = "")))

}


######################################################
#
#
#    REALIZAR ANÁLISIS VISUAL Y EDITAR LOS ARCHIVOS SEGÚN st_datainfo
#
#   Editado los archivos, correr nuevamente el script desde la fila 1.
#######################################################################



###############
#
# SEGUNDA PARTE - IMPUTE
#
################


#Extracción de aquellas estaciones que se van a utilizar
#Impute y tabla resumen con % de NA
#-------------------------------------------------------

#data frame donde se almacena la info de NA por estacion y variable
GapsEst <- data.frame(matrix(nrow = length(archivos_pais), ncol = 4))
GapsEst[,1] <- archivos_pais
colnames(GapsEst) <- c("Station", "Pp", "Tmax", "Tmin")


#EstFiltroImp <- list()
for (j in 1:length(archivos_pais)){
  
  print(paste("procesando", archivos_pais[j]))
  
  stat_for <- read.delim(paste(getwd(), "/Input_Datos/", archivos_pais[j], sep =""), sep = ";") #station formato
  
  #Porcentaje de Gaps
  #------------------
  GapsEst[j, 2:4]<- apply(stat_for[4:6], 2, function(x) sum(is.na(x)/length(x)*100))
  
  
  #impute
  #------
  stat_imp <- missForest(stat_for[,4:6])
  stat_imp <- stat_imp$ximp
  
  #modificar aquellos valores que pudiesen resultar negativos
  stat_imp[stat_imp < 0] <- 0
  colnames(stat_imp) <- c("precip", "t_max", "t_min")
  
  #date
  date <- paste(stat_for[,1], "-", sprintf("%02d", stat_for[,2]), "-", sprintf("%02d", stat_for[,3]), sep = "")
  
  #yday
  yday <- (as.POSIXlt(as.character(date), format = "%Y-%m-%d"))$yday + 1
  
  
  #Armado de formato Final
  #-----------------------
  formato <- data.frame("year" = stat_for$year, "yday" = yday, "t_min" = stat_imp$t_min, 
                        "t_max" = stat_imp$t_max, "precip" = stat_imp$precip, "solar" = 8, "date" = date)
  
  #Se Exporta
  #----------
  #Pablo: modificado 10-08-2017, se agrega row.names = FALSE
  write.table(formato, paste(getwd(), "/daily/", archivos_pais[j], "_formateado", ".csv", sep ="" ), sep =";",row.names = FALSE)
  
}
