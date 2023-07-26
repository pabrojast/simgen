##########################
#
#  Datos MAISA
#
####################

##############
#PRIMERA PARTE
##############

#Esta primera parte es útil para determinar qué estaciones se van
# a utilizar, haciendo un análisis subjetivo y visual.
#--------------------------------------------------------

#Archivos en directorio
archivos <- list.files("Input_Datos", pattern = ".txt") 


#####################################
#
#   Formato Simgen
#
####################################

for (j in 1:length(archivos)){
  Estacion <- read.delim(paste(getwd(), "/Input_Datos/", archivos[j], sep=""), sep = "\t")
  
  date <- paste(Estacion[,1], "-", sprintf("%02d",Estacion[,2]), "-", sprintf("%02d", Estacion[,3]), sep ="")
  yday <- as.POSIXlt(date, format = "%Y-%m-%d")$yday + 1
  
  #Armado data frame
  #-----------------
  arm <-data.frame(Estacion[,1], yday, sprintf("%.1f",Estacion[,6]), sprintf("%.1f",Estacion[,5]), sprintf("%.1f",Estacion[,4]), solar = 8, date)
  colnames(arm) <- c("year", "yday", "t_min", "t_max", "precip", "solar", "date")
  
  write.table(arm, paste(getwd(), "/daily/", archivos[j], "_completo", ".csv", sep ="" ), sep =";")
  
}










