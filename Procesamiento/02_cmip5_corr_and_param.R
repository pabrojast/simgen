###################################################
#
# Correlation between cmip5 models and observed data
#
###################################################

#libraries
#---------
library(XML); 
library(zoo);
library(xts);
library(wq); 
library(plyr); 
library(hydroTSM);

#CONFIG
##Working Dir

workingdir <- 'C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen/'
setwd(workingdir)
##Study area
##----------
##- Limari Basin is established as study area (as an example).
##Study area boundaries

LongMin <- -70.7
LongMax <- -70.2
LatMin <- -28.8
LatMax <- -28.0

#First of all, we need to identify which models ("historical".. and projected) are uploaded to the w3.cdl.cl platform.

#Historical cmip5 models.
#------------------------
#sistema de cache 25-03-2018
if(file.exists("Procesamiento/tmp/cmip5hist_mod.html")){
  cmip5hist_mod <- readHTMLTable("Procesamiento/tmp/cmip5hist_mod.html", header = FALSE)
  cmip5hist_mod <- as.character(cmip5hist_mod[[2]][,1])
}else {
  download.file("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/", destfile = "Procesamiento/tmp/cmip5hist_mod.html")
  cmip5hist_mod <- readHTMLTable("Procesamiento/tmp/cmip5hist_mod.html", header = FALSE)
  cmip5hist_mod <- as.character(cmip5hist_mod[[2]][,1])
}
#sistema de cache 25-03-2018
#Simulated cmip5 models.
if(file.exists("Procesamiento/tmp/cmip5proj_mod.html")){
  cmip5proj_mod <- readHTMLTable("Procesamiento/tmp/cmip5proj_mod.html", header = FALSE)
  cmip5proj_mod <- as.character(cmip5proj_mod[[2]][,1])
}else {
  download.file("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/", destfile = "Procesamiento/tmp/cmip5proj_mod.html")
  cmip5proj_mod <- readHTMLTable("Procesamiento/tmp/cmip5proj_mod.html", header = FALSE)
  cmip5proj_mod <- as.character(cmip5proj_mod[[2]][,1])
}



#Due differences between both database length, an intersection to found commom models should be made.
#Intersection
#------------
cmip5_final <- intersect(cmip5hist_mod, cmip5proj_mod)

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------  
# Download CMIP5 data
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#Historical and projected data.
#------------------------------

#First of all, start and end date must be figured out in order to manage data later.
fecha_modelos <- data.frame(matrix(nrow = length(cmip5_final), ncol = 9))
colnames(fecha_modelos) <- c("modelo", "smes_hist", "syear_hist", "emes_hist", "eyear_hist",
                             "smes_proj", "syear_proj", "emes_proj", "eyear_proj") # s = start, e = end.


for (i in 1:length(cmip5_final)){
  #Ingreso nombre modelo a fecha_modelos
  fecha_modelos[i,1] <- cmip5_final[i]
  print(paste("Retrieving", cmip5_final[i], "date information", sep = " "))
  
  #1 Determinar fecha inicio y termino de modelo
  #--------------------------------------------
  #--------------------------------------------
  
  #1.1 Modelo Historico
  #------------------
  ######modificacion 25-03-2018
  if(file.exists(paste("Procesamiento/tmp/fecha_hist",i,".html",sep=""))){
    fecha_hist <- readHTMLTable(paste("Procesamiento/tmp/fecha_hist",i,".html",sep=""), header = FALSE)
  }else {
    download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/",
                        cmip5_final[i], "/T/first/%28Dec%202005%29RANGE/", sep=""), destfile = paste("Procesamiento/tmp/fecha_hist",i,".html",sep=""))
    fecha_hist <- readHTMLTable(paste("Procesamiento/tmp/fecha_hist",i,".html",sep=""), header = FALSE)
  }
  ######modificacion 25-03-2018
  
  fecha_hist <- data.frame(fecha_hist[3], stringsAsFactors = FALSE)
  fecha_hist <- as.character(fecha_hist[1,2])
  fecha_hist <- strsplit(fecha_hist, " ")
  fecha_hist <- fecha_hist[[1]]
  
  #1.1.1 Ingreso de informaci贸n hist贸rica a fecha_modelos
  #------------------------------------------------------
  #Mes inicio hist贸rico
  fecha_modelos[i,2] <- unlist(strsplit(fecha_hist[8], "[(]"))[2]
  
  #Year inicio hist贸rico
  fecha_modelos[i,3] <- unlist(strsplit(fecha_hist[9], "[)]"))[1]
  
  #Mes fin hist贸rico
  fecha_modelos[i,4] <- unlist(strsplit(fecha_hist[11], "[()]"))[2]
  
  #Year fin hist贸rico
  fecha_modelos[i,5] <- unlist(strsplit(fecha_hist[12], "[)]"))[1]
  
  #------
  
  #1.2 Modelo Proyectado
  #---------------------
  ######modificacion 25-03-2018
  if(file.exists(paste("Procesamiento/tmp/fecha_proj",i,".html",sep=""))){
    fecha_proy <- readHTMLTable(paste("Procesamiento/tmp/fecha_proj",i,".html",sep=""), header = FALSE)
  }else {
    download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/rcp85/.MONTHLY/",
                        cmip5_final[i], sep=""), destfile = paste("Procesamiento/tmp/fecha_proj",i,".html",sep=""))
    fecha_proy <- readHTMLTable(paste("Procesamiento/tmp/fecha_proj",i,".html",sep=""), header = FALSE)
  }
  ###fin de modificacion
  fecha_proy <- data.frame(fecha_proy[3], stringsAsFactors = FALSE)
  fecha_proy <- as.character(fecha_proy[1,2])
  fecha_proy <- strsplit(fecha_proy, " ")
  fecha_proy <- fecha_proy[[1]]
  
  
  #1.2.1 Ingreso de informaci贸n proyectada a fecha_modelos
  #------------------------------------------------------
  #Mes inicio proyectada
  fecha_modelos[i,6] <- unlist(strsplit(fecha_proy[8], "[(]"))[2]
  
  #Year inicio proyectada
  fecha_modelos[i,7] <- unlist(strsplit(fecha_proy[9], "[)]"))[1]
  
  #Mes fin proyectada
  fecha_modelos[i,8] <- unlist(strsplit(fecha_proy[11], "[()]"))[2]
  
  #Year fin proyectada
  fecha_modelos[i,9] <- unlist(strsplit(fecha_proy[12], "[)]"))[1]
  
}# Fin ingreso datos a fecha_modelos.
  
################################################
################################################


#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------
#     Precipitation data download
#     (Historical and Projected)
#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------


#A list is created so that values of T and Pp can be stored
tdppList <- list()

for (j in 1:nrow(fecha_modelos)){
  
  #Temperature Data (mean global)
  #------------------------------
  #------------------------------
  print(paste("Processing", fecha_modelos[j,1], "data", sep = " "))
  
  #Historical Data
  #---------------
  ###########funcion cache################### 25-03-2018
  ######################################################
  if(file.exists(paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""))){
    td_hist <- read.delim(paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""),stringsAsFactors = FALSE)
  }else{
    download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/.",
                        fecha_modelos[j,1],"/.tas/%5BX+Y+%5Daverage/T/first/%28Dec%202005%29RANGE/gridtable.tsv", sep = ""), destfile = paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""))
    td_hist <- read.delim(paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""),stringsAsFactors = FALSE)
  }
  ########fin de modificacion ##############25-03-2018
  
  td_hist <- td_hist[2:nrow(td_hist),]
  td_hist[,2] <- as.numeric(td_hist[,2])
  td_hist <- ts(td_hist[,2], start = c(as.numeric(fecha_modelos[j,3]),as.numeric(match(fecha_modelos[j,2], month.abb))), frequency = 12)    
  
  #td_hist to data frame long table
  td_hist <- ts2df(td_hist)
  td_hist$year <- row.names(td_hist)
  td_hist <- reshape(td_hist, direction="long", varying= list(names(td_hist)[1:12]), v.names = "Temp_Celsius", idvar = c("year"), timevar = "Month", times = 1:12)
  td_hist <- arrange(td_hist, year)
  td_hist <- na.omit(td_hist) #Importante, ya que elimina los NA generado por ts2df.
  
  #Projected Data
  #--------------
  #inicio de modificacion de cache 25-03-2018
  if(file.exists(paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""))){
    td_proj <- read.delim(paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""),stringsAsFactors = FALSE)
  }else{
    download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                        fecha_modelos[j,1],"/.tas/%5BX+Y+%5Daverage/gridtable.tsv", sep = ""), destfile = paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""))
    td_proj <- read.delim(paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""),stringsAsFactors = FALSE)
  }
  ###modificacion de cache
  
  td_proj <- td_proj[2:nrow(td_proj),]
  td_proj[,2] <- as.numeric(td_proj[,2])
  td_proj <- ts(td_proj[,2], start = c(as.numeric(fecha_modelos[j,7]),as.numeric(match(fecha_modelos[j,6], month.abb))), frequency = 12)    
  
  #td_proj to data frame long table
  td_proj <- ts2df(td_proj)
  td_proj$year <- row.names(td_proj)
  td_proj <- reshape(td_proj, direction="long", varying= list(names(td_proj)[1:12]), v.names = "Temp_Celsius", idvar = c("year"), timevar = "Month", times = 1:12)
  td_proj <- arrange(td_proj, year)
  td_proj <- na.omit(td_proj)
  
  
  #Merge both Temperature data sets
  #--------------------------------
  td_merge <- rbind(td_hist, td_proj)
  
  
  #Precipitation Data (Local)
  #--------------------------
  #--------------------------
  
  #Historical Data
  #---------------
  #inicio de modificacion de cache 25-03-2018
  if(file.exists(paste("Procesamiento/tmp/pp_hist",j,".tsv",sep=""))){
    pp_hist <- read.delim(paste("Procesamiento/tmp/pp_hist",j,".tsv",sep=""), header = FALSE)
  }else{
    download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/.",
                        fecha_modelos[j,1],"/.pr/%28bb:",
                        LongMin,":",
                        LatMin,":",
                        LongMax,":",
                        LatMax,
                        ":bb%29geoobject%5BX/Y%5Dweighted-average/T/first/%28Dec%202005%29RANGE/[T]data.tsv",sep = ""), destfile = paste("Procesamiento/tmp/pp_hist",j,".tsv",sep=""))
    pp_hist <- read.delim(paste("Procesamiento/tmp/pp_hist",j,".tsv",sep=""), header = FALSE)
  }
  ###modificacion de cache
  
  pp_hist <- t(pp_hist)*30 #30 dias.
  pp_hist <- ts(pp_hist, start = c(as.numeric(fecha_modelos[j,3]),as.numeric(match(fecha_modelos[j,2], month.abb))), frequency = 12)
  
  #pp_hist to data frame long table
  pp_hist <- ts2df(pp_hist)
  pp_hist$year <- row.names(pp_hist)
  pp_hist <- reshape(pp_hist, direction="long", varying= list(names(pp_hist)[1:12]), v.names = "Pp_mm/month", idvar = c("year"), timevar = "Month", times = 1:12)
  pp_hist <- arrange(pp_hist, year)
  pp_hist <- na.omit(pp_hist)
  

  #Projected Data
  #--------------
  ######################################################
  if(file.exists(paste("Procesamiento/tmp/pp_proj",j,".tsv",sep=""))){
    pp_proj <- read.delim(paste("Procesamiento/tmp/pp_proj",j,".tsv",sep=""), header = FALSE)
  }else{
    download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                        fecha_modelos[j,1],"/.pr/%28bb:",
                        LongMin,":",
                        LatMin,":",
                        LongMax,":",
                        LatMax,
                        ":bb%29geoobject%5BX/Y%5Dweighted-average/[T]data.tsv",sep = ""), destfile = paste("Procesamiento/tmp/pp_proj",j,".tsv",sep=""))
    pp_proj <- read.delim(paste("Procesamiento/tmp/pp_proj",j,".tsv",sep=""), header = FALSE)
  }
  ########fin de modificacion ##############25-03-2018
  
  
  pp_proj <- t(pp_proj)*30
  pp_proj <- ts(pp_proj, start = c(as.numeric(fecha_modelos[j,7]),as.numeric(match(fecha_modelos[j,6], month.abb))), frequency = 12)
  
  #pp_proj to data frame long table
  pp_proj <- ts2df(pp_proj)
  pp_proj$year <- row.names(pp_proj)
  pp_proj <- reshape(pp_proj, direction="long", varying= list(names(pp_proj)[1:12]), v.names = "Pp_mm/month", idvar = c("year"), timevar = "Month", times = 1:12)
  pp_proj <- arrange(pp_proj, year)
  pp_proj <- na.omit(pp_proj)
  
  #Merge Both data sets
  #--------------------
  pp_merge <- rbind(pp_hist, pp_proj)
  
  
  
  
  #Merging Temperature and Precipitation
  #-------------------------------------------------------
  #-------------------------------------------------------
  tdpp <- cbind(td_merge,pp_merge[,3])
  colnames(tdpp)[4] <- "Pp_mm/month"
  
  tdppList[[j]] <- tdpp
  names(tdppList)[j] <- fecha_modelos[j,1]
}

#Fin almacenamiento en Lista.



#--------------------------------------------------------------------------------
################################################################################
#--------------------------------------------------------------------------------
#
#Importar datos de usuario
#
#--------------------------------------------------------------------------------
#################################################################################
#--------------------------------------------------------------------------------

###############################################
#NOTA: UTILIZAR SCRIPT 02_Formateo_Datos_Pais
###############################################

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#
# Correlation between user data and cmip5 data
#
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

#setwd("~/Limari")
archivoscor <- list.files("daily", pattern = ".csv")

Datos_mensuales <- list()

#Ciclo para leer archivos
#------------------------

for (k in 1:length(archivoscor)){
  
  ppmens <- read.delim(paste(getwd(), "/daily/", archivoscor[k], sep=""), sep = ";")
  
  data_mensual <- zoo(ppmens[,5], as.Date(ppmens$date))
  data_mensual <- daily2monthly(data_mensual, FUN = sum)
  
  Datos_mensuales[[k]] <- data_mensual

}

#Lectura de archivos

#gg <- data.frame(Datos_mensuales[[1]])


#####################################333
#
#   Correlaciones
#
####################

#La informaci贸n ser谩 almacenada en un DF resumen.

Correlaciones <- data.frame(matrix(nrow = length(cmip5_final), ncol = length(archivoscor) + 1))
Correlaciones[,1] <- fecha_modelos[,1]

colnames(Correlaciones) <- c("Modelo", archivoscor)

pdf("check cor2.pdf")
#Se calcula la correlaci贸n entre la estaci贸n (datos mensuales) y el modelo (tdpp) y luego se almacena en "Correlaciones"
for (l in 2:ncol(Correlaciones)){
  print(colnames(Correlaciones[l]))
  
  for(m in 1:nrow(Correlaciones)){
    
    Est_mes <- data.frame(Datos_mensuales[l-1])
    fechas <- data.frame(strsplit(c(row.names(Est_mes)), "-"))
    
    fechas_ini <- t(fechas[,1])
    fechas_ini <- c(as.numeric(as.character(fechas_ini[,1])), as.numeric(as.character(fechas_ini[,2])))
    
    fechas_fin <- t(fechas[,ncol(fechas)])
    fechas_fin <- c(as.numeric(as.character(fechas_fin[,1])), as.numeric(as.character(fechas_fin[,2])))
    
    #subset de Datos modelo seg煤n fecha de estaciones
    mm <- tdppList[[m]]
    
    pos_ini <- match(fechas_ini[1], mm[,1])
    pos_fin <- (match(fechas_fin[1] +1, mm[,1]) -1)
    
    mm <- mm[pos_ini:pos_fin,]
    
    #Extraer solo columnas de precipitacion
    mm <- mm[,4]
    
    #Correlacion
    #-----------
    corr <- cor(Est_mes, mm)
    
    #se almacena en archivo Correlaciones
    Correlaciones[m,l] <- as.numeric(corr) 
    namen <- colnames(Correlaciones)
    a駉 <- seq(as.Date(paste(fechas_ini[[1]],"/1/1",sep="")), as.Date(paste(fechas_fin[[1]],"/12/31",sep="")), by = "month")
    st1 <- namen[l]
    st2 <- Correlaciones$Modelo[m]
    st <- paste(st1,st2)
    st <- gsub("csv|formateado|CSV|TXT|txt|csv_|\\.","",st)
    este <-Est_mes$structure.c.0..0..0..13..0..0..0..0..0..0..0..0..0..0..0..0..
    plot(a駉,Est_mes[,1],main = st, type = "l", col = "red", xlab="A駉", ylab="Precipitaci髇 [mm]")
    lines(a駉,  mm, "l",col = "green")
    legend("topright", legend = c(st1,st2), col = 1:2, lty = c(1,1), title="Leyenda" )
    
    
  }
}
dev.off()
write.csv(Correlaciones, "corr2.csv")
#---------------------------------------------
#---------------------------------------------
#  Promedio de Correlaciones y an谩lisis
#-------------------------------------------
#-----------------------------------------------
Prom_corr <- data.frame(cbind(Correlaciones[,1], apply(Correlaciones[,2:ncol(Correlaciones)], 1, FUN = mean)))
Prom_corr <- Prom_corr[order(Prom_corr[,2], decreasing = TRUE),]

#Definir umbral (Se descartan aquellos modelos con correlaciones menores al umbral)
#----------------------------------------------------------------------------------
umbral = 0.30

#Modelos descartados seg煤n umbral
#--------------------------------
descarte <- which(as.numeric(as.character(Prom_corr[,2])) < umbral)
descarte <- as.character(Prom_corr[descarte,1])

#Se obtiene el conjunto de modelos que ser谩 empleado para determinar sd y mean, modelos originales menos los con correlacion bajo el umbral
cmip5_total <- setdiff(cmip5_final, descarte)

################################################
#
#Funci贸n para calcular average & mean, de T y Pp
################################################
################################################
cmip5dataDL <- function(modelo,mesini,mesfin){
  
  #Temperature Data (mean global)
  #------------------------------
  td <- read.delim(paste("http://www.climatedatalibrary.cl/expert/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                         cmip5_total[modelo],"/.tas/%5BX+Y+%5Daverage/gridtable.tsv", sep = ""),stringsAsFactors = FALSE)
  
  td <- td[2:nrow(td),]
  td[,2] <- as.numeric(td[,2])
  td <- ts(td[,2], start = c(2006,1), frequency = 12)
  td <- window(td, start = c(2006,1), end = c(2099,12), frequency = 12)
  
  #td to data frame
  td <- ts2df(td)
  td$year <- row.names(td)
  
  #Se hace el subset.
  td <- cbind(td[,13], td[,mesini:mesfin])
  
  #promedio temperatura anual
  td <- cbind(td[,1],data.frame(rowMeans(td[,2:ncol(td)])))
  colnames(td) <- c("Year","T_Average")
  
  #td <- reshape(td, direction="long", varying= list(names(td)[1:12]), v.names = "Temp_Celsius", idvar = c("year"), timevar = "Month", times = 1:12)
  #td <- arrange(td, year)
  
  
  #Precipitation Data (Local)
  #--------------------------
  pp <- read.delim(paste("http://www.climatedatalibrary.cl/expert/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",cmip5_total[modelo],"/.pr/X/%28",
                         LongMin,"%29%28",
                         LongMax,"%29RANGEEDGES/Y/%28",
                         LatMin,"%29%28",
                         LatMax,"%29RANGEEDGES%5BX/Y%5Daverage/gridtable.tsv",sep = ""), header = FALSE)
  
  pp <- as.numeric(as.character(pp[3:nrow(pp),2]))
  
  #pp <- t(pp)#*30 #30 dias.                
  pp <- ts(pp, start = c(2006,1), frequency = 12)
  pp <- window(pp, start = c(2006,1), end = c(2099,12), frequency = 12)
  
  #pp to data frame
  pp <- ts2df(pp)
  pp$year <- row.names(pp)
  
  #Se hace el subset.
  pp <- cbind(pp[,13], pp[,mesini:mesfin])
  
  #promedio temperatura anual
  pp <- cbind(pp[,1],data.frame(rowSums(pp[,2:ncol(pp)])))
  colnames(pp) <- c("Year","Pp_Sum")
  
  
  #Merge
  #------
  
  #Merging both data frames(Temperature and Precipitation)....
  tdpp <- cbind(td,pp[,2])
  colnames(tdpp)[3] <- "Pp_Sum"
  
  return(tdpp)
  
}#Fin Funci贸n
########################################################
########################################################


#Almacenamiento en Lista
#-----------------------
#A list is created so that values of T and Pp can be stored
tdppList_2 <- list() #almacena informaci贸n anual.

for (i in 1:length(cmip5_total)){
  
  print(cmip5_total[i])
  
  tdppList_2[[i]] <- cmip5dataDL(i,1,12) #i, porque el loop se encarga de reemplazarlo (1..length(cmipmodels))
  names(tdppList_2)[i] <- cmip5_total[i]
  
}#Fin almacenamiento en Lista.


##################################################
#
#   pr_historical = b0 + b1*T_global
#
###########################################

#Creaci贸n de un DF donde se almacenar谩n los valores,
Coef <- data.frame(matrix(nrow = length(tdppList_2), ncol = 4))
colnames(Coef) <- c("CMIP5_Model", "b0", "b1", "r2")

Coef2 <- data.frame(matrix(nrow = length(tdppList_2), ncol = 4))
colnames(Coef2) <- c("CMIP5_Model", "b0", "b1", "r2")

#Almacenamiento de coeficientes
#------------------------------
for (j in 1:length(tdppList_2)){
  betas <- tdppList_2[[j]]
  #modificado por pablo, 03-08-2017:
  betas <- data.frame((betas[,2] ), log(betas[,3]))
  
  #Pueden presentarse Infinitos al calcular logaritmos (ej 0), luego, hay que removerlos.
  betas[betas[,2] == -Inf,] <- NA
  colnames(betas) <- c("T_Anomaly", "Log_Pp")
  betas <- lm(betas[,2] ~ betas[,1])
  
  #Se almacena en el data frame
  Coef[j,1] <- names(tdppList_2)[j]
  Coef[j,2] <- betas[[1]][2] #slope, expresado como porcentaje
  Coef[j,3] <- betas[[1]][1] #intercepto
  Coef[j,4] <- summary(betas)$r.squared #Coef of determination
  #modificado por pablo 03-08-2017
  
  scatter <- tdppList_2[[j]]
  scatter2 <- scatter[,c(1,2)]
  scatter3 <- ts(scatter2[,2]-mean(scatter2[,2]), start = c(2006,1))
  tempbetas<- lm(scatter2[,2] ~ as.numeric(scatter2[,1]))
  slope <- signif(tempbetas[[1]][2], digits = 4)
  
  Coef2[j,1] <- names(tdppList_2)[j]
  Coef2[j,2] <- slope #slope, expresado como porcentaje
  Coef2[j,3] <- tempbetas[[1]][1] #intercepto
  Coef2[j,4] <- summary(tempbetas)$r.squared #Coef of determination
  
}#Fin almacenamiento.
##########################################################


#Secci贸n exportar a PDF
#######################
#######################

#PDF con gr谩fico de:

#log Pp v/s anomal铆a Temp
#Pp v/s tiempo

pdf("Cmip5 scatterplots.pdf")

for(k in 1:length(tdppList_2)){
  layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
  
  scatter <- tdppList_2[[k]]
  scatter1 <- data.frame((scatter[,2] - mean(scatter[,2])), log(scatter[,3])) 
  plot(scatter1[,1], scatter1[,2], main = names(tdppList_2)[[k]], xlab = "T_Anomaly", ylab = "Log(Pp)", type = "p" ) #debe ser punto, porque no hay orden l贸gico.
  abline(lm(scatter1[,2] ~ scatter1[,1]))
  mtext(paste("b0 = ", signif(Coef[k,2], digits = 3), "%", "        ", "r2 = ",  signif(Coef[k,4], digits = 4)), side = 3, line = 0)
  
  #scatter <- tdppList_2[[k]]
  scatter2 <- scatter[,c(1,3)]
  scatter3 <- ts(scatter2[,2], start = c(2006,1))
  plot(scatter3, main = names(tdppList_2)[[k]], xlab = "Year", ylab = "Pp(mm/day)", type = "l")
  mtext(paste("slope", signif(lm(scatter2[,2] ~ as.numeric(scatter2[,1]))[[1]][2], digits = 4), side = 3))
  #abline(lm(scatter2[,2] ~ as.numeric(scatter2[,1])))
  
  
}
dev.off() #fin export pdf



#Histograma final
pdf("Histogram.pdf")

hist(Coef[,2], xlab = "% change per degree")

dev.off()


####################################
#
#  mean & s.d.
#
##############################
print("Precipitacion:")
Parameters <- list()
Parameters$mean <- mean(Coef[,2])
Parameters$sd <- sd(Coef[,2])
pdf("PP_.pdf")
hist(Coef[,2], xlab = "% change per degree", main="Precipitacion")
mtext(paste("mean: ", signif(Parameters$mean,digits = 3)," sd:",signif(Parameters$sd,digits = 3)),3)
dev.off()

print(Parameters)
##################################################################
print("Temperatura:")
pdf("T_.pdf")
hist(Coef2[,2], xlab = "% change per degree", main="Temperatura")
Parameters <- list()
Parameters$mean <- mean(Coef2[,2]) 
Parameters$sd <- sd(Coef2[,2])
mtext(paste("mean: ", signif(Parameters$mean,digits = 3)," sd:",signif(Parameters$sd,digits = 3)),3)
print(Parameters)
dev.off()