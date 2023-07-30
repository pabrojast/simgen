#############PACKAGES###################################
#install.packages(stringr,dep=TRUE)
#instala y/o llama paquetes
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)){
      #try to install the package from the local file
      if(x == "dse"){
        install.packages("Rpackage/dse_2015.12-1.zip", repos = NULL, type="source")
      }
      if(x == "missForest"){
        install.packages("Rpackage/missForest_1.4.zip", repos = NULL, type="source")
      }
      if(x == "seas"){
        install.packages("Rpackage/seas_0.5-2.zip", repos = NULL, type="source")
      }
      if(x == "wq"){
        install.packages("Rpackage/wq_0.4.8.tar.gz", repos = NULL, type="source")
      }
    }
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
cargar_librerias<- function(){
  pkgTest("missForest");
  pkgTest("XML"); 
  pkgTest("zoo");
  pkgTest("xts");
  pkgTest("knitr");
  pkgTest("wq"); 
  pkgTest("plyr"); 
  pkgTest("hydroTSM");
  pkgTest("lattice");
  pkgTest("dse");
  pkgTest("ggplot2");
  pkgTest("seas");
  pkgTest("date");
  pkgTest("corrplot");
  pkgTest("ggpubr");
  pkgTest("rstudioapi");
}

#setwd("C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen")

#######esta funcion reemplaza el archivo 01_Formateo_Datos_Pais.R

formateo_datos_pais <- function(workingdirectory = "" ) {
  #setwd(workingdirectory)
  #archivos_pais en directorio
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
}

#############segundo script#################

corrcmip5models <- function(LongMin = -70.7, LongMax = -70.2, LatMin = -28.8,LatMax = -28.0, umbral = 0.30, clearcache = "NO") {
  
  if (clearcache == "YES") {
    clear.cache <- dir(path="Procesamiento/tmp", pattern="tsv|html|htm", full.names=TRUE) 
    file.remove(clear.cache)
    print("Cache eliminado")
  }else{
    print("Se mantiene el cache")
  }
  ###################################################
  #
  # Correlation between cmip5 models and observed data
  #
  ###################################################
  #LongMin <- -70.7
  #LongMax <- -70.2
  #LatMin <- -28.8
  #LatMax <- -28.0
  #libraries
  #---------

  

  ##Study area
  ##----------
  ##- Limari Basin is established as study area (as an example).
  ##Study area boundaries
  

  
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
    
    #1.1.1 Ingreso de información histórica a fecha_modelos
    #------------------------------------------------------
    #Mes inicio histórico
    fecha_modelos[i,2] <- unlist(strsplit(fecha_hist[8], "[(]"))[2]
    
    #Year inicio histórico
    fecha_modelos[i,3] <- unlist(strsplit(fecha_hist[9], "[)]"))[1]
    
    #Mes fin histórico
    fecha_modelos[i,4] <- unlist(strsplit(fecha_hist[11], "[()]"))[2]
    
    #Year fin histórico
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
    
    
    #1.2.1 Ingreso de información proyectada a fecha_modelos
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
  #LongMin = 18; LongMax = 32; LatMin = -29; LatMax = -16; umbral = 0.2; clearcache = "NO";
  
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
      td_hist <- read.table(paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""),stringsAsFactors = FALSE, skip = 2)
    }else{
      download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/.",
                          fecha_modelos[j,1],"/.tas/%5BX+Y+%5Daverage/T/first/%28Dec%202005%29RANGE/gridtable.tsv", sep = ""), destfile = paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""))
      td_hist <- read.table(paste("Procesamiento/tmp/gridtable",j,".tsv",sep=""),stringsAsFactors = FALSE, skip = 2)
    }
    ########fin de modificacion ##############25-03-2018
    ## 29-07-2023
    #para poder leer el archivo elimine las primeras dos lineas
    #ahora las agregare como dummy ya que se borran despues
    #td_hist <- rbind(td_hist[1:2,], td_hist)
    
    #td_hist <- td_hist[2:nrow(td_hist),]
    #rownames(td_hist) <- 1:nrow(td_hist)
    
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
      td_proj <- read.table(paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""),stringsAsFactors = FALSE,skip=2)
    }else{
      download.file(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                          fecha_modelos[j,1],"/.tas/%5BX+Y+%5Daverage/gridtable.tsv", sep = ""), destfile = paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""))
      td_proj <- read.table(paste("Procesamiento/tmp/gridtable_proj",j,".tsv",sep=""),stringsAsFactors = FALSE, skip=2)
    }
    ###modificacion de cache
    #ahora las agregare como dummy ya que se borran despues
    #td_proj <- rbind(td_proj[1:2,], td_proj)
    ##
    
    #td_proj <- td_proj[2:nrow(td_proj),]
    #rownames(td_proj) <- 1:nrow(td_proj)
    
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
  
  #La información será almacenada en un DF resumen.
  
  Correlaciones <- data.frame(matrix(nrow = length(cmip5_final), ncol = length(archivoscor) + 1))
  Correlaciones[,1] <- fecha_modelos[,1]
  
  colnames(Correlaciones) <- c("Modelo", archivoscor)
  
  pdf("Procesamiento/output/correlation_graphs.pdf")
  #Se calcula la correlación entre la estación (datos mensuales) y el modelo (tdpp) y luego se almacena en "Correlaciones"
  for (l in 2:ncol(Correlaciones)){
    print(colnames(Correlaciones[l]))
    
    for(m in 1:nrow(Correlaciones)){
      
      Est_mes <- data.frame(Datos_mensuales[l-1])
      fechas <- data.frame(strsplit(c(row.names(Est_mes)), "-"))
      
      fechas_ini <- t(fechas[,1])
      fechas_ini <- c(as.numeric(as.character(fechas_ini[,1])), as.numeric(as.character(fechas_ini[,2])))
      
      fechas_fin <- t(fechas[,ncol(fechas)])
      fechas_fin <- c(as.numeric(as.character(fechas_fin[,1])), as.numeric(as.character(fechas_fin[,2])))
      
      #subset de Datos modelo según fecha de estaciones
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
      anio <- seq(as.Date(paste(fechas_ini[[1]],"/1/1",sep="")), as.Date(paste(fechas_fin[[1]],"/12/31",sep="")), by = "month")
      st1 <- namen[l]
      st2 <- Correlaciones$Modelo[m]
      st <- paste("Station:",st1,"Model: ",st2, sep=" ")
      st <- gsub("csv|formateado|CSV|TXT|txt|csv_|\\.","",st)
      st1 <- gsub("csv|formateado|CSV|TXT|txt|csv_|\\.","",st1)
      este <-Est_mes$structure.c.0..0..0..13..0..0..0..0..0..0..0..0..0..0..0..0..
      plot(anio,Est_mes[,1],main = st, type = "l", col = "red", xlab="Year", ylab="Precipitation [mm]")
      #solo para hacer nuevo grafico
      #write.csv(Est_mes[,1], file = paste("Procesamiento/output/datos_de_correlacion/",st1,".csv",sep=""),row.names=FALSE, na="")
      #write.csv(mm, file = paste("Procesamiento/output/datos_de_correlacion/",st2,".csv",sep=""),row.names=FALSE, na="")
      #write.csv(anio, file="Procesamiento/output/datos_de_correlacion/anho.csv", sep="",row.names=FALSE, na="")
      #
      #
      lines(anio,  mm, "l",col = "green")
      legend("topright", legend = c(st1,st2), col = c("red","green"), lty = c(1,1), bty = "n" )
      #legend("topright", inset=c(-0.2,0), legend=c("A","B"), pch=c(1,3), title="Group")
    }
  }
  dev.off()
  
  
  
  #Se calcula la correlación entre la estación (datos mensuales) y el modelo (tdpp) y luego se almacena en "Correlaciones"
  listadeplot <- list()
  contador <- 0
  for (l in 2:ncol(Correlaciones)){
    print(colnames(Correlaciones[l]))
    
    for(m in 1:nrow(Correlaciones)){
      contador <- contador+1
      Est_mes <- data.frame(Datos_mensuales[l-1])
      fechas <- data.frame(strsplit(c(row.names(Est_mes)), "-"))
      
      fechas_ini <- t(fechas[,1])
      fechas_ini <- c(as.numeric(as.character(fechas_ini[,1])), as.numeric(as.character(fechas_ini[,2])))
      
      fechas_fin <- t(fechas[,ncol(fechas)])
      fechas_fin <- c(as.numeric(as.character(fechas_fin[,1])), as.numeric(as.character(fechas_fin[,2])))
      
      #subset de Datos modelo según fecha de estaciones
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
      anio <- seq(as.Date(paste(fechas_ini[[1]],"/1/1",sep="")), as.Date(paste(fechas_fin[[1]],"/12/31",sep="")), by = "month")
      st1 <- namen[l]
      st2 <- Correlaciones$Modelo[m]
      st <- paste("Station:",st1,"Model: ",st2, sep=" ")
      st <- gsub("csv|formateado|CSV|TXT|txt|csv_|\\.","",st)
      st1 <- gsub("csv|formateado|CSV|TXT|txt|csv_|\\.","",st1)
      este <-Est_mes$structure.c.0..0..0..13..0..0..0..0..0..0..0..0..0..0..0..0..
      #plot(anio,Est_mes[,1],main = st, type = "l", col = "red", xlab="Year", ylab="Precipitation [mm]")
      #lines(anio,  mm, "l",col = "green")
      dataa = data.frame(xx=Est_mes[,1], yy=mm)
      
      listadeplot[[contador]] <- ggscatter(dataa, x = "xx", y = "yy", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = paste(st1,"Dga Station"), ylab = paste(st2,"Model"))

      #legend("topright", legend = c(st1,st2), col = c("red","green"), lty = c(1,1), bty = "n" )
      #legend("topright", inset=c(-0.2,0), legend=c("A","B"), pch=c(1,3), title="Group")
    }
  }
  
  #pdf("Procesamiento/output/correlation_graphs_v2.1.pdf",onefile = TRUE)
  #for(i in listadeplot){
  #  print(i)
  #}
  #dev.off()
  
  #ggsave("Procesamiento/output/correlation_graphs_v2.1.pdf",listadeplot, width = 4, height = 4)
  
  
  write.csv(Correlaciones, "Procesamiento/output/correlation_table.csv")
  
  #modificacion de ploteo 04-20-2019, se agrega corrplot en carga de liberias
  #pdf("Procesamiento/output/correlation_graphs_v2.pdf")
  #print(Correlaciones)
  #corrplot(Correlaciones, method = "circle")
  #https://stackoverflow.com/questions/50631646/saving-a-correlation-matrix-graphic-as-pdf
  #dev.off()
  #---------------------------------------------
  #---------------------------------------------
  #  Promedio de Correlaciones y análisis
  #-------------------------------------------
  #-----------------------------------------------
  Prom_corr <- data.frame(cbind(Correlaciones[,1], apply(Correlaciones[,2:ncol(Correlaciones)], 1, FUN = mean)))
  Prom_corr <- Prom_corr[order(Prom_corr[,2], decreasing = TRUE),]
  
  #Definir umbral (Se descartan aquellos modelos con correlaciones menores al umbral)
  #----------------------------------------------------------------------------------
  #umbral = 0.30
  
  #Modelos descartados según umbral
  #--------------------------------
  descarte <- which(as.numeric(as.character(Prom_corr[,2])) < umbral)
  descarte <- as.character(Prom_corr[descarte,1])
  
  #Se obtiene el conjunto de modelos que será empleado para determinar sd y mean, modelos originales menos los con correlacion bajo el umbral
  cmip5_total <- setdiff(cmip5_final, descarte)
  
  ################################################
  #
  #Función para calcular average & mean, de T y Pp
  ################################################
  ################################################
  cmip5dataDL <- function(modelo,mesini,mesfin){
    
    #Temperature Data (mean global)
    #------------------------------
    #inicio de modificacion de cache 25-03-2018
    if(file.exists(paste("Procesamiento/tmp/cmip5dataDL_",cmip5_total[modelo],".tsv",sep=""))){
      td <- read.delim(paste("Procesamiento/tmp/cmip5dataDL_",cmip5_total[modelo],".tsv",sep=""),stringsAsFactors = FALSE)
    }else{
      download.file(paste("http://www.climatedatalibrary.cl/expert/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                          cmip5_total[modelo],"/.tas/%5BX+Y+%5Daverage/gridtable.tsv", sep = ""), destfile = paste("Procesamiento/tmp/cmip5dataDL_",cmip5_total[modelo],".tsv",sep=""))
      td <- read.delim(paste("Procesamiento/tmp/cmip5dataDL_",cmip5_total[modelo],".tsv",sep=""),stringsAsFactors = FALSE)
    }
    ###modificacion de cache
    
    
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
    #inicio de modificacion de cache 25-03-2018
    if(file.exists(paste("Procesamiento/tmp/cmip5dataDLpp_",cmip5_total[modelo],".tsv",sep=""))){
      pp <- read.delim(paste("Procesamiento/tmp/cmip5dataDLpp_",cmip5_total[modelo],".tsv",sep=""), header = FALSE)
    }else{
      download.file(paste("http://www.climatedatalibrary.cl/expert/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",cmip5_total[modelo],"/.pr/X/%28",
                          LongMin,"%29%28",
                          LongMax,"%29RANGEEDGES/Y/%28",
                          LatMin,"%29%28",
                          LatMax,"%29RANGEEDGES%5BX/Y%5Daverage/gridtable.tsv",sep = ""), destfile = paste("Procesamiento/tmp/cmip5dataDLpp_",cmip5_total[modelo],".tsv",sep=""))
      pp <- read.delim(paste("Procesamiento/tmp/cmip5dataDLpp_",cmip5_total[modelo],".tsv",sep=""), header = FALSE)
    }
    ###modificacion de cache
  
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
    
  }#Fin Función
  ########################################################
  ########################################################
  
  
  #Almacenamiento en Lista
  #-----------------------
  #A list is created so that values of T and Pp can be stored
  tdppList_2 <- list() #almacena información anual.
  
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
  
  #Creación de un DF donde se almacenarán los valores,
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
  
  
  #Sección exportar a PDF
  #######################
  #######################
  
  #PDF con gráfico de:
  
  #log Pp v/s anomal Temp
  #Pp v/s tiempo
  
  pdf("Procesamiento/output/cmip5_scatterplots.pdf")
  
  for(k in 1:length(tdppList_2)){
    layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
    
    scatter <- tdppList_2[[k]]
    scatter1 <- data.frame((scatter[,2] - mean(scatter[,2])), log(scatter[,3])) 
    plot(scatter1[,1], scatter1[,2], main = names(tdppList_2)[[k]], xlab = "T_Anomaly", ylab = "Log(Pp)", type = "p" ) #debe ser punto, porque no hay orden lógico.
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
  #pdf("Procesamiento/output/Histogram.pdf")
  #hist(Coef[,2], xlab = "% change per degree")
  #dev.off()
  
  
  ####################################
  #
  #  mean & s.d.
  #
  ##############################
  
  print("Distribution of precipitation trends from the GCMs")
  Parameters <- list()
  Parameters$mean <- mean(Coef[,2])
  Parameters$sd <- sd(Coef[,2])
  pdf("Procesamiento/output/Distribution_of_precipitation.pdf", width=7, height=4)
  hist(Coef[,2], xlab = "Fractional change of precipitation per ?C change of global temperature", main="Distribution of precipitation trends from the GCMs", ylab="Frequency (# of GCMs)")
  mtext(paste("mean: ", signif(Parameters$mean,digits = 3)," sd:",signif(Parameters$sd,digits = 3)),3)
  dev.off()
  
  print(Parameters)
  ##################################################################
  print("Temperature")
  pdf("Procesamiento/output/Distribution_of_Temperature.pdf", width=7, height=4)
  hist(Coef2[,2], xlab = "Change per Degree", main="Temperature")
  Parameters <- list()
  Parameters$mean <- mean(Coef2[,2]) 
  Parameters$sd <- sd(Coef2[,2])
  mtext(paste("mean: ", signif(Parameters$mean,digits = 3)," sd:",signif(Parameters$sd,digits = 3)),3)
  print(Parameters)
  dev.off()
}

##tercer script###
###preparacion de datos###
preparacion_datos <- function(codigo.estacion.inicial = 2000 ) {
  ## Modificado Pablo Rojas, 04-08-2017, fix: write table con problemas.
  ### Script para transformar los datos al formato ACRU
  ## Cargo las librers necesarias ###########################################################

  
  #library(lattice); library(dse) ; library(ggplot2); library(seas); library(date)
  #############################################################################################
  # Preparación de datos para la media regional
  # En este caso uso de ejemplo las estaciones de Argentina y Uruguay
  #############################################################################################
  ### Leo los archivos diarios modificados (imputados)
  # seteo el directorio de trabajo
  #setwd("/media/sf_Compartido/simgen")
  #setwd("C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen")
  files <- list.files("daily", pattern=".csv")
  # debe comenzar en 1000 para mantener los 4 digitos
  #codigo.estacion.inicial <- 2000 
  
  ### calculo el periodo en que coinciden todas las estaciones
  inilist <- vector(length=length(files))
  endlist <- vector(length=length(files))
  n <-1
  for (eachfile in files) {   # eachfile="RATROBS.st"
    dat <- read.delim(paste("daily/", eachfile, sep=""), sep =";")
    temp <- subset(dat, dat$year==min(dat$year))
    inilist[n] <- ifelse(length(temp$year) <=364, min(dat$year)+1, min(dat$year))
    temp <- subset(dat, dat$year==max(dat$year))
    endlist[n] <- ifelse(length(temp$year) <=364, max(dat$year)-1, max(dat$year))
    n <- n+1
  }
  
  year_ini <- max(inilist)
  year_end <- min(endlist)
  
  for (eachfile in files) {  # eachfile="RATROBS.st"                            
    dat <- read.delim(paste("daily/", eachfile, sep=""), sep =";")
    dat <- subset(dat, dat$year >= year_ini & dat$year <= year_end)
    dat$cod <- "00000007"  
    dat$datetex <- as.character(dat$date)
    dat$mes <- substr(dat$datetex, 6,7)
    dat$dia <- substr(dat$datetex, 9,10)
    dat$col1 <- paste(dat$cod, dat$year, dat$mes, dat$dia, sep="")
    dat$dummy <- "   4.9 26.33 95.00 47.00 138.2"
    dat$precip <- as.numeric(dat$precip)
    rain <- format(dat$precip, digits = 3, nsmall=1, justify = c("right"))
    tmax <- format(dat$t_max, digits = 3, trim=FALSE, justify = c("right"), width=5)
    tmin <- format(dat$t_min, digits = 3, nsmall=1, justify = c("right"), width=5)
    acru <- cbind(dat$col1, rain, tmax, tmin, dat$dummy)
    #### Anualizacion Lluvia ##########           
    datos.rain <- data.frame(cbind(dat$year, dat$precip))
    datos.rain.na <- na.omit(datos.rain)
    names(datos.rain.na) <- c("year", "rain")
    rain.anual <- aggregate(datos.rain.na$rain, by = list(datos.rain.na$year), FUN = "sum", na.omit=TRUE)
    names(rain.anual) <- c("year", "rain")
    ## TMAX ###
    datos.tmax <- data.frame(cbind(dat$year, dat$t_max))
    datos.tmax.na <- na.omit(datos.tmax)
    names(datos.tmax.na) <- c("year", "tmax")
    tmax.anual <- aggregate(datos.tmax.na$tmax, by = list(datos.tmax.na$year), FUN = "mean", na.omit=TRUE)
    names(tmax.anual) <- c("year", "tmax")
    ## TMIN  ##
    datos.tmin <- data.frame(cbind(dat$year, dat$t_min))
    datos.tmin.na <- na.omit(datos.tmin)
    names(datos.tmin.na) <- c("year", "tmin")
    tmin.anual <- aggregate(datos.tmin.na$tmin, by = list(datos.tmin.na$year), FUN = "mean", na.omit=TRUE)
    names(tmin.anual) <- c("year", "tmin")
    ## junto todo
    names(rain.anual) <- c("year", "rain")
    datos.anual <- merge(rain.anual, tmin.anual)
    datos.anual2 <- merge(datos.anual, tmax.anual)
    #### los archivos de datos observados deben llamarse "obshis_XXXX.txt" donde XXXX son 4 digitos
    #### Creo directorio ACRU y guardo lista de estaciones, acru y anuales
    dir.create(file.path("acru"), showWarnings = FALSE)
    filesal.acru <- paste("acru/",  "obshis_", codigo.estacion.inicial, ".txt" ,sep="")
    filesal.anual <- paste("acru/", eachfile, "_ANUAL.TXT", sep="")
    filesal.anual2 <- paste("acru/", "TODOS_ANUAL.TXT2", sep="")
    codigos <- data.frame(eachfile, codigo.estacion.inicial)
    names(codigos) <- c("estacion", "codigo_obshist")
    write.table(acru, filesal.acru, quote = FALSE, col.names=FALSE, row.names=FALSE, sep=" ")
    write.table(datos.anual2, filesal.anual, quote = FALSE, col.names=FALSE, row.names=FALSE, sep=" " )
    write.table(codigos, "acru/codigos.txt", append=TRUE,col.names=FALSE, row.names=FALSE, sep=" " )
    datos.anual2$estacion <- eachfile
    write.table(datos.anual2, filesal.anual2, append= TRUE, quote = FALSE, col.names=FALSE, row.names=FALSE, sep=" " )
    codigo.estacion.inicial <- codigo.estacion.inicial+1
  }
  
  ### Calculo la media regional, leo el archivo TODOS_ANUAL.TXT2
  
  dat <- read.table("acru/TODOS_ANUAL.TXT2")
  dat$V5 <- NULL
  names(dat) <- c("year", "rain", "tmin", "tmax")
  media.regional <- aggregate(dat, by = list(dat$year), FUN = "mean", na.omit=TRUE)
  
  media.regional.print <- media.regional[,-(1:2)]
  write.table(media.regional.print, "acru/media_regional.txt", col.names=FALSE, row.names=FALSE )
  
  media.regional.dia <- media.regional.print
  media.regional.dia$rain <- media.regional$rain/365
  write.table(media.regional.dia, "acru/media_regional_dia.txt", col.names=FALSE, row.names=FALSE  )
  
  ##############################################################################################
  # para usar el paquete DSE los datos deben estar "detrended", A Greene recomendo lineal,
  # tambien se podria usar el método de la descomposicion teniendo en cuenta la temp o el ozono.
  #### remuevo la tendencia con la temp global
  ##############################################################################################
  periodo <- 8                                              ## Per?odo m?nimo a retener
  icone=1                                                   ## tipo de relleno serie
  iconb=1                                                   ## tipo de relleno serie
  year <- seq(1901, 2095, by=1)
  tsm <- read.table("daily/serie_cmip5.txt", quote="\"")
  trend <- cbind(year, tsm)
  names(trend) <- c("year", "tsm")
  data <- merge(media.regional, trend)
  ## CALCULO DE LA VARIABILIDAD DE LARGO PLAZO (CC)
  ## Calculo parametro de correlacion Chi-Sq - CORR Pearson (Serie TSM : Serie Obs) * SD(obs)/SD(TSM)
  base <- data.frame(na.omit(cbind(data$year, data$tsm, data$rain/365)))  
  names(base) <- c("year", "tsm", "rain")                 
  corr <- cor.test (base$tsm, base$rain)
  coef <- corr$estimate * sd(base$rain)/sd(base$tsm)
  # calculo de la serie con media de cc TrendCC = Media(obs) + K * TSM(i)-Media(TSM)
  resultado <- data.frame(base$year, base$rain)
  names(resultado) <- c("year", "rain")
  resultado$trendccM <- round(mean(base$rain) + coef * (base$tsm-(mean(base$tsm))),2)
  resultado$trendcc <- round(resultado$trendccM - mean(base$rain),2)
  resultado$detrendcc <- round(base$rain - resultado$trendccM,2)
  resultado.rain <- data.frame(resultado, "rain")
  ## Figura 3 del paper para tmin en este ejemplo
  pdf("Procesamiento/output/fig3pp.pdf")
  plot(resultado$year, resultado$rain, type="p")
  lines(resultado$year, resultado$trendccM, col="red")
  xyplot(detrendcc ~ year, data = resultado, type=c("l", "p"))
  
  detrended <- data.frame(resultado$detrendcc)
  names(detrended) <- c("rain")
  dev.off() 
  ## Ahora hago lo mismo pero para tmax
  pdf("Procesamiento/output/fig3tmax.pdf")
  base <- data.frame(na.omit(cbind(data$year, data$tsm, data$tmax)))                            
  names(base) <- c("year", "tsm", "tmax")                 
  corr <- cor.test (base$tsm, base$tmax)
  coef <- corr$estimate * sd(base$tmax)/sd(base$tsm)
  resultado <- data.frame(base$year, base$tmax)
  names(resultado) <- c("year", "tmax")
  resultado$trendccM <- round(mean(base$tmax) + coef * (base$tsm-(mean(base$tsm))),2)
  resultado$trendcc <- round(resultado$trendccM - mean(base$tmax),2)
  resultado$detrendcc <- round(base$tmax - resultado$trendccM,2)
  
  plot(resultado$year, resultado$tmax, type="l")
  lines(resultado$year, resultado$trendccM, col="red")
  xyplot(detrendcc ~ year, data = resultado, type=c("l","p"))
  detrended$tmax <- resultado$detrendcc  
  resultado.tmax <- data.frame(resultado, "tmax")
  dev.off() 
  ## Ahora hago lo mismo pero para tmin
  pdf("Procesamiento/output/fig3tmin.pdf")
  
  base <- data.frame(na.omit(cbind(data$year, data$tsm, data$tmin)))                            
  names(base) <- c("year", "tsm", "tmin")                 
  corr <- cor.test (base$tsm, base$tmin)
  coef <- corr$estimate * sd(base$tmin)/sd(base$tsm)
  resultado <- data.frame(base$year, base$tmin)
  names(resultado) <- c("year", "tmin")
  resultado$trendccM <- round(mean(base$tmin) + coef * (base$tsm-(mean(base$tsm))),2)
  resultado$trendcc <- round(resultado$trendccM - mean(base$tmin),2)
  resultado$detrendcc <- round(base$tmin - resultado$trendccM,2)
  plot(resultado$year, resultado$tmin, type="l")
  lines(resultado$year, resultado$trendccM, col="red")
  
  
  xyplot(detrendcc ~ year, data = resultado, type=c("l", "p"))
  detrended$tmin <- resultado$detrendcc
  resultado.tmin <- data.frame(resultado, "tmin")
  dev.off()
  
  write.table(detrended, "acru/media_regional_dia_detrended.txt", col.names=FALSE, row.names=FALSE)
  # Armo la figura 3 del paper
  names(resultado.rain) <-c("year", "value", "trendccM", "trendcc", "detrendcc", "var")
  names(resultado.tmax) <-c("year", "value", "trendccM", "trendcc", "detrendcc", "var")
  names(resultado.tmin) <-c("year", "value", "trendccM", "trendcc", "detrendcc", "var")
  pdf("Procesamiento/output/tmax-tmin-rain.pdf")
  datgraf <- rbind(resultado.rain, resultado.tmax, resultado.tmin)
  q <- ggplot(datgraf, aes(year, value))  +geom_line()
  q <- q + geom_line(aes(year, trendccM), colour="red") + facet_grid(var ~ . , scales = "free", space = "free" ) + theme_classic()
  print(q)
  dev.off()
  ################################
  # Figura 7 del paper
  pdf("Procesamiento/output/fig7.pdf")
    plot(detrended)
  dev.off()
  ################################
  ## Ahora debo aplicar el paquete DSE a la serie sin tendencia
  ## primero estimo el modelo con estVARXls y se genera la serie con simulate
  ## 
  pdf("Procesamiento/output/dse.pdf")
  ts <- TSdata(output= detrended)
  seriesNamesOutput(ts) <- c("Rain","Tmax", "Tmin")
  acf(ts) # los datos no parecen autocorrelacionados
  
  model01 <- estVARXls(ts, max.lag=1)
  model02 <- estVARXls(ts, max.lag=2)
  model03 <- estVARXls(ts, max.lag=3)
  model04 <- estVARXls(ts, max.lag=4)
  model05 <- estVARXls(ts, max.lag=5)
  model06 <- estVARXls(ts, max.lag=6)
  model07 <- estVARXls(ts, max.lag=7)
  #model08 <- estVARXls(ts, max.lag=8)
  #revisar
  #model09 <- estVARXls(ts, max.lag=9)
  #model10 <- estVARXls(ts, max.lag=9)
  
  #informationTests(model01, model02, model03, model04, model05, model06, model07, model08, model09, model10)
  informationTests(model01, model02, model03, model04, model05, model06, model07)
  
  checkResiduals(model01)
  # genero la long.seq con el modelo lag(1) ___________ revisar start 
  long.seq <- simulate(model01, start=c(1973,1), freq=1,sampleT=100000)
  # calculo las correlaciones en los detrended(obs) y los simulados
  cor(detrended)
  cor(long.seq$output[])
  dev.off()
  # escribe archivo de salida de long.seq
  write(t(long.seq$output), ncolumns=3, file="acru/long_seq.txt")
  
  #cambio automático de archivos de directorio
  #-------------------------------------------
  #-------------------------------------------
  
  
  #Carpeta /acru (archivos obshis) a carpeta /obs
  #---------------------------------------------
  arch <- list.files("acru", pattern = "obshis_")
  
  for (x in 1:length(arch)){
    obs <- read.delim(paste(getwd(),"/acru/", arch[x], sep = ""), header = FALSE)
    write.table(obs, paste(getwd(), "/obs/", arch[x], sep = ""), col.names=FALSE, row.names=FALSE, quote = FALSE)  
  }
  
  #Carpeta /acru (archivos media_regional_dia) a carpeta /obs
  #----------------------------------------------------------
  arch <- list.files("acru", pattern = "media_regional_dia")
  nomb <- c("obsav.dat", "obsavdt.dat")
  y<-NULL
  for (y in 1:length(arch)){
    print(y)
    obs <- read.delim(paste(getwd(),"/acru/",arch[y], sep = ""), header = FALSE)
    write.table(obs,paste(getwd(), "/dat/",nomb[y],sep=""), col.names=FALSE, row.names=FALSE, quote = FALSE)
  }
  
  #Carpeta /acru (archivo long_seq) a carpeta /input_sim
  #-----------------------------------------------------
  arch <- list.files("acru", pattern = "long_seq")
  nomb <- "sim_100kyr.dat"
  
  obs <- read.delim(paste(getwd(),"/acru/",arch, sep = ""))
  write.table(obs, paste(getwd(), "/input_sim/", nomb, sep =""), col.names=FALSE, row.names=FALSE, quote = FALSE)
  
  ######################################################
  #####################################################
  #
  #     FIN FIN FIN 
  #
  ############
}

remover_archivos_antiguos <- function(directorio = c("output_sim","Procesamiento_output","obs","acru","daily","Input_Datos","input_sim"), patterns = "txt") {
  for (directorio.unitario in directorio) {
    print(paste("Deleting... ",directorio.unitario))
    if(directorio.unitario == "daily"){
      junk <- dir(path=directorio.unitario, pattern="csv", full.names=TRUE) 
      file.remove(junk)
    }
    else if(directorio.unitario == "input_sim"){
      if(file.exists("input_sim/sim_100kyr.dat")){
      file.remove("input_sim/sim_100kyr.dat")
      }
      else{
        print("sim_100kyr.dat no existe...ok")
      }
    }
    else if(directorio.unitario == "acru"){
      junk <- dir(path=directorio.unitario, pattern="TXT|txt", full.names=TRUE) 
      file.remove(junk)
    }
    else if(directorio.unitario == "Input_Datos"){
      junk <- dir(path="Input_Datos", pattern="csv|xlsx", full.names=TRUE) 
      file.remove(junk)
    }
    else if(directorio.unitario == "output_sim"){
      junk <- dir(path="output_sim", pattern="csv|txt|TXT|pdf|PDF", full.names=TRUE,recursive = TRUE )
      file.remove(junk)
    }
    else if(directorio.unitario == "Procesamiento_output"){
      junk <- dir(path="Procesamiento/output", pattern="csv|txt|TXT|PDF|pdf|Pdf", full.names=TRUE,recursive = TRUE )
      file.remove(junk)
    }
    else{
      junk <- dir(path=directorio.unitario, pattern=patterns, full.names=TRUE) 
      file.remove(junk)
    }
  }
}
