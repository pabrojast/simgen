#######################
#
# Correlation between cmip5 models and observed data
#
###################################################

#libraries
#---------
library(XML); library(wq); library(plyr); library(zoo); library(hydroTSM)
require(tcltk); library(stringr); library(missForest)

mydialog <- function(){
  LongMin <- tclVar(-70.7)
  LongMax <- tclVar(-70.2)
  LatMin <- tclVar(-28.8)
  LatMax <- tclVar(-28.0)
  wd <- tclVar(getwd())
  tt <- tktoplevel()
  tkwm.title(tt,"CMIP5 CORRELATION")
  wd.entry <- tkentry(tt, textvariable=wd)
  LongMin.entry <- tkentry(tt, textvariable=LongMin)
  LongMax.entry <- tkentry(tt, textvariable=LongMax)
  LatMin.entry <- tkentry(tt, textvariable=LatMin)
  LatMax.entry <- tkentry(tt, textvariable=LatMax)
  
  reset <- function() {
    tclvalue(LongMin)<-""
    tclvalue(LongMax)<-""
    tclvalue(LatMin)<-""
    tclvalue(LatMax)<-""
    tclvalue(wd)<-""
  }
  
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit <- function() {
    workingdir = tclvalue(wd)
    #########################################################
    setwd(workingdir)
    #First of all, we need to identify which models ("historical".. and projected) are uploaded to the w3.cdl.cl platform.
    #Historical cmip5 models.
    #------------------------
    cmip5hist_mod <- readHTMLTable("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/", header = FALSE)
    #cambiado de cmip5hist_mod <- as.character(cmip5hist_mod[[7]][,1])
    cmip5hist_mod <- as.character(cmip5hist_mod[[2]][,1])
    #Simulated cmip5 models.
    #------------------------
    cmip5proj_mod <- readHTMLTable("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/", header = FALSE)
    #cambiado de cmip5proj_mod <- as.character(cmip5proj_mod[[7]][,1])
    cmip5proj_mod <- as.character(cmip5proj_mod[[2]][,1])
    
    
    #Due differences between both database length, an intersection to found commom models should be made.
    
    #Intersection
    #------------
    cmip5_final <- intersect(cmip5hist_mod, cmip5proj_mod)
    
    
    #Study area
    #----------
    #- Huasco Basin is established as study area (as an example).
    
    #Study area boundaries
    
    LongMin <- as.numeric(tclvalue(LongMin))
    LongMax <- as.numeric(tclvalue(LongMax))
    LatMin <- as.numeric(tclvalue(LatMin))
    LatMax <- as.numeric(tclvalue(LatMax))
    
    
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
      fecha_hist <- readHTMLTable(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/",
                                        cmip5_final[i], "/T/first/%28Dec%202005%29RANGE/", sep=""), header = FALSE)
      fecha_hist <- data.frame(fecha_hist[3], stringsAsFactors = FALSE)
      fecha_hist <- as.character(fecha_hist[1,2])
      fecha_hist <- strsplit(fecha_hist, " ")
      fecha_hist <- fecha_hist[[1]]
      
      #1.1.1 Ingreso de informaciÃ³n histÃ³rica a fecha_modelos
      #------------------------------------------------------
      #Mes inicio histÃ³rico
      fecha_modelos[i,2] <- unlist(strsplit(fecha_hist[8], "[(]"))[2]
      
      #Year inicio histÃ³rico
      fecha_modelos[i,3] <- unlist(strsplit(fecha_hist[9], "[)]"))[1]
      
      #Mes fin histÃ³rico
      fecha_modelos[i,4] <- unlist(strsplit(fecha_hist[11], "[()]"))[2]
      
      #Year fin histÃ³rico
      fecha_modelos[i,5] <- unlist(strsplit(fecha_hist[12], "[)]"))[1]
      
      #------
      
      #1.2 Modelo Proyectado
      #---------------------
      fecha_proy <- readHTMLTable(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/rcp85/.MONTHLY/",
                                        cmip5_final[i], sep=""), header = FALSE)
      fecha_proy <- data.frame(fecha_proy[3], stringsAsFactors = FALSE)
      fecha_proy <- as.character(fecha_proy[1,2])
      fecha_proy <- strsplit(fecha_proy, " ")
      fecha_proy <- fecha_proy[[1]]
      
      
      #1.2.1 Ingreso de informaciÃ³n proyectada a fecha_modelos
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
      td_hist <- read.delim(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/.",
                                  fecha_modelos[j,1],"/.tas/%5BX+Y+%5Daverage/T/first/%28Dec%202005%29RANGE/gridtable.tsv", sep = ""),stringsAsFactors = FALSE)
      
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
      td_proj <- read.delim(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                                  fecha_modelos[j,1],"/.tas/%5BX+Y+%5Daverage/gridtable.tsv", sep = ""),stringsAsFactors = FALSE)
      
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
      pp_hist <- read.delim(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.Historical/.MONTHLY/.",
                                  fecha_modelos[j,1],"/.pr/%28bb:",
                                  LongMin,":",
                                  LatMin,":",
                                  LongMax,":",
                                  LatMax,
                                  ":bb%29geoobject%5BX/Y%5Dweighted-average/T/first/%28Dec%202005%29RANGE/[T]data.tsv",sep = ""), header = FALSE)
      
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
      pp_proj <- read.delim(paste("http://www.climatedatalibrary.cl/SOURCES/.WCRP/.CMIP5/.rcp85/.MONTHLY/.",
                                  fecha_modelos[j,1],"/.pr/%28bb:",
                                  LongMin,":",
                                  LatMin,":",
                                  LongMax,":",
                                  LatMax,
                                  ":bb%29geoobject%5BX/Y%5Dweighted-average/[T]data.tsv",sep = ""), header = FALSE)
      
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
    }#Fin almacenamiento en Lista.
    
    
    
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
    
    archivoscor <- list.files("daily", pattern = ".csv")
    
    Datos_mensuales <- list()
    
    #Ciclo para leer archivos
    #------------------------
    
    for (k in 1:length(archivoscor)){
      
      ppmens <- read.delim(paste(getwd(), "/daily/", archivoscor[k], sep=""), sep = ";")
      
      data_mensual <- zoo(ppmens[,5], as.Date(ppmens$date))
      data_mensual <- daily2monthly(data_mensual, FUN = sum)
      
      Datos_mensuales[[k]] <- data_mensual
      names(Datos_mensuales) <- archivoscor[k]
      
    }
    
    #Lectura de archivos
    
    #gg <- data.frame(Datos_mensuales[[1]])
    
    
    #####################################333
    #
    #   Correlaciones
    #
    ####################
    
    #La informaciÃ³n serÃ¡ almacenada en un DF resumen.
    
    Correlaciones <- data.frame(matrix(nrow = length(cmip5_final), ncol = length(archivoscor) + 1))
    Correlaciones[,1] <- fecha_modelos[,1]
    
    colnames(Correlaciones) <- c("Modelo", archivoscor)
    
    #Se calcula la correlaciÃ³n entre la estaciÃ³n y el modelo y luego se almacena en "Correlaciones"
    for (l in 2:ncol(Correlaciones)){
      print(colnames(Correlaciones[l]))
      
      for(m in 1:nrow(Correlaciones)){
        
        Est_mes <- data.frame(Datos_mensuales[l-1])
        fechas <- data.frame(strsplit(c(row.names(Est_mes)), "-"))
        
        fechas_ini <- t(fechas[,1])
        fechas_ini <- c(as.numeric(as.character(fechas_ini[,1])), as.numeric(as.character(fechas_ini[,2])))
        
        fechas_fin <- t(fechas[,ncol(fechas)])
        fechas_fin <- c(as.numeric(as.character(fechas_fin[,1])), as.numeric(as.character(fechas_fin[,2])))
        
        #subset de Datos modelo segÃºn fecha de estaciones
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
        
      }
    }
    
    #---------------------------------------------
    #---------------------------------------------
    #  Promedio de Correlaciones y anÃ¡lisis
    #-------------------------------------------
    #-----------------------------------------------
    Prom_corr <- data.frame(cbind(Correlaciones[,1], apply(Correlaciones[,2:ncol(Correlaciones)], 1, FUN = mean)))
    Prom_corr <- Prom_corr[order(Prom_corr[,2], decreasing = TRUE),]
    
    #Definir umbral (Se descartan aquellos modelos con correlaciones menores al umbral)
    #----------------------------------------------------------------------------------
    umbral = 0.15
    
    #Modelos descartados segÃºn umbral
    #--------------------------------
    descarte <- which(as.numeric(as.character(Prom_corr[,2])) < umbral)
    descarte <- as.character(Prom_corr[descarte,1])
    
    #Se obtiene el conjunto de modelos que serÃ¡ empleado para determinar sd y mean
    cmip5_total <- setdiff(cmip5_final, descarte)
    
    ################################################
    #
    #FunciÃ³n para calcular average & mean, de T y Pp
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
      
    }#Fin FunciÃ³n
    ########################################################
    ########################################################
    
    
    #Almacenamiento en Lista
    #-----------------------
    #A list is created so that values of T and Pp can be stored
    tdppList_2 <- list() #almacena informaciÃ³n anual.
    
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
    
    #CreaciÃ³n de un DF donde se almacenarÃ¡n los valores,
    Coef <- data.frame(matrix(nrow = length(tdppList_2), ncol = 4))
    colnames(Coef) <- c("CMIP5_Model", "b0", "b1", "r2")
    
    
    #Almacenamiento de coeficientes
    #------------------------------
    for (j in 1:length(tdppList_2)){
      betas <- tdppList_2[[j]]
      betas <- data.frame((betas[,2] - mean(betas[,2])), log(betas[,3]))
      
      #Pueden presentarse Infinitos al calcular logaritmos (ej 0), luego, hay que removerlos.
      betas[betas[,2] == -Inf,] <- NA
      colnames(betas) <- c("T_Anomaly", "Log_Pp")
      betas <- lm(betas[,2] ~ betas[,1])
      
      #Se almacena en el data frame
      Coef[j,1] <- names(tdppList_2)[j]
      Coef[j,2] <- betas[[1]][2]*100 #slope, expresado como porcentaje
      Coef[j,3] <- betas[[1]][1] #intercepto
      Coef[j,4] <- summary(betas)$r.squared #Coef of determination
      
    }#Fin almacenamiento.
    ##########################################################
    
    
    #SecciÃ³n exportar a PDF
    #######################
    #######################
    
    #PDF con grÃ¡fico de:
    
    #log Pp v/s anomalÃ�a Temp
    #Pp v/s tiempo
    
    pdf("Cmip5 scatterplots.pdf")
    
    for(k in 1:length(tdppList_2)){
      layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
      
      scatter <- tdppList_2[[k]]
      scatter1 <- data.frame((scatter[,2] - mean(scatter[,2])), log(scatter[,3])) 
      plot(scatter1[,1], scatter1[,2], main = names(tdppList_2)[[k]], xlab = "T_Anomaly", ylab = "Log(Pp)", type = "p" ) #debe ser punto, porque no hay orden lÃ³gico.
      abline(lm(scatter1[,2] ~ scatter1[,1]))
      mtext(paste("b0 = ", signif(Coef[k,2], digits = 3), "%", "        ", "r2 = ",  signif(Coef[k,4], digits = 4)), side = 3, line = 0)
      
      
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
    
    Parameters <- list()
    Parameters$mean <- mean(Coef[,2]) 
    Parameters$sd <- sd(Coef[,2])
    
    print(Parameters)
    ##################################################################
    } #fin de funcion
  rellenar <- function() {
    ###################################
    #
    # Script final adaptaciÃ³n datos PAÃS a Simgen.
    #
    ###########################################
    workingdir = tclvalue(wd)
    #########################################################
    setwd(workingdir)
    ##############
    #
    #PRIMERA PARTE - ANÃLISIS DE DATOS.
    #
    ##############
    
    #Esta primera parte es Ãºtil para determinar quÃ© estaciones se van
    # a utilizar, haciendo un anÃ¡lisis subjetivo y visual.
    #--------------------------------------------------------
    
    #archivos_pais en directorio
    archivos_pais <- list.files("Input_Datos", pattern = ".csv") 
    
    #Indicar fecha donde aparece el primer dato observado (de T y Pp)
    #---------------------------------------------------------------
    
    #crear un Data Frame que almacene la informaciÃ³n de cada estaciÃ³n (fecha en que aparece el primer valor)
    st_datainfo <- data.frame(matrix(nrow = length(archivos_pais), ncol = 7))
    colnames(st_datainfo) <- c("station", "precip_first", "precip_last", "tmin_first", "tmin_last", "tmax_first", "tmax_last")
    st_datainfo$station <- archivos_pais
    
    
    for (i in 1:nrow(st_datainfo)){
      
      #Data analisys
      #-------------
      analisis <- read.delim(paste(getwd(), "/Input_Datos/", st_datainfo[i,1], sep=""), sep = ";")
      
      #PosiciÃ³n primer y last dato meteo
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
    #    REALIZAR ANÃLISIS VISUAL Y EDITAR LOS ARCHIVOS SEGÃN st_datainfo
    #
    #   Editado los archivos, correr nuevamente el script desde la fila 1.
    #######################################################################
    
    
    
    ###############
    #
    # SEGUNDA PARTE - IMPUTE
    #
    ################
    
    
    #ExtracciÃ³n de aquellas estaciones que se van a utilizar
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
      write.table(formato, paste(getwd(), "/daily/", archivos_pais[j], "_formateado", ".csv", sep ="" ), sep =";")
    }
    msgBox <- tkmessageBox(title = "Anuncio",
                           message = "Terminó el relleno de Datos", icon = "info", type = "ok")
  }
  submit.but <- tkbutton(tt, text="2. Calcular", command=submit)
  rellenar.but <- tkbutton(tt, text="1. Rellenar", command=rellenar)
  tkgrid(tklabel(tt,text="Directorio"), wd.entry, pady= 10, padx= 10)
  tkgrid(tklabel(tt,text="Opciones"),columnspan=3, pady = 10)
  tkgrid(tklabel(tt,text="Long Min"), LongMin.entry, pady= 10, padx= 10)
  tkgrid(tklabel(tt,text="Long Máx"), LongMax.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="Lat Min"), LatMin.entry, pady= 10, padx= 10)
  tkgrid(tklabel(tt,text="Lat Máx"), LatMax.entry, pady= 5, padx= 10)
  tkgrid(rellenar.but, submit.but, reset.but, pady= 10, padx= 10)
}
mydialog()