#genero el archivo con cdo
#cdo mergetime tmin.????.nc tmin.nc
#cdo sellonlatbox,-180,180,-90,90 tmin.nc tmin.nc
library(raster)
library(ncdf4)

#path to id
path <- "C:/Users/pablo/OneDrive/Escritorio/CAZALAC/ADA/BWA/"
#path to tmp folder
pathpreproc <- "D:/tmp2/"
#metadata
stations_metadata <- read.csv(paste0(path,"BaseDatosEstaciones.csv"))


#generate csv tmin
tmin_nc <- brick("D:/tmp/tmin_vf.nc")
pts <- cbind(stations_metadata$lon,stations_metadata$lat) # gives your long/lat point
extractedData <- extract(tmin_nc,pts,method = "bilinear")
dummy <- t(extractedData)
colnames(dummy) <- stations_metadata$id_station
write.csv(dummy, file=paste0(pathpreproc,"_tmin.csv"))



#generate csv tmax
tmin_nc <- brick("D:/tmp/tmax_vf.nc")
pts <- cbind(stations_metadata$lon,stations_metadata$lat) # gives your long/lat point
extractedData <- extract(tmin_nc,pts,method = "bilinear")
dummy <- t(extractedData)
colnames(dummy) <- stations_metadata$id_station
write.csv(dummy, file=paste0(pathpreproc,"_tmax.csv"))

#opt0 generate pp
database_creation_simgen <- function(model = "CRU", country = "BWA",  maxstations=10000, resol="25") {
  clip_method="Shape"
  # BLOCK I.A. DATABASE CONSTRUCTION FROM CRU 3.21 ------------
  # debug: model = "CHIRPS"; country = "BWA"; clip_method="Rectangle"; resol=25
  if(model=="CRU"){
    # Optional Config Setup, Country Code, Shape and raster creation =================
    workdir <- here()
    setwd(workdir)
    print(workdir)
    # Open CRU nc file --------------------
    nc <- nc_open("cru_ts3.24.01.1901.2015.pre.dat.nc")
    print(nc)
    
    # Extract variables ====================
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time")
    time.index=as.Date(as.numeric(time),origin="1900-01-01")
    pre = list()
    pre$x = ncvar_get(nc, "lon")
    pre$y = ncvar_get(nc, "lat")
    pre$z = ncvar_get(nc, "pre", start=c(300, 90, 829), count=c(213, 183, 552))#1970 a 2015, lon=-30.25 to 75.75 lat=-45.25 to 45.75
    # Just for Africa
    xmin=lon[300];xmax=lon[300+213];ymin=lat[90];ymax=lat[90+183]
    #New version
    #xmin=min(lon);xmax=max(lon);ymin=min(lat);ymax=max(lat)
    
    # Create rasterstack with precipitation layers ==================
    pre.layers<-raster::stack()
    for (i in 1:dim(pre$z)[3]){
      r.pre=flip(raster(t(pre$z[,,i])),direction='y')
      extent(r.pre)=c(xmin,xmax,ymin,ymax)
      crs(r.pre) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      pre.layers<-stack(pre.layers,r.pre)
    }
    plot(pre.layers)
    
    #Crop and mask rasterstack with continent boundaries ==========
    dummyreturn <- shape_country(country)
    Boundaries <- dummyreturn$Boundaries
    sps <- dummyreturn$sps
    
    rm(dummyreturn)
    #BoundariesAFR=readOGR("AfricaDA.shp") #from http://www.maplibrary.org/library/stacks/Africa/index.htm
    if(clip_method == "Shape"){
      pre.monthly.AFR=mask(pre.layers,Boundaries)
      pre.monthly.AFR=crop(pre.monthly.AFR,Boundaries)
      
    } else {
      pre.monthly.AFR=mask(pre.layers,polygons(sps))
      pre.monthly.AFR=crop(pre.monthly.AFR,polygons(sps))
    }
    
    
    plot(pre.monthly.AFR[[1]])
    class(pre.monthly.AFR)
    #    writeRaster(pre.monthly.AFR, 'testo.tif')
    # Rasterstack time series creation with monthly data  ================
    d= time.index[seq(829,length.out=dim(pre$z)[3])]
    nlayers(pre.layers)==length(d)
    nlayers(pre.monthly.AFR)==length(d)
    pre.l.ts<-rts(pre.monthly.AFR,d)
    
    #Rasterstack time series creation with annual data ===================
    pre.anual <- apply.yearly(pre.l.ts, sum)
    plot(mean(pre.anual@raster))
    #pre.anual.2=mask(pre.anual[[1]],polygons(Boundaries))
    #pre.anual.2=crop(pre.anual.2,polygons(Boundaries))
    
    
    # DataBase creation for a given country -------------------------
    #Create working directory and set working directory for a given country
    dir.create(paste0("./",country))# Only the first time and if it has not been created
    setwd(paste0("./",country))
    
    #writeOGR(Boundaries, dsn = '.', layer = country, driver = "ESRI Shapefile",overwrite_layer=TRUE)
    ## S4 method for signature 'Spatial'
    shapefile(Boundaries, filename=country, overwrite=TRUE)
    #plot(Boundaries)
    #rm(Boundaries)
    #.....................
    # manual correction of country's shapefile in SAGA GIS and then save it to the folder
    #.....................
    
    plot(Boundaries)
    
    if(clip_method == "Shape"){
      country.rts=mask(pre.monthly.AFR,polygons(Boundaries))
      country.rts=crop(country.rts,polygons(Boundaries))
    } else {
      country.rts=mask(pre.monthly.AFR,polygons(sps))
      country.rts=crop(country.rts,polygons(sps))
    }
    
    country.rts.monthly=rts(country.rts,d)
    country.rts.anual <- apply.yearly(country.rts.monthly, sum)
    plot(mean(country.rts.anual@raster))
    
    #Here a maximum number of stations is defined ======================================
    
    useful.pixels=length(na.omit(getValues(country.rts[[1]])))
    if(useful.pixels<maxstations) size=useful.pixels else size=maxstations
    
    rs=sampleRandom(country.rts[[1]], cells=TRUE,xy=TRUE,size=size,sp=TRUE)
    points(rs,col="red")
    plot(rs)
    #writeOGR(obj=rs, dsn=getwd(),layer="randomsample", driver="ESRI Shapefile",overwrite_layer=TRUE)
    shapefile(rs, filename="randomsample", overwrite=TRUE)
    
    write.csv(rs,"randomsample.csv")
    
    BaseDatosEstaciones=data.frame(id_station=paste0("station_",1:size),
                                   lon=data.frame(rs)[,2],
                                   lat=data.frame(rs)[,3])
    
    #Here the Stations DataBase ("BaseDatosEstaciones.csv") is saved into the working directory
    write.csv(BaseDatosEstaciones,"BaseDatosEstaciones.csv",row.names = FALSE)
    #write.csv(BaseDatosEstaciones,"BaseDatosEstacionesBackup.csv",row.names = FALSE)
    
    #Records DataBase Generation ("BaseDatosRegistros.csv") for a given country
    ppDB=raster::extract(country.rts,data.frame(rs)[,2:3])
    #ppDB.DF <- data.frame(matrix(unlist(ppDB), nrow=size, byrow=F),stringsAsFactors=FALSE)
    ppDB.DF2 <- t(data.frame(matrix(unlist(ppDB), nrow=size, byrow=F),stringsAsFactors=FALSE))
    ppDB.DF2.ts=ts(ppDB.DF2,start=c(1970,1),end=c(2015,12),frequency = 12)
    
    BaseDatosRegistros=data.frame(matrix(ppDB.DF2.ts[,1],ncol=12,byrow=TRUE))
    #If there is more than one station, the binding is performed. 
    #Otherwise, it is omitted to avoid an out of bounds error.
    if(dim(BaseDatosEstaciones)[1]>1){
      for(i in 2:dim(BaseDatosEstaciones)[1]){
        temp.df=data.frame(matrix(ppDB.DF2.ts[,i],ncol=12,byrow=TRUE))
        BaseDatosRegistros=rbind(BaseDatosRegistros,temp.df)
      }
    } else {
      message("Warning: Only 1 station selected in CRU")
    }
    colnames(BaseDatosRegistros)[1:12]=month.abb
    BaseDatosRegistros$id_station=rep(BaseDatosEstaciones$id_station,each=46)
    BaseDatosRegistros$Year=rep(1970:2015,dim(BaseDatosEstaciones)[1])
    BaseDatosRegistros=BaseDatosRegistros[,c(13,14,1:12)]
    
    
    # Zero values are corrected
    BaseDatosRegistros <- zero_correction(BaseDatosRegistros)
    
    #Here the Records DataBase ("BaseDatosRegistros.csv") is saved into the working directory
    write.csv(BaseDatosRegistros,"BaseDatosRegistros.csv",row.names = FALSE)
    #write.csv(BaseDatosRegistros,"BaseDatosRegistrosBackup.csv",row.names = FALSE)
    #................................................. END ...........................................................
    setwd(workdir)
    
  }
  else if(model=="CHIRPS"){
    
    # BLOCK I.B. DATABASE CONSTRUCTION FROM CHIRPS ------------------
    #AFRICAN DROUGHT ATLAS CHIRPS
    workdir <- "D:/tmp2/"
    setwd(workdir)
    print(workdir)
    
    
    # Listado de paises segun codigo ISO
    ISO.codes <- read.csv("CountryISOCodes.csv",sep=";")
    Afr.country.list <- as.character(ISO.codes$ThreeLetter)
    Afr.country.name <- countrycode(Afr.country.list, "iso3c","country.name")
    
    
    shape_return <- shape_country(country)
    Boundaries <- shape_return$Boundaries
    sps <- shape_return$sps
    rm(shape_return)
    
    #Create working directory and set working directory
    dir.create(paste0("./",country,sep=""))
    setwd(paste0("./",country,sep=""))
    
    shapefile(Boundaries, filename=country, overwrite=TRUE)
    
    #Boundaries=readOGR(".",country)   #Obtenido de http://www.maplibrary.org/library/stacks/Africa/index.htm
    plot(Boundaries)
    Bound.Pol=extent(Boundaries)
    if(Bound.Pol[1]>0) xmin=Bound.Pol[1]*0.95 else xmin=Bound.Pol[1]*1.05
    if(Bound.Pol[2]>0) xmax=Bound.Pol[2]*1.05 else xmax=Bound.Pol[2]*0.95
    if(Bound.Pol[3]>0) ymin=Bound.Pol[3]*0.95 else ymin=Bound.Pol[3]*1.05
    if(Bound.Pol[4]>0) ymax=Bound.Pol[4]*1.05 else ymax=Bound.Pol[4]*0.95
    xmin=round(xmin,4)
    xmax=round(xmax,4)
    ymin=round(ymin,4)
    ymax=round(ymax,4)
    
    
    #If download fail, then copy paste url in a web browser and download the data.nc file manually
    #After download, put the file into the country's folder.
    # Download netCDF file
    
    url=paste0('http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p',resol,'/.prcp/X/',xmin,'/',xmax,'/RANGEEDGES/Y/',ymin,'/',ymax,'/RANGEEDGES/T/(Jan%201981)/(Dec%202016)/RANGE/data.nc')
    
    dfile=c("data.nc")      
    print(url)
    #Additional time for slow connections and if DL takes time to process
    options(timeout = max(6000, getOption("timeout")))
    
    tryCatch(download.file(url, mode="wb",destfile=paste0(getwd(),"/data.nc")), 
             error = function(e) print(paste(url, 'The data cannot be downloaded from the datalibrary.')))  
    
    # Lectura de datos mediante netCDF ===========
    # 06/07/2017
    # H.Maureira
    
    #Importar datos (Esto debe ser descargado de manera manual desde IRI)
    #datos_ncdf <- nc_open("./input/data_10N_20S.nc")
    datos_ncdf <- nc_open(paste0(getwd(),"/data.nc"))
    #Extraer longitud
    lon <- ncvar_get(datos_ncdf, "X")
    
    #Extraer latitud
    lat <- ncvar_get(datos_ncdf, "Y")
    
    #Extraer tiempo (esto puede ser confuso, pero como tenemos las fechas, no habr?a problema en modificarlo) (1 jan 1981) - (31 Dic 2016)
    tpo <- ncvar_get(datos_ncdf, "T")
    tpo2 <- seq(from = as.Date("1981-01-01"), to = as.Date("2016-12-31"), by = "days")
    
    #get the name of the variable
    var_name_ndf = names(datos_ncdf$var)[1]
    
    # get precipitation
    #prcp_array <- ncvar_get(datos_ncdf, "precipitation") #prcp aparece en float
    prcp_array <- ncvar_get(datos_ncdf, var_name_ndf) #prcp aparece en float
    
    dlname <- ncatt_get(datos_ncdf,var_name_ndf,"long_name")
    dunits <- ncatt_get(datos_ncdf,var_name_ndf,"units")
    fillvalue <- ncatt_get(datos_ncdf,var_name_ndf,"_FillValue")
    dim(prcp_array)
    
    #Quick look
    image(lon,lat,prcp_array[,,1], col=rev(brewer.pal(10,"RdBu")))
    
    # create dataframe -- reshape data
    # matrix (nlon*nlat rows by 2 cols) of lons and lats
    # De esta forma, se obtiene la cantidad de estaciones virtuales en el CUADRADO.
    lonlat <- as.matrix(expand.grid(lon,lat))
    dim(lonlat)
    
    #Transformar el array en un rasterbrick (no es necesario parece)
    #array_brick <- brick(prcp_array, xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))
    
    # Se selecciona mapa del pa?s para seleccionar puntos de interes ==========
    
    
    #Se usan los l?mites del mapa para recortar la base de datos
    
    if(clip_method == "Shape"){
      mapa <- Boundaries
    } else {
      mapa <- sps
    }
    
    
    
    #Crear shapefile de puntos a partir de lonlat (corresponde a todas las estaciones virtuales, o sea, el rect?ngulo)
    est_cuadro <- data.frame(lonlat)
    est_cuadro$ID <- paste("id_", 1:nrow(est_cuadro), sep = "")
    #est_cuadro <- SpatialPointsDataFrame(coords = lonlatDF[,1:2], proj4string = proj4string(mapa), bbox = lonlatDF)
    coordinates(est_cuadro) <- ~ Var1 + Var2
    proj4string(est_cuadro) <- proj4string(mapa)
    
    #Se edita el archivo lonlat para seleccionar aquellos puntos que est?n dentro del l?mite pa?s
    est_selec <- est_cuadro[mapa,] #perfecto, se guarda
    plot(est_selec)
    
    #Se obtiene un archivo (dataframe) lonlat con los puntos seleccionados
    est_selecDF <- data.frame(est_selec)
    
    #Se crea un data frame que almacenar? en cada lista, los valores por d?a
    DF_est <- data.frame(date = tpo2, prcp_chirps = NA)
    
    #Se crea una lista cuyos nombres corresponde a lat long (que en el fondo son las estaciones)
    lista_est <- list()
    for (i in 1:nrow(est_selecDF)){ #Continuar desde ac?...
      
      print(paste0("station ",i," of ",nrow(est_selecDF)))
      
      lista_est[[i]] <- DF_est
      names(lista_est)[i] <- as.character(est_selecDF[i,3])
      
    }
    
    #........................................
    # Extraer valores desde el prcpc_array usando "est_selecDF"
    # almacenar en la lista
    
    l = 1
    for (j in 1:dim(prcp_array)[3]){ #ac? no es array brick, OJO
      print(paste0("month ",j," of ",dim(prcp_array)[3]))
      
      raster_uni <- prcp_array[,,j]
      raster_uni <- raster(t(raster_uni), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))
      raster_uni <- flip(raster_uni, "y")
      
      for (k in 1:nrow(est_selecDF)){
        
        valor_fecha <- extract(raster_uni, matrix(c(est_selecDF[k,1], est_selecDF[k,2]), ncol = 2))
        lista_est[[k]][l,2] <- valor_fecha
        
      }
      
      l = 1+l
      
    } # ac? finalizado debiera estar ordenados los valores por fecha.
    
    
    #...........................................
    # Exportar datos como txt
    dir.create(paste0(getwd(),"/est_procesadas/"))
    
    #fix by pablo, elimina columnas enteras que tengan na
    lista_est <- Filter(function(x)!all(is.na(x$prcp_chirps)), lista_est)
    
    for (m in 1:length(lista_est)){
      print(paste0("station ", m, " of ",length(lista_est)))
      
      export <- lista_est[[m]]
      write.table(export, paste0(getwd(),"/est_procesadas/", names(lista_est)[m], "_latam.txt"), sep =";", row.names = FALSE)
    }
    
    # Se exporta la metadata (Estaciones virtuales)
    metaexp <- est_selecDF[,c(3,1,2)]
    #si se encuentra dentro de las filtradas sin nan
    metaexp[metaexp$ID %in% names(lista_est),]
    
    colnames(metaexp) <- c("station_id", "lon", "lat")
    
    write.table(metaexp, paste0(getwd(),"/est_procesadas/","metadata.txt"), sep = ";", row.names = FALSE)
    
    #.............................................................
    #Convertir al formato CAZALAC BaseDatosEstaciones y BasedatosRegistros
    
    EstacionesLong=list()
    latam.list=list.files(path=paste0(getwd(),"/est_procesadas") ,
                          pattern = "\\latam.txt$")
    latam.list=substr(latam.list, 1, nchar(latam.list)-10)
    for (j in 1:length(latam.list)){
      EstacionesLong[[j]]=read.table(paste0(getwd(),"/est_procesadas/",latam.list[j],"_latam.txt"),sep=";",header=TRUE)
    }
    
    
    
    
    
    
    
    
    
    
    # BaseDatosRegistros=data.frame(id_station=rep(latam.list[1],36),
    #                               Year=1981:2016,
    #                               matrix(EstacionesLong[[1]][,2],ncol=12,byrow=T))
    # 
    # for (k in 2:length(latam.list)){
    #   bd=data.frame(id_station=rep(latam.list[k],36),
    #                 Year=1981:2016,
    #                 matrix(EstacionesLong[[k]][,2],ncol=365,byrow=T))
    #   BaseDatosRegistros=rbind(BaseDatosRegistros,bd)
    # }
    # 
    # colnames(BaseDatosRegistros)[3:14]=month.abb
    # 
    # # CORRECCION DE CEROS. TODOS LOS CEROS SON REEMPLAZADOS POR UN VALOR RANDOM UNIF ENTRE 0 Y 1
    # 
    # BaseDatosRegistros <- zero_correction(BaseDatosRegistros)
    # 
    # 
    # 
    # #row.names(BaseRegistrosPr)=NULL
    # write.csv(BaseDatosRegistros,"BaseDatosRegistros.csv",row.names=FALSE)
    # #write.csv(BaseDatosRegistros,"BaseDarosRegistrosBackup.csv",sep=",",row.names=FALSE)
    # 
    # #BaseDatosEstaciones. Defino el tama?o total a 500
    # BaseDatosEstaciones=read.csv(paste0(getwd(),"/est_procesadas/metadata.txt"),sep=";",header=TRUE)
    # colnames(BaseDatosEstaciones)[1]="id_station"
    # row.names(BaseDatosEstaciones)=NULL
    # BaseDatosEstaciones = BaseDatosEstaciones[BaseDatosEstaciones$id_station %in% names(lista_est),]
    # 
    # if (dim(BaseDatosEstaciones)[1]>maxstations){
    #   BaseDatosEstaciones=randomSample(BaseDatosEstaciones,maxstations)
    # } else {
    #   BaseDatosEstaciones=BaseDatosEstaciones
    # }
    # 
    # write.csv(BaseDatosEstaciones,"BaseDatosEstaciones.csv", row.names=FALSE)
    # #write.csv(BaseDatosEstaciones,"BaseDatosEstacionesBackup.csv", row.names=FALSE)
    # #..........................................................END ...........................................
    # newshapefile <- BaseDatosEstaciones
    # coordinates(newshapefile)=~lon+lat
    # proj4string(newshapefile)<- CRS("+proj=longlat +datum=WGS84")
    # #the same format that nc file
    # newshapefile@data["x"] <- BaseDatosEstaciones$lon
    # newshapefile@data["y"] <- BaseDatosEstaciones$lat
    # newshapefile@data["cell"] <- 1
    # raster::shapefile(newshapefile, "randomsample.shp", overwrite=TRUE)
    setwd(workdir)
    
  }
  
  else{
    message("Error selecting the model.")
  }
}



#opt1 generate pp 

for (k in 1:length(stations_metadata$id_station)) {
  coordenaday <- c(stations_metadata$lat[k]) #lat
  coordenadax <- c(stations_metadata$lon[k]) #lon
  
  #descargamos
  test <- tempfile()
  download.file(paste("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/T/(1%20Jan%201981)/(31%20Dec%202016)/RANGEEDGES/X/",round(coordenadax,1),"/VALUES/Y/",round(coordenaday,1),"/VALUES/T/exch/table:/text/text/:table/R.tsv", sep=""),test)
  sim <- NULL
  sim <- read.table(test, header = TRUE, sep="\t")
  
  #sim2$T <-as.Date(as.character(sim2$T), format = "%d %B %Y")
  sim$T <-seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by="days")
  write.csv(sim, paste(pathpreproc,stations_metadata$id_station[k],"_19812015_CHIRPSonly.csv", sep=""), row.names = FALSE)
  
}


















