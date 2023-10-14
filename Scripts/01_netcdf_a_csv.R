#funciones
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
#library
pkgTest("ncdf4")
#prenombre = tmax or tmin
#####config
prenombre <- "tmax" # filename, this case tmax or tmin
dname <- "tmax"  # var in the netcdf, this case tmax or tmin
setwd("D:/Koen_TMAXyTMIN")
#just in case you need it
#memory.limit(16000)
#####enconfig

for (j in 1979:2019) {
  ncname <- paste(prenombre,".",j,sep="")
  print(ncname)
  ncfname <- paste(ncname,".nc", sep="")
  
  # open a netCDF file
  ncin <- nc_open(ncfname)
  print(ncin)
  
  #Get the longtiudes and latitudes as before, using now the ncvar_get() function in ncdf4.
  lon <- ncvar_get(ncin,"lon")
  nlon <- dim(lon)
  head(lon)
  
  lat <- ncvar_get(ncin,"lat",verbose=F)
  nlat <- dim(lat)
  head(lat)
  
  #Get the time
  t <- ncvar_get(ncin,"time")
  tunits <- ncatt_get(ncin,"time","units")
  nt <- dim(t)
  
  #Get the input variable (tmp) and its attributes, and verify the size of the array.
  tmp_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"missing_value")
  #remplace NA
  tmp_array[tmp_array==fillvalue$value] <- NA
  ## [1] 720 360  12
  ####Get the global attributes.
  
  tmp_vec_long <- as.vector(tmp_array)
  tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
  lonlat <- as.matrix(expand.grid(lon,lat))
  tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
  head(na.omit(tmp_df02,1))
  
  # Hack, Changing the relevant longitude (0-360 to 180-180)
  names <- as.numeric(tmp_df02$Var1)
  names[names > 180] <- names[names > 180] - 360
  tmp_df02$Var1 <- names
  
  #en caso de querer limitar los archivos por peso
  #Var2 <- 40
  #Var1 <- -72
  #var 1 de 0 a 360 longitud
    #eliminaos 1 lat sobre y 1 lan bajo
  #Limitado <- tmp_df02[as.numeric(tmp_df02$Var2)<as.numeric(Var2+1) & as.numeric(tmp_df02$Var2)>as.numeric(Var2-1),]
  #fastest than use tmp_df02
  #Limitado2 <- Limitado[as.numeric(Limitado$Var1)<as.numeric(Var1+1) & as.numeric(Limitado$Var1)>as.numeric(Var1-1),]
  #eliminamos en blanco
  #Limitado3 <- Limitado2[!is.na(Limitado2)]
  
  write.csv(tmp_df02,file = paste(ncname,".csv"))
  
  nc_close(ncin)
}