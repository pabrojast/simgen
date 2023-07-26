## Modificado Pablo Rojas, 04-08-2017, fix: write table con problemas.
### Script para transformar los datos al formato ACRU
## Cargo las librerías necesarias ###########################################################
library(lattice); library(dse) ; library(ggplot2); library(seas); library(date)
#############################################################################################
# Preparación de datos para la media regional
# En este caso uso de ejemplo las estaciones de Argentina y Uruguay
#############################################################################################
### Leo los archivos diarios modificados (imputados)
# seteo el directorio de trabajo
#setwd("/media/sf_Compartido/simgen")
setwd("C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen")
files <- list.files("daily", pattern=".csv")
# debe comenzar en 1000 para mantener los 4 digitos
codigo.estacion.inicial <- 2000 

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

plot(resultado$year, resultado$rain, type="p")
lines(resultado$year, resultado$trendccM, col="red")
xyplot(detrendcc ~ year, data = resultado, type=c("l", "p"))

detrended <- data.frame(resultado$detrendcc)
names(detrended) <- c("rain")
## Ahora hago lo mismo pero para tmax
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

## Ahora hago lo mismo pero para tmin
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

write.table(detrended, "acru/media_regional_dia_detrended.txt", col.names=FALSE, row.names=FALSE)
# Armo la figura 3 del paper
names(resultado.rain) <-c("year", "value", "trendccM", "trendcc", "detrendcc", "var")
names(resultado.tmax) <-c("year", "value", "trendccM", "trendcc", "detrendcc", "var")
names(resultado.tmin) <-c("year", "value", "trendccM", "trendcc", "detrendcc", "var")

datgraf <- rbind(resultado.rain, resultado.tmax, resultado.tmin)
q <- ggplot(datgraf, aes(year, value))  +geom_line()

q + geom_line(aes(year, trendccM), colour="red") + facet_grid(var ~ . , scales = "free", space = "free" )
################################
# Figura 7 del paper
plot(detrended)
################################
## Ahora debo aplicar el paquete DSE a la serie sin tendencia
## primero estimo el modelo con estVARXls y se genera la serie con simulate
## 

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
model08 <- estVARXls(ts, max.lag=8)
model09 <- estVARXls(ts, max.lag=9)
model10 <- estVARXls(ts, max.lag=9)

informationTests(model01, model02, model03, model04, model05, model06, model07, model08, model09, model10)
checkResiduals(model01)
# genero la long.seq con el modelo lag(1)
long.seq <- simulate(model01, start=c(1973,1), freq=1,sampleT=100000)
# calculo las correlaciones en los detrended(obs) y los simulados
cor(detrended)
cor(long.seq$output[])

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