### Script para transformar los datos al formato ACRU
## Cargo las librerías necesarias ###########################################################
library(lattice); library(dse) ; library(ggplot2); library(seas); library(date)
#############################################################################################
# Preparación de datos para la media regional
# En este caso uso de ejemplo las estaciones de Argentina y Uruguay
#############################################################################################
### Leo los archivos diarios del LARS Observado
# seteo el directorio de trabajo
setwd("/media/compartido/simgen_Huasco/")
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
  write.table(codigos, "acru/codigos.txt", append=TRUE,col.names=FALSE, row.names=FALSE, sep=" "  )
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
#model09 <- estVARXls(ts, max.lag=9)
#model10 <- estVARXls(ts, max.lag=10)

informationTests(model01, model02, model03, model04, model05, model06, model07, model08)
checkResiduals(model01)
# genero la long.seq con el modelo lag(1)
long.seq <- simulate(model01, start=c(1973,1), freq=1,sampleT=100000)
# calculo las correlaciones en los detrended(obs) y los simulados
cor(detrended)
cor(long.seq$output[])

# Tabla 3 del paper   Correlaci??n entre variables
# (detrended)                               
#            rain       tmax     tmin
# rain  1.0000000 -0.4366612 0.506614
# tmax -0.4366612  1.0000000 0.223449
# tmin  0.5066140  0.2234490 1.000000
# (long.seq$output[])
#            Rain       Tmax      Tmin
# Rain  1.0000000 -0.4400227 0.5083784
# Tmax -0.4400227  1.0000000 0.2193674
# Tmin  0.5083784  0.2193674 1.0000000

# # model is OK!! 

# ### HASTA ACA LLEGUE --- ME parece que de aca en mas no lo usa mas  !!!!!!!

# calculo correlacion obs-media regional
dat <- read.table("acru/TODOS_ANUAL.TXT2")
dat$V2 <- dat$V2/365 # paso a lluvia por dia

correlaciones <- data.frame(length=length(files))
n <-1
for (estac in unique(dat$V5)) { #estac="UYLBOBS.st"
  
  tempo <- subset(dat, dat$V5==estac)  
  correlaciones[n] <- cor(tempo$V2, media.regional.dia[,1] ) 
  n <-n+1
}

colnames(correlaciones)<- unique(dat$V5)

# escribe archivo de salida de long.seq
write(t(long.seq$output), ncolumns=3, file="acru/long_seq.txt")

# ########################################################################
#  Ahora comienzo a procesar los datos GCM para hacer la proyecci??n
# Utilizo los 14 modelos del paper (en ese orden- tabla 1)
# Hay que realizar un suavizado de la serie de precip y temp y regresar el 
# log(precip) con la temp para calcular el cambio de la precip/grado de aumento
# de la temp. Despues ajustar a una distribucion normal, para calcular la media
# y el sd (trendmean y trendsd) y de esta distribucion se calcula trendq
# #########################################################################
year <- rep(2006:2099)
# para 1 modelo, ver de hacer el loop por los files x ahora lo hago a mano

path <- "~/IRI/CMIP5_UY" 
arch <- list.files(path)
num_files <- length(arch)
coef <- numeric(0)
for(n in 1:(num_files/2)) {
  #n=11
  
  number <-formatC(n, width = 2, format = "d", flag = "0") 
  fileinpr <- paste(path, "/cmip5_rain_", number, ".tsv", sep=""   )  
  fileintm <- paste(path, "/cmip5_temp_", number, ".tsv", sep=""   )  
  
  pr01 <- read.delim(fileinpr, skip=1)
  pr01 <-pr01[1:94,]
  names(pr01)<- c("tiempo", "pr")
  pr01$year <- year
  pr01$pr <- pr01$pr*86400
  pr01$prA <- pr01$pr*365
  tm01 <- read.delim("~/IRI/CMIP5_UY/cmip5_temp_01.tsv", skip=1)
  tm01 <-tm01[1:94,]
  names(tm01)<- c("tiempo", "tm")
  tm01$year <- year
  
  reg01 <- lm(log(pr01$pr) ~ tm01$tm)
  coefb <- summary(reg01)$coefficients[2, 1]
  coef[n]<- coefb
}
library(MASS) 
trendmean <- mean(coef)
trendsd <- sd(coef)
f <- fitdistr(coef, "normal")

qqnorm(coef)
qqline(coef)

plot(tm01$tm, log(pr01$pr))



### genera la media movil de la long seq cada 10 a??os

sim<- read.table("C:/IRI_Oct/acru/long_seq.txt", quote="\"")
# sim_mean <- sim[1:10000,]
# write(t(sim_mean), ncolumns=3, file="C:/IRI_Junio2013/datos simgen/UY/input_sim/sim_mean.txt")

ma <- function(x,n=10){filter(x,rep(1/n,n), sides=2)}

mapr <- ma(sim$V1)
plot(mapr)
decades <- rep(1:10000, each=10)
tempodec <- cbind(sim, decades)
tempodec2<- aggregate(tempodec,by = list(decades), FUN="mean")
tempodec2$Group.1 <- tempodec2$decades <- NULL
write(t(tempodec2), ncolumns=3, file="C:/IRI_Oct/acru/sim_mean_OK.txt")

