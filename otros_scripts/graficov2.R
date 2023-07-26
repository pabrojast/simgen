require(geoR)
require(lattice)
library(rasterVis)
library(xts)
#vmax y vmin
setwd("C:\\Users\\Pablo\\Documents\\WEAP Areas\\San Lucas\\Datos")
file <- "caudal_mensual_weap.csv"
temp <- read.csv(file, header = FALSE, sep = ",", quote = "\"",
                 dec = ".", fill = TRUE, comment.char = "")

temp$date <- as.Date(paste(temp$V1,"-",temp$V2,"-01",sep=""))
temp$V1 <- NULL
temp$V2 <- NULL
temp <- na.omit(temp)
#porque no tengo temp ni pp posterior para este ejemplo
temp <- temp[temp$date<"2011-01-01",]
ts <- xts(temp$V3, temp$date)

ts_y = apply.yearly(ts, sum)


#como ya tenemos vmax y vmin, ahora necesitamos Tprom y pp en la fecha dada
###temperatura
file <- "original_tt.csv"
temperatura <- read.csv(file, header = FALSE, sep = ",", quote = "\"",
                 dec = ".", fill = TRUE, comment.char = "")

temperatura$date <- as.Date(paste(temperatura$V1,"-",temperatura$V2,"-01",sep=""))
temperatura$V1 <- NULL
temperatura$V2 <- NULL
temperatura <- na.omit(temperatura)
temperatura$mean <- (temperatura$V3+temperatura$V4)/2
tstempprom <- xts(temperatura$mean, temperatura$date)

tstemp_y = apply.yearly(tstempprom, mean)
##################
#precipitacion
###################
file <- "original_pp.csv"
pp <- read.csv(file, header = FALSE, sep = ",", quote = "\"",
                        dec = ".", fill = TRUE, comment.char = "")

pp$date <- as.Date(paste(pp$V1,"-",pp$V2,"-01",sep=""))
pp$V1 <- NULL
pp$V2 <- NULL
pp <- na.omit(pp)
##precipitacion promedio por estacion
pp$mean <- (pp$V3+pp$V4)/2
tsppprom <- xts(pp$mean, pp$date)

tspp_y = apply.yearly(tsppprom, sum)
########
#buscando maximos y minimos
########

#buscamos nuestro punto de volumen Vmax y Vmin
#vmax
time(ts_y[which.max(ts_y), ])
#vmin
time(ts_y[which.min(ts_y), ])

#punto maximo
tspp_y[time(ts_y[which.max(ts_y), ])]
tstemp_y[time(ts_y[which.max(ts_y), ])]
#output : 756.95, 23.28827

#punto minimo 1994-2010
tspp_y[time(ts_y[which.min(ts_y), ])]
tstemp_y[time(ts_y[which.min(ts_y), ])]
#output : 125.7, 21.81622

#########futuro include, con la data actual, incluido por comodidad para compartir y trabajar en cuencas##########
file <- "C:/Users/Pablo/Documents/WEAP Areas/San Lucas/script/salida.csv"
salida <- read.csv(file, header = TRUE, sep = ",", quote = "\"",
                 dec = ".", fill = TRUE, comment.char = "")

DataSanLucas <- setClass(
  # Set the name for the class
  "DataSanLucas",
  
  # Define the slots
  slots = c(
    pp = "numeric",
    tt = "numeric",
    caudal = "numeric",
    nombre = "character"
  ),
  
  # Set the default values for the slots. (optional)
  #Malla - caudal en Mm³
  prototype=list(
    pp = as.numeric(salida$pp),
    tt  = as.numeric(salida$tt),
    caudal = as.numeric(salida$volumen)/1000000,
    nombre = "San Lucas"
  )
)
###############################################################
#iniciamos san lucas
cuenca <- DataSanLucas()

cuenca@caudal
cuenca@pp
cuenca@tt
cuenca@nombre


#corresponde a la coordenada X de los puntos 
#output : 756.95, 23.28827
#output : 125.7, 21.81622
#prom, 5%,95%, 2010-2090
puntosx <- c(316.620967742,96.525,549.1,247.57345679,246.713580247,262.914197531,265.400617284,283.895679012,282.914197)
#corresponde a la coordenada Y de los puntos
puntosy <- c(23.0093320772,21.4548317189,24.4444359005,26.8823261791,26.8815,26.181431941,26.17849,25.4998181612,25.4994439)
  
#corresponde al tipo de punto a colocar en el gráfico
pchpuntos <- c(0,1,2,3,3,4,4,5,5)
typos <- c("p","p","p","p","p","p","p","p")


#Mallas Generadas
############Se genera grid#################################

elevation.df = data.frame(x = cuenca@pp, y = cuenca@tt, z = cuenca@caudal )
elevation.loess = loess(z ~ x*y, data = elevation.df, degree = 2, span = 0.25)
elevation.fit = expand.grid(list(x = seq(min(cuenca@pp), max(cuenca@pp), 0.05), y = seq(min(cuenca@tt), max(cuenca@tt), 0.05)))
z = predict(elevation.loess, newdata = elevation.fit)
elevation.fit$Height = as.numeric(z)

#Corresponde a la etiqueta, se vuelve a difinir tipo de puntos
#ref https://stat.ethz.ch/R-manual/R-devel/library/lattice/html/xyplot.html
key2 <- list(
  #title="Leyenda", cex.title = 1,
  #prom, 5%,50%,95%, 2010-2090
  text=list(lab=c("Promedio 1980-2010","Percentil 5 - 1980-2010", "Percentil 95 1980-2010", "Proyección 2010-2090 CC5%", "Proyección 2010-2090 CC50%","Proyección 2010-2090 CC95%" ), cex=0.9),
  lines=list(
    pch=c(0,1,2,3,3,4,4,5,5),
    col=1,
    type=c("p","p","p","p","p","p","p","p","p"),
    cex=0.8),
  space='top', rep = FALSE,
  
  #space="right", 
  rectangles=list(col=c("#f1eef6", "#bdc9e1","#74a9cf","#0570b0","#045a8d"), size=3),
  text=list(c('E (< 0.48 [Mm]³)', 'D (0.48 - 0.79 [Mm]³)', 'C (0.79 - 1.42 [Mm]³)','B (1.42 - 2.9 [Mm]³)','A (> 2.9 [Mm]³)'), cex = 0.8)
)

levelplot(Height ~ x*y, data = elevation.fit, aspect = 1, col.regions = colorRampPalette(c("#f1eef6", "#bdc9e1","#74a9cf","#0570b0","#045a8d")), at=c(0,0.48,0.79,1.42,2.90,max(cuenca@caudal)),
          xlab = "Total Anual de Precipitación [mm]", ylab = "Temperatura Promedio [°C]",
          main = paste(cuenca@nombre,"\n Volumen Acumulado Anual Promedio [Mm³]"), key=key2,
           panel = function(x, y, subscripts, ...) {
            panel.levelplot(x, y, subscripts, ...)
            panel.xyplot(puntosx,puntosy , type=typos, cex = 1, col = 1, pch=pchpuntos)
            #panel.xyplot(puntosx,puntosy , type=c("l","l"), cex = 0.5, col.line = "red", lwd=2.5, pch=pchpuntos) #agrega la linea entre
          }
)




#no necesario en esta parte
#######Gráfico en xyz 27-03-2017
#generando una secuencia en el rango de los puntos x y replicamos en y con reg. lineal.
yy <- list()
xx = seq(min(puntosx), max(puntosx), 0.05)
for (variable in 1:length(xx)) {
  yy[variable] = lm(puntosy ~ puntosx)$coeff[[2]]*as.numeric(xx[variable])+lm(puntosy ~ puntosx)$coeff[[1]]
}
recta <- data.frame(x = as.numeric(xx), y = as.numeric(yy))
zz = predict(elevation.loess, newdata = recta)
recta$z = as.numeric(zz)

#generando los gráficos
#pp vs volumen
plot(range(recta$x),range(c(recta$z,0.7*quantile(cc, c(limite)))),xlab="Precipitación [mm]", ylab="Volumen [Mm³]", type="n")
lines(rev(recta$x), recta$z, type="l", lwd=2.5, col="blue")
#quantile
lines(c(min(recta$x),max(recta$x)),c(quantile(cc, c(limite)),quantile(cc, c(limite))), type="l", lwd=2.5,  col="red")
legend(min(recta$x)+(max(recta$x)-min(recta$x))/1.8, range(recta$z)[2], legend=c("Límite Crítico","Volumen"), lty=c(1,1), col=c("red","blue"), lwd=c(2.5,2.5))

title(var)
#temperatura vs volumen
plot(range(recta$y),range(c(recta$z,0.7*quantile(cc, c(limite)))),xlab="Temperatura [°C]", ylab="Volumen [Mm³]", type="n")
lines(recta$y, recta$z, type="l", lwd=2.5, col="blue")
title(var)
#quantile
lines(c(min(recta$y),max(recta$y)),c(quantile(cc, c(limite)),quantile(cc, c(limite))), type="l", lwd=2.5, col="red")
legend(min(recta$y)+(max(recta$y)-min(recta$y))/1.8, range(recta$z)[2], legend=c("Límite Crítico","Volumen"), lty=c(1,1), col=c("red","blue"), lwd=c(2.5,2.5))
###Fin de modificación