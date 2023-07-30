#############PACKAGES###################################
#install.packages(stringr,dep=TRUE)
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("stringr")
############################################################
library(stringr)
quantsearch <- function(workingdir, periodo=10, percentil=50, pydir="")
{
setwd(workingdir)
##############funcion quantsearch-windows#################
setwd("input_sim")
bat <- paste(pydir,"python.exe quantsearch.py ",periodo," ",percentil,sep="")
write(bat, "qs.bat")
var <- system2("qs.bat", stdout=TRUE)
transformado<-''
a <- sapply(1, function(x) strsplit(var[30], ","))
for (j in 1:length(a[[1]])) {
  transformado[j] <- str_replace(sapply(1, function(x) strsplit(var[30], ","))[[1]][j] , '[:punct:]', '')
}
transformado <- transformado[!is.na(transformado)]
resultado2 <- strsplit(var[30],", ")
#transformado <- rapply(resultado2, c)
#transformado <- str_replace(transformado, '[:punct:]', '')
print(transformado)
setwd(workingdir)
#log
write(var, file = "log/quantsearch-output.txt")
return(as.numeric(transformado))
##########################################################
}
##############simgen9s_cmip5################################
#dejar en funcion de trendq
#solo 5 simix en funcion que se pueda modificar
simgen <- function(workingdir, simixlen=5, quantiles = c(0.5), obsix = '2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010' , simix, write=1, fname='sim_100kyr.dat', simlen=76, locate=2041, xval=0, M=0, trendmean=-0.125968, trendsd=1.76931, temptrendmean=3.97, temptrendsd=1.26, pydir="")
{
if(length(simix) < simixlen){
  simixlen = length(simix)
  print("Modificado simixlen")
}
setwd(workingdir)
write("", file = "log/logout-output.txt")

###CONFIG 2################################################
#quantiles <- c(0.15,0.25,0.5,0.75,0.85,0.95)
###!CONFIG#################################################

for (simquantil in 1:length(quantiles) ){
for (simmm in 1:simixlen){
  ####CONFIG####
  #,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010
  simix2 <- as.integer(simix[simmm])
  #variar trenq 0.15 0.25 0.5 0.75 0.95
  trendq <- quantiles[simquantil]
  ####END CONFIG#####
  bat <- paste(pydir,"python simgen9s_cmip5.py ",obsix," ",simix2," ",trendq," ", write," ",fname," ",simlen," ",locate," ",xval," ",M," ",trendmean," ",trendsd," ",temptrendmean," ",temptrendsd, sep="")
  write(bat, "correr.bat")
  log <- system2("correr.bat", stdout=TRUE)
  write(log, file = "log/logout-output.txt", append = TRUE)
}}
######################################################
}

###########graficos###################################
#instalamos si no existen
pkgTest("ggplot2")
pkgTest("reshape")
#cargamos
library(ggplot2) ; library(reshape)
#directorio al archivo
grafico_original <- function(workingdir,simix,estacion=2000,inicio=1980,iniciopret=2030,finpret=2036,quantiles)
{
  setwd(workingdir)
  file<- paste("output_sim/yearly_simulation_obshis_",estacion,"_",str_pad(simix,6,pad=0),".txt",sep="")
  data <- read.table(file, quote="\"")
  names(data) <- c("pr", "tx", "tm")
  data$year <- seq(inicio, by=1, length.out=length(data[[1]]))
  grafdata <- melt(data, id="year")
  
  q <- ggplot(grafdata, aes(year, value))  +geom_line()
  m <-q + geom_line(aes(year, value)) + facet_grid(variable ~ . , scales = "free", space = "free" ) + geom_vline(xintercept=c(iniciopret,finpret), colour="red")+  ggtitle(paste("quantiles",quantiles,sep=""))
  m + geom_vline(xintercept=c(2010), colour="blue" )
  
}




GraficarAnual <- function(numestacion = 1, percentiles = c(10,50,95), yearend=2014) {
numestacion <- numestacion+1
  for (percentil.value in percentile){
    tmax <- dir(path=paste("output_sim/anual/",percentil.value,"",sep=""), pattern="tmax", full.names=TRUE, recursive = TRUE) 
    tmin <- dir(path=paste("output_sim/anual/",percentil.value,"",sep=""), pattern="tmin", full.names=TRUE, recursive = TRUE) 
    pp <- dir(path=paste("output_sim/anual/",percentil.value,"",sep=""), pattern="pp", full.names=TRUE, recursive = TRUE) 
    #generamos 3,1
    pp_est <- read.csv(pp[1], head=TRUE)
    pdf(paste("output_sim/graficos/P",percentil.value,"_",gsub('X','', colnames(pp_est)[numestacion]),".pdf",sep=""),  width=8, height=9)
    par(mfrow=c(3,1))
    plot(pp_est[,numestacion], x=as.Date(pp_est[,1]) , type="l", ylab = "Precipitation [mm]", xlab = "Year")
    #title(paste("Proyecci?n Precipitaci?n Para Estaci?n",gsub('X','', colnames(pp_est)[numestacion]),"P",percentil.value))
    
    for (pp.value in pp){
      pp_est <- read.csv(pp.value)
      lines(pp_est[,numestacion], x=as.Date(pp_est[,1]))
      abline(v=as.Date(paste(yearend,"-12-31",sep="")),lty = 5)
      
    }
    
    tmin_est <- read.csv(tmin[1])
    plot(tmin_est[,numestacion], type="l", x=as.Date(pp_est[,1]),  ylab = "Tmin [C]", xlab = "Year")
    #title(paste("Proyecci?n Tmin Para Estaci?n",gsub('X','', colnames(pp_est)[numestacion]),"P",percentil.value))
    
    for (tmin.value in tmin){
      tmin_est <- read.csv(tmin.value)
      lines(tmin_est[,numestacion],x=as.Date(pp_est[,1]))
      abline(v=as.Date(paste(yearend,"-12-31",sep="")),lty = 5)
    }
    
    tmax_est <- read.csv(tmax[1])
    plot(tmax_est[,numestacion], type="l", x=as.Date(pp_est[,1]), ylab ="Tmax [?C]" , xlab = "Year" )
    #title(paste("Proyecci?n Tm?x Para Estaci?n",gsub('X','', colnames(pp_est)[numestacion]),"P",percentil.value))
    
    for (tmax.value in tmax){
      tmax_est <- read.csv(tmax.value)
      lines(tmax_est[,numestacion],x=as.Date(pp_est[,1]))
      abline(v=as.Date(paste(yearend,"-12-31",sep="")),lty = 5)
    }
    dev.off()
  }
}

GraficarAnual2 <- function(numestacion = 1, percentiles = c(10,50,95)) {
  for (percentil.value in percentile){
    
    tmax <- dir(path=paste("output_sim/anual/",percentil.value,"",sep=""), pattern="tmax", full.names=TRUE, recursive = TRUE) 
    tmin <- dir(path=paste("output_sim/anual/",percentil.value,"",sep=""), pattern="tmin", full.names=TRUE, recursive = TRUE) 
    pp <- dir(path=paste("output_sim/anual/",percentil.value,"",sep=""), pattern="pp", full.names=TRUE, recursive = TRUE) 
    #generamos 3,1
    pp_est <- read.csv(pp[1], head=TRUE)
    pdf(paste("output_sim/graficos/P",percentil.value,"_",gsub('X','', colnames(pp_est)[numestacion]),".pdf",sep=""))
    par(mfrow=c(3,1))
    plot(pp_est[,numestacion], x=as.Date(pp_est[,1]) , type="l", ylab = "Precipitation", xlab = "Year")
    #title(paste("Proyecci?n Precipitaci?n Para Estaci?n",gsub('X','', colnames(pp_est)[numestacion]),"P",percentil.value))
    
    for (pp.value in pp){
      pp_est <- read.csv(pp.value)
      lines(pp_est[,numestacion], x=as.Date(pp_est[,1]))
      abline(v=as.Date("2013-12-31"),lty = 5)
      
    }
    
    tmin_est <- read.csv(tmin[1])
    plot(tmin_est[,numestacion], type="l", x=as.Date(pp_est[,1]),  ylab = "Tmin", xlab = "Year")
    #title(paste("Proyecci?n Tmin Para Estaci?n",gsub('X','', colnames(pp_est)[numestacion]),"P",percentil.value))
    
    for (tmin.value in tmin){
      tmin_est <- read.csv(tmin.value)
      lines(tmin_est[,numestacion],x=as.Date(pp_est[,1]))
      abline(v=as.Date("2013-12-31"),lty = 5)
    }
    
    tmax_est <- read.csv(tmax[1])
    plot(tmax_est[,numestacion], type="l", x=as.Date(pp_est[,1]), ylab ="Tmax" , xlab = "Year" )
    #title(paste("Proyecci?n Tm?x Para Estaci?n",gsub('X','', colnames(pp_est)[numestacion]),"P",percentil.value))
    
    for (tmax.value in tmax){
      tmax_est <- read.csv(tmax.value)
      lines(tmax_est[,numestacion],x=as.Date(pp_est[,1]))
      abline(v=as.Date("2013-12-31"),lty = 5)
    }
    dev.off()
  }
}
