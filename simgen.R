######################################
#Simgen desde R
#Este archivo ejecuta simgen desde R
#?ltima Modificaci?n: 13-05-2019 21:30
######################################

#configuraci?n
#workingdir = Corresponde a la carpeta donde se encuentra simgen, cambiar
#directorio de simgen y script
#workingdir <- 'C:/Users/simgen/Desktop/simgen'
#setwd(workingdir)

#seteo autom?tico de directorio de trabajo

library(rstudioapi)
current_path <- rstudioapi::getActiveDocumentContext()$path 
#UTF-8
#Encoding(current_path) <- "UTF-8"
#directorio de simgen y script
#opcion b
#setwd("D:/Dropbox-Temporal/cazalac/simgen-Limari_v3")
setwd(dirname(current_path ))
print(getwd())
workingdir <- getwd()

#python dir, con / al final
pythondir <- 'C:/Users/pablo/anaconda3/envs/py2/'
#add path to python
#############

#cargamos las funciones simix y simgen, no tocar
source("libreriasR/fun_simgen.R")

#Si se desea trabajar un solo percentil, sacar del ciclo usar rango en quantiles
#si no se desean un n?mero muy superior de simixlen, se recomienda limitar simixlen
percentile <- c(10,50,95)
#corresponde a las estaciones procesadas
estaciones <- '2000,2001,2002'

for (percentil.value in percentile) {
  #la funci?n quantsearch se encarga de llamar al script de python quantsearch.py
  #periodo: Corresponde a la matriz de a?os
  #percentil: Corresponde al percentil
  simix <- quantsearch(workingdir,periodo=10,percentil=percentil.value, pydir=pythondir)
  
  #la funci?n simgen se encarga de llamar al script simgen9s_cmip5.py
  #simixlen: Es el m?ximo de simix a utilizar por percentil, si simix < simixlen entonces se usa simix.
  #quantiles corresponde a los quantiles a generar
  simgen(workingdir, simixlen=100, quantiles = c(percentil.value/100), 
         obsix = estaciones, simix, write=1, fname='sim_100kyr.dat', simlen=80, locate=2014, 
         xval=0, M=1, trendmean=-0.03354315, trendsd=0.03462163, 
         temptrendmean=0.04753433, temptrendsd=0.01393825, pydir=pythondir)
}
# 1 = not necessarily the first is station 2000, can be 2002 for example
#yearend will do the vertical line.

for (sta in 1:length(unlist(strsplit(estaciones, ",")))) {
  GraficarAnual(numestacion = sta, percentiles = c(10,50,95), yearend=2014)  
}
