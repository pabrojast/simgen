library(stringr)
##DIRECTORIO DE TRABAJO (debe ser el mismo de simgen)#####
workingdir = 'C:/Users/Pablo/Dropbox/cazalac/simgen-Huasco'
#########################################################
setwd(workingdir)

##############funcion quantsearch-windows#################
periodo <- 10
percentil <- 10
setwd("input_sim")
bat <- paste("python quantsearch.py ",periodo," ",percentil)
write(bat, "qs.bat")
var <- system2("qs.bat", stdout=TRUE)
resultado2 <- strsplit(var[30],", ")
transformado <- rapply(resultado2, c)
transformado <- str_replace(transformado, '[:punct:]', '')
#log
write(var, file = "quantsearch-output.txt")
##########################################################


##############opciones simgen9s################################
##CONFIG########
setwd(workingdir)
obsix <- '2000,2001'
simix <- 8999
bfactor <- 0.1
write <- 1
fname <- 'sim_100kyr.dat'
simlen <-66
locate <-2041
xval <-0
M <-1
##END CONFIG#####
bat <- paste("python simgen9s.py ",obsix," ",simix," ",bfactor," ", write," ",fname," ",simlen," ",locate," ",xval," ",M)
write(bat, "correr.bat")
system2("correr.bat", stdout=TRUE)
######################################################


