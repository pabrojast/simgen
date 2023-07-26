workingdir = 'C:/Users/Pablo/Dropbox/cazalac/simgen_mexico_san_lucas' #sin / al final
quantilesq = 0.5
estaciones = c(2000,2001)
setwd(workingdir)

#listamos archivos en directarios y sacamos los simix disponibles
archivos <- list.files(paste(workingdir,"/output_sim/anual/",quantilesq*100,"/",sep=""))
nums <- str_match_all(archivos, "[0-9]+") %>% unlist %>% unique %>% as.numeric

#variables de guardado
temperatura <- list()
precipitacion <- list()
#sacamos los promedios 2010-2090
for (nsimix in 1:length(nums)) {
  #pp
  data <- read.table(paste(workingdir,"/output_sim/anual/",quantilesq*100,"/","pp_",nums[nsimix],".csv",sep=""), quote="\"",sep=",",header=TRUE)
  names(data) <- c("fecha", estaciones)
  data$year <- seq(inicio, by=1, length.out=length(data[[1]]))
  promedio <- mean(colMeans(data[(data$year>(2080)),as.character(estaciones)]))
  precipitacion <- c(precipitacion, promedio)
  #tt
  data <- read.table(paste(workingdir,"/output_sim/anual/",quantilesq*100,"/","tprom_",nums[nsimix],".csv",sep=""), quote="\"",sep=",",header=TRUE)
  names(data) <- c("fecha", estaciones)
  data$year <- seq(inicio, by=1, length.out=length(data[[1]]))
  promedio <- mean(colMeans(data[(data$year>(2080)),as.character(estaciones)]))
  temperatura <- c(temperatura, promedio)
}

TempFinal <- "("
PrecFinal <- "("
for (nn in 1:length(nums)) {
  TempFinal <- paste(TempFinal,as.character(temperatura[nn]),sep="",",")
  PrecFinal <- paste(PrecFinal,as.character(precipitacion[nn]),sep="",",")
}

write.table(TempFinal, file = paste("CC",quantilesq*100,"tt_.txt",sep=""), append = FALSE, quote = TRUE, sep = " ", 
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, 
            col.names = TRUE, qmethod = c("escape", "double")) 

write.table(PrecFinal, file = paste("CC",quantilesq*100,"pp.txt",sep=""), append = FALSE, quote = TRUE, sep = " ", 
            eol = "\n", na = "NA", dec = ".", row.names = TRUE, 
            col.names = TRUE, qmethod = c("escape", "double")) 