library(ggplot2) ; library(reshape)

#file<- "C:/Users/Pablo/Dropbox/cazalac/Simgen-Limari v2/simgen/output_sim/yearly_simulation_obshis_2005_000100.txt"
file <- "D:/Dropbox/cazalac/simgen-Limari_v3.01/simgen-Limari_v3/input_sim/sim_100kyr.dat"
data <- read.table(file, quote="\"")
names(data) <- c("pr", "tx", "tm")

data$year <- seq(as.Date('1973-01-01'),as.Date('1973-01-01')+length(data[[1]])-1,by = 1)

data$tx<-NULL
data$tm<-NULL
grafdata <- melt(data, id="year")


q <- ggplot(grafdata, aes(year, value))  +geom_line()


m <-q + geom_line(aes(year, value)) + facet_grid(variable ~ . , scales = "free", space = "free" ) + labs(x="Year", y = "Precipitation Anomaly [mm]")+ theme_light()
print(m)

################################
