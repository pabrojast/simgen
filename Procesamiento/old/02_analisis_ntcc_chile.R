library(ggplot2) ; library(reshape)
# Leo el archivo anualizado de una estacion

data <- read.table("output_sim/yearly_simulation_obshis_2000_001482.txt", quote="\"")
names(data) <- c("pr", "tx", "tm")
data$year <- seq(1973, by=1, length.out=121)

grafdata <- melt(data, id="year")

q <- ggplot(grafdata, aes(year, value))  +geom_line()

m <-q + geom_line(aes(year, value)) + facet_grid(variable ~ . , scales = "free", space = "free" ) + geom_vline(xintercept=c(2021,2031), colour="red")
m + geom_vline(xintercept=c(2011), colour="blue" )

################################