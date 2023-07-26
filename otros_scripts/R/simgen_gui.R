require(tcltk)
library(stringr)


mydialog <- function(){
  xvar <- tclVar('C:/Users/Pablo/Dropbox/cazalac/simgen-Huasco')
  yvar <- tclVar(10)
  zvar <- tclVar(10)
  obsix2 <- tclVar('2000,2001')
  trendq2 <- tclVar(0.1)
  write2 <- tclVar(1)
  fname2 <- tclVar('sim_100kyr.dat')
  simlen2 <- tclVar(66)
  locate2 <- tclVar(2041)
  xval2 <- tclVar(0)
  M2 <- tclVar(1)
  trendmean2 <- tclVar(-0.125968)
  trendsd2 <- tclVar(1.76931)
  tt <- tktoplevel()
  tkwm.title(tt,"Simgen simple GUI")
  x.entry <- tkentry(tt, textvariable=xvar)
  y.entry <- tkentry(tt, textvariable=yvar)
  z.entry <- tkentry(tt, textvariable=zvar)
  obsix2.entry <- tkentry(tt, textvariable=obsix2)
  trendq2.entry <- tkentry(tt, textvariable=trendq2)
  write2.entry <- tkentry(tt, textvariable=write2)
  fname2.entry <- tkentry(tt, textvariable=fname2)
  simlen2.entry <- tkentry(tt, textvariable=simlen2)
  locate2.entry <- tkentry(tt, textvariable=locate2)
  xval2.entry <- tkentry(tt, textvariable=xval2)
  M2.entry <- tkentry(tt, textvariable=M2)
  trendmean2.entry <- tkentry(tt, textvariable=trendmean2)
  trendsd2.entry <- tkentry(tt, textvariable=trendsd2)
  
  reset <- function() {
    tclvalue(xvar)<-''
    tclvalue(yvar)<-""
    tclvalue(zvar)<-""
    tclvalue(obsix2)<-""
    tclvalue(trendq2)<-""
    tclvalue(write2)<-""
    tclvalue(fname2)<-""
    tclvalue(simlen2)<-""
    tclvalue(locate2)<-""
    tclvalue(xval2)<-""
    tclvalue(M2)<-""
    tclvalue(trendmean2)<-""
    tclvalue(trendsd2)<-""
  }
  
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  submit <- function() {
    workingdir = tclvalue(xvar)
    #########################################################
    setwd(workingdir)
    y <- as.numeric(tclvalue(yvar))
    z <- as.numeric(tclvalue(zvar))
    ##############funcion quantsearch-windows#################
    periodo <- y
    percentil <- z
    setwd("input_sim")
    bat <- paste("python quantsearch.py ",periodo," ",percentil)
    write(bat, "qs.bat")
    var <- system2("qs.bat", stdout=TRUE)
    resultado2 <- strsplit(var[30],", ")
    transformado <- rapply(resultado2, c)
    transformado <- str_replace(transformado, '[:punct:]', '')
    #log
    write(var, file = "quantsearch-output.txt")
    print(transformado)
    ##########################################################
    
    setwd(workingdir)
    write("", file = "logout-output.txt")
  
    i <- 0
    pb <- tkProgressBar("test progress bar", "Some information in %",
                        0, 100, 50)
    
    for (simix in 1:length(transformado)){
      ####CONFIG####
      
      obsix <- tclvalue(obsix2)
      simix <- as.integer(transformado[simix])
      trendq <- as.numeric(tclvalue(trendq2))
      write <- as.numeric(tclvalue(write2))
      fname <- tclvalue(fname2)
      simlen <-as.numeric(tclvalue(simlen2))
      locate <-as.numeric(tclvalue(locate2))
      xval <- as.numeric(tclvalue(xval2))
      M <- as.numeric(tclvalue(M2))
      #z <-0
      trendmean<- as.numeric(tclvalue(trendmean2))
      trendsd<- as.numeric(tclvalue(trendsd2))
      ####END CONFIG#####
      bat <- paste("python simgen9s_cmip5.py ",obsix," ",simix," ",trendq," ", write," ",fname," ",simlen," ",locate," ",xval," ",M," ",trendmean," ",trendsd)
      write(bat, "correr.bat")
      log <- system2("correr.bat", stdout=TRUE)
      write(log, file = "logout-output.txt", append = TRUE)
      info <- sprintf("%d%% Completo", round(i/length(transformado)*100))
      i <- i+1
      setTkProgressBar(pb, round(i/length(transformado)*100), sprintf("Simgen (%s)", info), info)
      print(bat)
    }
    close(pb)
  }

  submit.but <- tkbutton(tt, text="Calcular", command=submit)
  
  
  tkgrid(tklabel(tt,text="Directorio"), x.entry, pady= 10, padx= 10)
  tkgrid(tklabel(tt,text="QuantSearch"),columnspan=3, pady = 10)
  tkgrid(tklabel(tt,text="Periodo"), y.entry, pady= 10, padx= 10)
  tkgrid(tklabel(tt,text="Percentil"), z.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="Simgen"),columnspan=3, pady = 5)
  
  
  tkgrid(tklabel(tt,text="obsix"), obsix2.entry, pady= 10, padx= 10)
  tkgrid(tklabel(tt,text="trendq"), trendq2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="write"), write2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="fname"), fname2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="simlen"), simlen2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="locate"), locate2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="xval"), xval2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="M"), M2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="trendmean"), trendmean2.entry, pady= 5, padx= 10)
  tkgrid(tklabel(tt,text="trendsd"), trendsd2.entry, pady= 5, padx= 10)
  

  tkgrid(submit.but, reset.but, pady= 10, padx= 10)
  
}
mydialog()