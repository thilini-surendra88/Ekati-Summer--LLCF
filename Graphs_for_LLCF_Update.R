##############################
### Graphs_for_LLCF_Update.R ####
#############################
############################


Observed.LLCF.graph <- function(param,p,syear,year,dataAll){ 

## Divide plotting area into 1 plots: with plot and Legend
layout(matrix(c(1,2),2,1,byrow=T),c(5,2),c(5))

###Plot 1- LLCF
##############
par(mar=c(1.5,4.5,2.5,1))

## labels for plot
Title <- as.character(param.list[p,'title'])

#ticks
Year.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
label.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
label.name <- as.character(format(label.tck,"%b-%y"))

#limits and ylabel
label1  <- as.character(param.list[p,'label'])

ymin1 <- 0
ymax1 <- param.list[p,'max']

#The LLCF data
##Observed Data
param <- as.character(param.list[p,'Variable.O'])


#Cell D- RW1
ind4 <- which(dataAll[,'LakeID']=='Cell D')
ind.RW1 <- which(dataAll[,'ID']=='RW1')
ind.open <- intersect(ind4,ind.RW1)

obs.plotdat <- dataAll[ind.open,]
x.date <- obs.plotdat[,'Date']
y.var <- obs.plotdat[,'Impute']

plot(x.date,y.var, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin1,ymax1),
     xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2,col='blue', pch=22)


##Make filled points for depth profiles used in calibration (e.g. LLD01 etc)
ind4 <- which(dataAll[,'LakeID']=='Cell D')
ind.D <- which(dataAll[,'ID']!='RW1')
ind.filled <- intersect(ind4,ind.D)
filled.points <- dataAll[ind.filled, c('Date','Impute')]

points(filled.points,col='blue',bg='blue',pch=22)

#Cell E
ind4 <- which(dataAll[,'LakeID']=='Cell E')
ind.E <- which(dataAll[,'ID']!='RW5')
ind.filled <- intersect(ind4,ind.E)

obs.plotdat <- dataAll[ind.filled,]
x.date <- obs.plotdat[,'Date']
y.var <- obs.plotdat[,'Impute']
points(x.date,y.var,col='red',bg='red',pch=23,cex=1)

##Make open circles for 1616-30 and RW5
ind4 <- which(dataAll[,'LakeID']=='161630 (LLCF)')
ind.RW5 <- which(dataAll[,'ID']=='RW5')
open.points1 <- dataAll[ind4, c('Date','Impute')]
open.points2 <- dataAll[ind.RW5, c('Date','Impute')]

points(open.points1,col='red',pch=23)
points(open.points2,col='red',pch=23)

#Cell C
ind.filled <- which(dataAll[,'LakeID']=='Cell C')

obs.plotdat <- dataAll[ind.filled,]
x.date <- obs.plotdat[,'Date']
y.var <- obs.plotdat[,'Impute']
points(x.date,y.var,col='green',bg='green',pch=24,cex=1)

#get current value
last.point <- length(obs.plotdat[,1])

CellC.current.date <-obs.plotdat[last.point,'Date']
CellC.current.K <- obs.plotdat[last.point,'Impute']

if(ymax1 > 10)
{axis(2,pos= as.Date(paste(syear,"01-01",sep="-")),at=pretty(ymin1:ymax1), labels= formatC(pretty(ymin1:ymax1),
   format="g", big.mark=',',digits=nchar(as.character(ymax1))),cex=.5,cex.axis=.85, las=2)
  ##Add Gridline
  ablineclip(h=pretty(ymin1:ymax1), x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  #ablineclip(h=pretty(ymin1:ymax1)/2, x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
}
if(ymax1 <= 10)
{axis(2,pos= as.Date(paste(syear,"01-01",sep="-")), cex=.5,cex.axis=0.85, las=2)
  segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)}



axis(1,pos= ymin1,at=Year.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1)
axis(1,pos= ymin1,at=label.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1,tck=-0.02)
axis(1,pos= ymin1,at=label.tck,labels=label.name,tick=F, cex=.5,cex.axis=.6,lwd.ticks=1)

## Add top and bottom
segments(as.Date(paste(syear,"01-01",sep="-")),ymax1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
segments(as.Date(paste(syear,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymin1)
segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)

#add EQC
EQC <- eqc.list[,param]
if(EQC=='calculate'){
  ###indicate hardness dependent in legend
  legend.EQC <- 'EQC not shown, hardness dependent'
 
}else {
  EQC <- as.numeric(as.character(EQC))
  par(xpd=FALSE)
  if (param=='Potassium'){
    segments(as.Date(paste(syear,"01-01",sep="-")),41,as.Date(paste(2018,"07-11",sep="-")),41,lty=2,col='black')
    segments(as.Date(paste(2018,"07-11",sep="-")),EQC,as.Date(paste(year+1,"01-01",sep="-")),EQC,lty=2,col='black')
    legend.EQC <- paste('EQC = ',EQC,' mg/L',sep='')
  }else{
    segments(as.Date(paste(syear,"01-01",sep="-")),EQC,as.Date(paste(year+1,"01-01",sep="-")),EQC,lty=2,col='black')
    legend.EQC <- paste('EQC = ',EQC,' mg/L',sep='')
  }
  
}

##add key dates
text1 <- 'Cell E to Leslie'
text2 <- 'Cell D to Cell E'
text3 <- 'Cell C to Cell D'

##2017 pumping
text(as.Date("2017-06-21"),0.87*ymax1,text1,adj=0,cex=0.7)
segments(as.Date("2017-06-21"),0.85*ymax1,as.Date("2017-07-27"),0.85*ymax1,col='red',lty=1,lwd=3)
segments(as.Date("2017-08-10"),0.85*ymax1,as.Date("2017-10-17"),0.85*ymax1,col='red',lty=1,lwd=3)

text(as.Date("2017-07-05"),0.82*ymax1,text2,adj=0,cex=0.7)
segments(as.Date("2017-07-05"),0.80*ymax1,as.Date("2017-07-27"),0.80*ymax1,col='blue',lty=1,lwd=3)

text(as.Date("2017-08-25"),0.77*ymax1,text3,adj=0,cex=0.7)
segments(as.Date("2017-08-25"),0.75*ymax1,as.Date("2017-10-8"),0.75*ymax1,col='green',lty=1,lwd=3)


##2018 pumping
text(as.Date("2018-06-01"),0.80*ymax1,text3,adj=0,cex=0.7)
segments(as.Date("2018-06-01"),0.77*ymax1,as.Date("2018-07-27"),0.77*ymax1,col='green',lty=1,lwd=3)

##2019 pumping
text(as.Date("2019-07-20"),0.87*ymax1,text1,adj=0,cex=0.7)
segments(as.Date("2019-07-20"),0.85*ymax1,as.Date("2019-08-20"),0.85*ymax1,col='red',lty=1,lwd=3)


mtext(label1, side=2, line= 2.5,adj= 0.5, font=2)



###2. Legend

par(mar=c(0.5,2,1.5,1))
plot.new()
plot.window(c(0,1), c(0,1));
box()
l.cex =0.7


text(x=0.05, y = 0.9,
     label = "LLCF Water Quality",
     cex = l.cex,
     adj = 0)
legend.names <- c('Cell C - Lake', 'Cell D - Lake', 'Cell E')
legend.col <- c('green','blue','red')
legend.pbg <- c('green','blue','red')
legend.pch <- c(24,22,23)

legend(x=0.02,y=.85,legend.names,pch=legend.pch,col= legend.col,pt.bg=legend.pbg, bty='n',cex = l.cex)


legend.names <- c('Cell D - RW1', 'Cell E - 1616-30 and RW5')
legend.col <- c('blue','red')
legend.pch <- c(22,23)
legend(x=0.2,y=.85,legend.names,pch=legend.pch,col= legend.col, bty='n',cex = l.cex)

##For EQC
if(EQC=='calculate'){
  ###indicate hardness dependent in legend
  text(x=0.2,y=.28,legend.EQC,cex = l.cex,adj = 0)
}else{  
  legend(x=0.2,y=.38,legend.EQC,lty=2,col= 'black', bty='n',cex = l.cex,lwd=1)
}

##For pumping volumes
text(x=0.8, y = 0.9,
     label = "Pumping From:",
     cex = l.cex,
     adj = 0)
legend.names <- c(text3, text2,text1)
legend.col <- c('green','blue','red')
legend.lty <- c(2,2)
legend(x=0.8,y=.85,legend.names,lty=legend.lty,col= legend.col, bty='n',cex = l.cex,lwd=3)



}

Observed.K.graph <- function(param,p,syear,year,dataAll,lake='Leslie'){ 
  
  ## Divide plotting area into 1 plots: with plot and Legend
  layout(matrix(c(1,2),2,1,byrow=T),c(5,2),c(5))
  
  ###Plot 1- LLCF
  ##############
  par(mar=c(1.5,4.5,2.5,1))
  
  ## labels for plot
  Title <- as.character(param.list[p,'title'])
  
  #ticks
  Year.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
  label.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
  label.name <- as.character(format(label.tck,"%b-%y"))
  
  #limits and ylabel
  label1  <- as.character(param.list[p,'label'])
  
  ymin1 <- 0
  ymax1 <- 70
  
  #The LLCF data
  ##Observed Data
  param <- as.character(param.list[p,'Variable.O'])
  
  
  #Select Leslie Lake
  ind.open <- which(dataAll[,'LakeID']==lake)
 
  obs.plotdat <- dataAll[ind.open,]
  x.date <- obs.plotdat[,'Date']
  y.var <- obs.plotdat[,'Impute']
  
  plot(x.date,y.var, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin1,ymax1),
       xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2,col='blue', pch=22)
  
  #Grid lines
  ablineclip(h=pretty(ymin1:ymax1), x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  #ablineclip(h=pretty(ymin1:ymax1)/2, x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  
  
  
  axis(2,pos= as.Date(paste(syear,"01-01",sep="-")),at=pretty(ymin1:ymax1), labels= formatC(pretty(ymin1:ymax1), format="g", big.mark=',',digits=nchar(as.character(ymax1))),cex=.5,cex.axis=.85, las=2)
  
  axis(1,pos= ymin1,at=Year.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1)
  axis(1,pos= ymin1,at=label.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1,tck=-0.02)
  axis(1,pos= ymin1,at=label.tck,labels=label.name,tick=F, cex=.5,cex.axis=.6,lwd.ticks=1)
  
  ## Add top and bottom
  segments(as.Date(paste(syear,"01-01",sep="-")),ymax1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  segments(as.Date(paste(syear,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymin1)
  segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  
  #add benchmark and add 50%, 70%, 90% lines
  benchmark.k <- 64
  ##add 50%, 70%, 90%
  text1 <- 'Benchmark'
  text2 <- '50%'
  text3 <- '70%'
  text4 <- '90%'
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.k,text1,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.k,as.Date(paste(year+1,"01-01",sep="-")),benchmark.k,col='black',lty=2, lwd=2)
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.k*0.9,text4,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.k*0.9,as.Date(paste(year+1,"01-01",sep="-")),benchmark.k*0.9,col='black',lty=2, lwd=2)
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.k*0.7,text3,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.k*0.7,as.Date(paste(year+1,"01-01",sep="-")),benchmark.k*0.7,col='black',lty=2, lwd=2)
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.k*0.5,text2,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.k*0.5,as.Date(paste(year+1,"01-01",sep="-")),benchmark.k*0.5,col='black',lty=2, lwd=2)
  
  #Add some open vs under ice season differentiation
  
  t.grey <- rgb(t(col2rgb("grey"))/255,alpha=0.3)
  
  rect(as.Date(paste(syear,"06-01",sep="-")),0,as.Date(paste(syear,"11-01",sep="-")),ymax1,col= t.grey)
  rect(as.Date(paste(syear+1,"06-01",sep="-")),0,as.Date(paste(syear+1,"11-01",sep="-")),ymax1,col= t.grey)
  rect(as.Date(paste(syear+2,"06-01",sep="-")),0,as.Date(paste(syear+2,"11-01",sep="-")),ymax1,col= t.grey)
  
  ##Add some year lines and text
  ##syear=2017
  text(as.Date(paste(syear,"06-15",sep="-")),ymax1*1.05,syear,adj=0,cex=1.2,xpd=NA)
  segments(as.Date(paste(syear+1,"01-01",sep="-")),ymin1,as.Date(paste(syear+1,"01-01",sep="-")),ymax1, lty = 3, col="gray",lwd=1.5)
  
  text(as.Date(paste(syear+1,"06-15",sep="-")),ymax1*1.05,syear+1,adj=0,cex=1.2,xpd=NA)
 
  segments(as.Date(paste(syear+2,"01-01",sep="-")),ymin1,as.Date(paste(syear+2,"01-01",sep="-")),ymax1, lty = 3, col="gray",lwd=1.5)
  
  text(as.Date(paste(syear+2,"06-15",sep="-")),ymax1*1.05,syear+2,adj=0,cex=1.2,xpd=NA)
  
  mtext(label1, side=2, line= 2.5,adj= 0.5, font=2)
  
  
}


Observed.Cl.graph <- function(param,p,syear,year,dataAll,lake='Leslie'){ 
  
  ## Divide plotting area into 1 plots: with plot and Legend
  layout(matrix(c(1,2),2,1,byrow=T),c(5,2),c(5))
  
  ###Plot 1- LLCF
  ##############
  par(mar=c(1.5,4.5,2.5,1))
  
  ## labels for plot
  Title <- as.character(param.list[p,'title'])
  
  #ticks
  Year.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
  label.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
  label.name <- as.character(format(label.tck,"%b-%y"))
  
  #limits and ylabel
  label1  <- as.character(param.list[p,'label'])
  
  ymin1 <- 0
  ymax1 <- 400
  
  #The LLCF data
  ##Observed Data
  param <- as.character(param.list[p,'Variable.O'])
  
  
  #Select  Lake
  ind.open <- which(dataAll[,'LakeID']==lake)
  
  obs.plotdat <- dataAll[ind.open,]
  x.date <- obs.plotdat[,'Date']
  y.var <- obs.plotdat[,'Impute']
  
  plot(x.date,y.var, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin1,ymax1),
       xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2,col='blue', pch=22)
  
  #Grid lines
  #ablineclip(h=pretty(ymin1:ymax1), x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  ablineclip(h=pretty(ymin1:ymax1)/2, x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  
  
  
  axis(2,pos= as.Date(paste(syear,"01-01",sep="-")),at=pretty(ymin1:ymax1), labels= formatC(pretty(ymin1:ymax1), format="g", big.mark=',',digits=nchar(as.character(ymax1))),cex=.5,cex.axis=.85, las=2)
  
  axis(1,pos= ymin1,at=Year.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1)
  axis(1,pos= ymin1,at=label.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1,tck=-0.02)
  axis(1,pos= ymin1,at=label.tck,labels=label.name,tick=F, cex=.5,cex.axis=.6,lwd.ticks=1)
  
  ## Add top and bottom
  segments(as.Date(paste(syear,"01-01",sep="-")),ymax1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  segments(as.Date(paste(syear,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymin1)
  segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  
  #add benchmark 
  ###adding a date to the hardness vector for plotting
  hardness <- obs.plotdat[,c('Hardness')]
  p.hardness <- as.data.frame(cbind(as.Date(obs.plotdat[,'Date'], "%m/%d/%Y"),hardness))
  names(p.hardness) <- c('Date','hardness')
  benchmark.val <- calculate.benchmarks (param,p.hardness,lake)
  par(xpd=FALSE)
  lines(benchmark.val[,c(1,3)],lty=2,lwd=2,col='black')
  lines(benchmark.val[,c(1)],benchmark.val[,c(3)]*.7,lty=2,lwd=2,col='black')
  lines(benchmark.val[,c(1)],benchmark.val[,c(3)]*.5,lty=2,lwd=2,col='black')
  
  ##add 50%, 70%, 100%
  text1 <- 'Benchmark'
  text2 <- '50%'
  text3 <- '70%'
  
  
  text(as.Date(paste(year,"05-15",sep="-")),max(benchmark.val[,c(3)]),text1,adj=0,cex=0.7)
  text(as.Date(paste(year,"05-15",sep="-")),max(benchmark.val[,c(3)]*0.7),text3,adj=0,cex=0.7)
  text(as.Date(paste(year,"05-15",sep="-")),max(benchmark.val[,c(3)]*0.5),text2,adj=0,cex=0.7)
  
  mtext(label1, side=2, line= 2.5,adj= 0.5, font=2)
  
  #Add some open vs under ice season differentiation
  
  t.grey <- rgb(t(col2rgb("grey"))/255,alpha=0.3)
  
  rect(as.Date(paste(syear,"06-01",sep="-")),0,as.Date(paste(syear,"11-01",sep="-")),ymax1,col= t.grey)
  rect(as.Date(paste(syear+1,"06-01",sep="-")),0,as.Date(paste(syear+1,"11-01",sep="-")),ymax1,col= t.grey)
  rect(as.Date(paste(syear+2,"06-01",sep="-")),0,as.Date(paste(syear+2,"11-01",sep="-")),ymax1,col= t.grey)
  
  ##Add some year lines and text
  ##syear=2017
  text(as.Date(paste(syear,"06-15",sep="-")),ymax1*1.05,syear,adj=0,cex=1.2,xpd=NA)
  segments(as.Date(paste(syear+1,"01-01",sep="-")),ymin1,as.Date(paste(syear+1,"01-01",sep="-")),ymax1, lty = 3, col="gray",lwd=1.5)
  
  text(as.Date(paste(syear+1,"06-15",sep="-")),ymax1*1.05,syear+1,adj=0,cex=1.2,xpd=NA)
  
  segments(as.Date(paste(syear+2,"01-01",sep="-")),ymin1,as.Date(paste(syear+2,"01-01",sep="-")),ymax1, lty = 3, col="gray",lwd=1.5)
  
  text(as.Date(paste(syear+2,"06-15",sep="-")),ymax1*1.05,syear+2,adj=0,cex=1.2,xpd=NA)
  
  
  
}

Observed.Se.graph <- function(param,p,syear,year,dataAll,lake='Leslie'){ 
  
  ## Divide plotting area into 1 plots: with plot and Legend
  layout(matrix(c(1,2),2,1,byrow=T),c(5,2),c(5))
  
  ###Plot 1- LLCF
  ##############
  par(mar=c(1.5,4.5,2.5,1))
  
  ## labels for plot
  Title <- as.character(param.list[p,'title'])
  
  #ticks
  Year.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
  label.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "month")
  label.name <- as.character(format(label.tck,"%b-%y"))
  
  #limits and ylabel
  label1  <- as.character(param.list[p,'label'])
  
  ymin1 <- 0
  ymax1 <- 0.002
  
  #The LLCF data
  ##Observed Data
  param <- as.character(param.list[p,'Variable.O'])
  
  
  #Select Leslie Lake
  ind.open <- which(dataAll[,'LakeID']==lake)
  
  obs.plotdat <- dataAll[ind.open,]
  x.date <- obs.plotdat[,'Date']
  y.var <- obs.plotdat[,'Impute']
  
  plot(x.date,y.var, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin1,ymax1),
       xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2,col='blue', pch=22)
  
  
  if(ymax1 > 10)
  {axis(2,pos= as.Date(paste(syear,"01-01",sep="-")),at=pretty(ymin1:ymax1), labels= formatC(pretty(ymin1:ymax1),
                                                                                             format="g", big.mark=',',digits=nchar(as.character(ymax1))),cex=.5,cex.axis=.85, las=2)
    ##Add Gridline
    ablineclip(h=pretty(ymin1:ymax1), x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
    #ablineclip(h=pretty(ymin1:ymax1)/2, x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  }
  if(ymax1 <= 10)
  {axis(2,pos= as.Date(paste(syear,"01-01",sep="-")), cex=.5,cex.axis=0.85, las=2)
    segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)}
  
  
  axis(1,pos= ymin1,at=Year.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1)
  axis(1,pos= ymin1,at=label.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1,tck=-0.02)
  axis(1,pos= ymin1,at=label.tck,labels=label.name,tick=F, cex=.5,cex.axis=.6,lwd.ticks=1)
  
  ## Add top and bottom
  segments(as.Date(paste(syear,"01-01",sep="-")),ymax1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  segments(as.Date(paste(syear,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymin1)
  segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  
  #add benchmark and add 50%, 70%, 90% lines
  benchmark.Se <- 0.001
  ##add 50%, 70%, 90%
  text1 <- 'Benchmark'
  text2 <- '50%'
  text3 <- '70%'
  text4 <- '90%'
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.Se,text1,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.Se,as.Date(paste(year+1,"01-01",sep="-")),benchmark.Se,col='black',lty=2, lwd=2)
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.Se*0.9,text4,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.Se*0.9,as.Date(paste(year+1,"01-01",sep="-")),benchmark.Se*0.9,col='black',lty=2, lwd=2)
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.Se*0.7,text3,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.Se*0.7,as.Date(paste(year+1,"01-01",sep="-")),benchmark.Se*0.7,col='black',lty=2, lwd=2)
  
  text(as.Date(paste(syear,"01-15",sep="-")),1.03*benchmark.Se*0.5,text2,adj=0,cex=0.7)
  segments(as.Date(paste(syear,"01-01",sep="-")),benchmark.Se*0.5,as.Date(paste(year+1,"01-01",sep="-")),benchmark.Se*0.5,col='black',lty=2, lwd=2)
  
  #Add some open vs under ice season differentiation
  
  t.grey <- rgb(t(col2rgb("grey"))/255,alpha=0.3)
  
  rect(as.Date(paste(syear,"06-01",sep="-")),0,as.Date(paste(syear,"11-01",sep="-")),ymax1,col= t.grey)
  rect(as.Date(paste(syear+1,"06-01",sep="-")),0,as.Date(paste(syear+1,"11-01",sep="-")),ymax1,col= t.grey)
  rect(as.Date(paste(syear+2,"06-01",sep="-")),0,as.Date(paste(syear+2,"11-01",sep="-")),ymax1,col= t.grey)
  
  ##Add some year lines and text
  ##syear=2017
  text(as.Date(paste(syear,"06-15",sep="-")),ymax1*1.05,syear,adj=0,cex=1.2,xpd=NA)
  segments(as.Date(paste(syear+1,"01-01",sep="-")),ymin1,as.Date(paste(syear+1,"01-01",sep="-")),ymax1, lty = 3, col="gray",lwd=1.5)
  
  text(as.Date(paste(syear+1,"06-15",sep="-")),ymax1*1.05,syear+1,adj=0,cex=1.2,xpd=NA)
  
  segments(as.Date(paste(syear+2,"01-01",sep="-")),ymin1,as.Date(paste(syear+2,"01-01",sep="-")),ymax1, lty = 3, col="gray",lwd=1.5)
  
  text(as.Date(paste(syear+2,"06-15",sep="-")),ymax1*1.05,syear+2,adj=0,cex=1.2,xpd=NA)
  
  mtext(label1, side=2, line= 2.5,adj= 0.5, font=2)
  
  
}

Cell.current <- function(dataAll,lakeid,id){
  
  ind4 <- which(dataAll[,'LakeID']==lakeid)
  ind.RW1 <- which(dataAll[,'ID']==id)
  ind.open <- intersect(ind4,ind.RW1)
  
  obs.plotdat <- dataAll[ind.open,]
  
  md <- obs.plotdat[order(as.Date(obs.plotdat$Date, format="%m/%d/%Y")),]
  current.date <- as.Date(md[nrow(md),'Date'], "%m/%d/%Y")
  
  ##average if multiple samples
  ind.m <- which(obs.plotdat[,'Date']==current.date)
  
  current.impute <- lapply(mean(obs.plotdat[ind.m,'Impute']),signif,3)
  
 
  
  n <- length(ind.m)
  
  output <- cbind(current.date, current.impute, n)
  
  return(output)
  
 
 
}

Beartooth.current <- function(datain.b,param,stn){
  
  obs.plotdat <- getData3(datain.b,param,stn)
  
  last.point <- length(obs.plotdat[,1])
  current.date <- obs.plotdat[last.point,'Date']
  current.K <- obs.plotdat[last.point,'Impute']
  
  output <-cbind(current.date, current.K)
  return(output) 
}

Reclaim.current <- function(datain.b,param,stn){
  
  obs.plotdat <- getData3(datain.b,param,stn)
  
  last.point <- length(obs.plotdat[,1])
  current.date <- obs.plotdat[last.point,'Date']
  current.K <- obs.plotdat[last.point,'Impute']
  
  output <-cbind(current.date, current.K)
  return(output)
}

CellE.profile <- function(datain.profile,fromDate,toDate){

#################################
layout(matrix(c(1,2,3,4),2,2,byrow=T),c(5,3),c(4,5))##just the profiles only
##### Extract data #####

y.label <- 'Field Specific Conductivity (uS/cm)'

oma=c(0,0,0,1)

##plot #1-Cell E - Specific Conductivity Profile
par(mar=c(2,5,4,0))

## Values for plot
x.label <- y.label
xmin <- 0
xmax <- 2000


##make the plot
# Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(20,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)

if(xmax <= 1000){
  axis(3,cex=.5,cex=.5,cex.axis=0.85)
}else{
  axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}

axis(side=2,las=2,cex=.5,cex.axis=0.85)
mtext(x.label,side=3,line=2)
mtext('Depth (m)',side=2,line=3)


##Add specific Conductivity

plotdat.all<- datain.profile[, c('Station','Date','Depth.m','SpCond')]


##Make sure data is in correct month/day/year format
plotdat.all[,'Date'] <- as.Date(plotdat.all[,'Date'], "%m/%d/%Y")
##plot only date range selected
ind.dates <- which(plotdat.all[,'Date'] >= fromDate & plotdat.all[,'Date'] <= toDate)
plotdat <- plotdat.all[ind.dates,]
#plotdat <- subset(plotdat.all, format(as.Date(Date),"%Y")==2018)

ind5 <- which(plotdat[,'Station'] == 'CellE'|plotdat[,'Station'] == 'CellE07'|plotdat[,'Station'] == 'CellE05'|plotdat[,'Station'] == 'CellE03')
select.plotdat <- plotdat[ind5,]
dates <- unique(select.plotdat[,'Date'])

PlotColours<-timPalette(n=length(unique(dates)))
line.types <- c(2,1,3)
line.type.l <- NULL
dates.l <- structure(integer(), class = "Date")
line.colours.l <- NULL
stations.l <- NULL

for (sd in 1:length(dates)){
  ind.d <- which(select.plotdat[,'Date']==dates[sd])
  ind.s <- which(select.plotdat[,'Station'] == 'CellE'|select.plotdat[,'Station'] == 'CellE07')
  ind <- intersect(ind.d,ind.s)
  
  if(length(ind)!=0){
    lines(select.plotdat[ind,'SpCond'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
    
    line.type.l <- c(line.type.l,line.types[1])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    stations.l <- c(stations.l,'LLE07')
  }
  
  ind.ss <- which(select.plotdat[,'Station'] == 'CellE05')
  ind.n <- intersect(ind.d,ind.ss)
  if(length(ind.n)!=0){
    lines(select.plotdat[ind.n,'SpCond'],select.plotdat[ind.n,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[2])
    
    line.type.l <- c(line.type.l,line.types[2])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    stations.l <- c(stations.l,'LLE05')
  }
  
  ind.sss <- which(select.plotdat[,'Station'] == 'CellE03')
  ind.nn <- intersect(ind.d,ind.sss)
  
  if(length(ind.nn)!=0){
    lines(select.plotdat[ind.nn,'SpCond'],select.plotdat[ind.nn,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[3])
    
    line.type.l <- c(line.type.l,line.types[3])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    stations.l <- c(stations.l,'LLE03')
    
  }
  
  
}



##PLOT legend
plot.new()
par(mar=c(2,0,4,1))

plot.window(c(0,1), c(0,1));
box()
text(0.03,0.995,'Cell E - Field Specific Conductivity', cex=1.2,adj=0,font = 2)
legcex <- .7

## Legend for Conductivity

legend.txt2 <- paste(as.character(format(dates.l,"%b %d, %Y")),stations.l,sep='; ')

text(0.08,0.95,"Profile Date", cex=0.9,adj=0)
#1-25
legend(x=.08,y=.9,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
       cex=legcex,bty='n')
#26+
#ll <- length(legend.txt2)
#legend(x=.5,y=.9,legend.txt2[26:ll],lty=1,col=PlotColours.p[26:ll],lwd=2,
#      cex=legcex,bty='n')

##plot #1-Cell D
par(mar=c(2,5,4,0))
## Values for plot
x.label <- 'Calculated Potassium (mg/L)'
xmin <- 0
xmax <- 80


##make the plot
# Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(20,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)

if(xmax <= 1000){
  axis(3,cex=.5,cex=.5,cex.axis=0.85)
}else{
  axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}

axis(side=2,las=2,cex=.5,cex.axis=0.85)
mtext(x.label,side=3,line=2)
mtext('Depth (m)',side=2,line=3)


#Calculate Potassium
select.plotdat[,'Potassium.field']<- datain.rel[3,'Intercept'] +  (datain.rel[3,'m']*select.plotdat[,'SpCond'])

avg.pred.p <- NULL
for (sd in 1:length(dates)){
  ind.d <- which(select.plotdat[,'Date']==dates[sd])
  ind.s <- which(select.plotdat[,'Station'] == 'CellE'|select.plotdat[,'Station'] == 'CellE07')
  ind <- intersect(ind.d,ind.s)
  
  if(length(ind)!=0){
    lines(select.plotdat[ind,'Potassium.field'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
    average <- mean(select.plotdat[ind,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
  }
  
  ind.ss <- which(select.plotdat[,'Station'] == 'CellE05')
  ind.n <- intersect(ind.d,ind.ss)
  
  if(length(ind.n)!=0){
    lines(select.plotdat[ind.n,'Potassium.field'],select.plotdat[ind.n,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[2])
    average <- mean(select.plotdat[ind.n,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
    
    
  }
  
  ind.sss <- which(select.plotdat[,'Station'] == 'CellE03')
  ind.nn <- intersect(ind.d,ind.sss)
  
  if(length(ind.nn)!=0){
    lines(select.plotdat[ind.nn,'Potassium.field'],select.plotdat[ind.nn,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[3])
    average <- mean(select.plotdat[ind.nn,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
    
    
    
  }
  
  
}





##Add EQC
lines(rep(53,length(c(0:20))),c(0:20),lty=3,col='red')
text(53.5,4.5,'EQC = 53 mg/L (July 11)',cex=0.8,adj=0)

##Add Observed points
##Field Specific Conductivity
field[,'Date'] <- as.Date(field[,'Date'], "%m/%d/%Y")

ind.y <-  which(as.numeric(field[,'Date'],"%m/%d/%Y")>= 17329)
ind.D <- which(field[,'LakeID']=='Cell E')
ind.YD <- intersect(ind.y,ind.D)

##Observed Data- Potassium
##TO ADD WHEN PUMPING OCCURS
#obs.K <- field[ind.YD,c('Date','Potassium')]

#obs.plotdat.K <- subset(obs.K , format(as.Date(Date),"%Y")==2018)

#dates.p <- unique(obs.plotdat.K[,'Date'])
#PlotColours.p<-timPalette(n=length(dates.p))

#for (sd in 1:length(dates.p)){
#  ind <- which(obs.plotdat.K[,'Date']==dates.p[sd])
#  points(obs.plotdat.K[ind,'Potassium'],rep(0.1,length(ind)),col=PlotColours.p[ind], pch=22)
#}


##PLOT legend1
plot.new()
par(mar=c(2,0,4,1))

plot.window(c(0,1), c(0,1));
box()
text(0.08,0.999,'Cell E', cex=1.2,adj=0.5,font = 2)
legcex <- .7
eq3 <- paste0("Potassium = ", datain.rel[3,'Intercept'],
              ifelse(sign(datain.rel[3,'m'])==1, " + ", " - "), abs(datain.rel[3,'m']), " [SpCond-Field] ")

#legend.txt <- c('Lab Conductivity Relationship','Field Conductivity Relationship','Field Specific Conductivity Relationship')
#legend(x=.03,y=.975,legend.txt[3],lty=c(1),col='black',lwd=2,
#        cex=legcex,bty='n')
text(0.03,0.95,eq3, cex=legcex,adj=0)
## Legend for Profile dates


avg.pred.p <- round(avg.pred.p,1)

legend.potassium <- paste(c(avg.pred.p),"mg/L",sep =" ")
legend.dates <- as.character(format(dates.l,"%b %d, %Y"))

legend.txt2 <- paste(stations.l,"; ",legend.dates,"; Avg = ",legend.potassium,sep= "")



text(0.08,0.91,"Profile Date", cex=0.8,adj=0)

legend(x=.08,y=.85,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
       cex=legcex,bty='n')

}

#########################################
#########################################

CellD.profile <- function(datain.profile,fromDate,toDate){


layout(matrix(c(1,2,3,4),2,2,byrow=T),c(5,3),c(4,5))##just the profiles only
##### Extract data #####

y.label <- 'Field Specific Conductivity (uS/cm)'

oma=c(0,0,0,1)

##plot #1-Cell E - Specific Conductivity Profile
par(mar=c(2,5,4,0))

## Values for plot
x.label <- y.label
xmin <- 0
xmax <- 2000


##make the plot
# Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(30,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)

if(xmax <= 1000){
  axis(3,cex=.5,cex=.5,cex.axis=0.85)
}else{
  axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}

axis(side=2,las=2,cex=.5,cex.axis=0.85)
mtext(x.label,side=3,line=2)
mtext('Depth (m)',side=2,line=3)


##Add specific Conductivity

plotdat.all<- datain.profile[, c('Station','Date','Depth.m','SpCond')]


##Make sure data is in correct month/day/year format
plotdat.all[,'Date'] <- as.Date(plotdat.all[,'Date'], "%m/%d/%Y")
##plot only date range selected
ind.dates <- which(plotdat.all[,'Date'] >= fromDate & plotdat.all[,'Date'] <= toDate)
plotdat <- plotdat.all[ind.dates,]
#plotdat <- subset(plotdat.all, format(as.Date(Date),"%Y")==2018)


ind5 <- which(plotdat[,'Station'] == 'CellD'|plotdat[,'Station'] == 'CellD01'|plotdat[,'Station'] == 'CellD05'|plotdat[,'Station'] == 'CellD03'|plotdat[,'Station'] == 'CellD04')
select.plotdat <- plotdat[ind5,]
dates <- unique(select.plotdat[,'Date'])

PlotColours<-timPalette(n=length(unique(dates)))
line.types <- c(1,2,3,4)
line.type.l <- NULL
dates.l <- structure(integer(), class = "Date")
line.colours.l <- NULL
stations.l <- NULL

for (sd in 1:length(dates)){
  ind.d <- which(select.plotdat[,'Date']==dates[sd])
  ind.s <- which(select.plotdat[,'Station'] == 'CellD'|select.plotdat[,'Station'] == 'CellD04')
  ind <- intersect(ind.d,ind.s)
  lines(select.plotdat[ind,'SpCond'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
  
  line.type.l <- c(line.type.l,line.types[1])
  dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
  line.colours.l <- c(line.colours.l,PlotColours[sd])
  stations.l <- c(stations.l,'LLD04')
  
  ind.ss <- which(select.plotdat[,'Station'] == 'CellD03')
  ind.n <- intersect(ind.d,ind.ss)
  if(length(ind.n)!=0){
    lines(select.plotdat[ind.n,'SpCond'],select.plotdat[ind.n,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[2])
    
    line.type.l <- c(line.type.l,line.types[2])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    stations.l <- c(stations.l,'LLD03')
  }
  
  ind.sss <- which(select.plotdat[,'Station'] == 'CellD05')
  ind.nn <- intersect(ind.d,ind.sss)
  
  if(length(ind.nn)!=0){
    lines(select.plotdat[ind.nn,'SpCond'],select.plotdat[ind.nn,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[3])
    
    line.type.l <- c(line.type.l,line.types[3])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    stations.l <- c(stations.l,'LLD05')
  }
  
  ind.ssss <- which(select.plotdat[,'Station'] == 'CellD01')
  ind.nnn <- intersect(ind.d,ind.ssss)
  
  if(length(ind.nnn)!=0){
    lines(select.plotdat[ind.nnn,'SpCond'],select.plotdat[ind.nnn,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[4])
    
    line.type.l <- c(line.type.l,line.types[3])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    stations.l <- c(stations.l,'LLD01')
    
  }
  
  
}



##PLOT legend
plot.new()
par(mar=c(2,0,4,1))

plot.window(c(0,1), c(0,1));
box()
text(0.03,0.995,'Cell D - Field Specific Conductivity', cex=1.2,adj=0,font = 2)
legcex <- .7

## Legend for Conductivity

legend.txt2 <- paste(stations.l,as.character(format(dates.l,"%b %d, %Y")),sep="; ")

text(0.08,0.95,"Profile Date", cex=0.9,adj=0)
#1-25
legend(x=.08,y=.9,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
       cex=legcex,bty='n')
#26+
#ll <- length(legend.txt2)
#legend(x=.5,y=.9,legend.txt2[26:ll],lty=1,col=PlotColours.p[26:ll],lwd=2,
#      cex=legcex,bty='n')

##plot #1-Cell D
par(mar=c(2,5,4,0))
## Values for plot
x.label <- 'Calculated Potassium (mg/L)'
xmin <- 0
xmax <- 80


##make the plot
# Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(30,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)

if(xmax <= 1000){
  axis(3,cex=.5,cex=.5,cex.axis=0.85)
}else{
  axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}

axis(side=2,las=2,cex=.5,cex.axis=0.85)
mtext(x.label,side=3,line=2)
mtext('Depth (m)',side=2,line=3)


#Calculate Potassium
select.plotdat[,'Potassium.field']<- datain.rel[3,'Intercept'] +  (datain.rel[3,'m']*select.plotdat[,'SpCond'])

avg.pred.p <- NULL
for (sd in 1:length(dates)){
  ind.d <- which(select.plotdat[,'Date']==dates[sd])
  ind.s <- which(select.plotdat[,'Station'] == 'CellD'|select.plotdat[,'Station'] == 'CellD04')
  ind <- intersect(ind.d,ind.s)
  
  lines(select.plotdat[ind,'Potassium.field'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
  average <- mean(select.plotdat[ind,'Potassium.field'])
  avg.pred.p <- c(avg.pred.p,average)
  
  
  ind.ss <- which(select.plotdat[,'Station'] == 'CellD03')
  ind.n <- intersect(ind.d,ind.ss)
  
  if(length(ind.n)!=0){
    lines(select.plotdat[ind.n,'Potassium.field'],select.plotdat[ind.n,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[2])
    average <- mean(select.plotdat[ind.n,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
    
    
  }
  
  ind.sss <- which(select.plotdat[,'Station'] == 'CellD05')
  ind.nn <- intersect(ind.d,ind.sss)
  
  if(length(ind.nn)!=0){
    lines(select.plotdat[ind.nn,'Potassium.field'],select.plotdat[ind.nn,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[3])
    average <- mean(select.plotdat[ind.nn,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
    
    
    
  }
  
  ind.ssss <- which(select.plotdat[,'Station'] == 'CellD01')
  ind.nnn <- intersect(ind.d,ind.ssss)
  
  if(length(ind.nnn)!=0){
    lines(select.plotdat[ind.nnn,'Potassium.field'],select.plotdat[ind.nnn,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[4])
    average <- mean(select.plotdat[ind.nnn,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
    
    
    
  }
  
  
}





##Add EQC
#lines(rep(41,length(c(0:20))),c(0:20),lty=3,col='red')
#text(41.5,4.5,'EQC = 41 mg/L',cex=0.8,adj=0)

##Add Observed points
##Field Specific Conductivity
field[,'Date'] <- as.Date(field[,'Date'], "%m/%d/%Y")

ind.y <-  which(as.numeric(field[,'Date'],"%m/%d/%Y")>= 17329)
ind.D <- which(field[,'LakeID']=='Cell E')
ind.YD <- intersect(ind.y,ind.D)

##Observed Data- Potassium
##TO ADD WHEN PUMPING OCCURS
#obs.K <- field[ind.YD,c('Date','Potassium')]

#obs.plotdat.K <- subset(obs.K , format(as.Date(Date),"%Y")==2018)

#dates.p <- unique(obs.plotdat.K[,'Date'])
#PlotColours.p<-timPalette(n=length(dates.p))

#for (sd in 1:length(dates.p)){
#  ind <- which(obs.plotdat.K[,'Date']==dates.p[sd])
#  points(obs.plotdat.K[ind,'Potassium'],rep(0.1,length(ind)),col=PlotColours.p[ind], pch=22)
#}


##PLOT legend1
plot.new()
par(mar=c(2,0,4,1))

plot.window(c(0,1), c(0,1));
box()
text(0.08,0.999,'Cell D', cex=1.2,adj=0.5,font = 2)
legcex <- .7
eq3 <- paste0("Potassium = ", datain.rel[3,'Intercept'],
              ifelse(sign(datain.rel[3,'m'])==1, " + ", " - "), abs(datain.rel[3,'m']), " [SpCond-Field] ")

#legend.txt <- c('Lab Conductivity Relationship','Field Conductivity Relationship','Field Specific Conductivity Relationship')
#legend(x=.03,y=.975,legend.txt[3],lty=c(1),col='black',lwd=2,
#        cex=legcex,bty='n')

text(0.03,0.95,eq3, cex=legcex,adj=0)
## Legend for Profile dates


avg.pred.p <- round(avg.pred.p,1)

legend.potassium <- paste(c(avg.pred.p),"mg/L",sep =" ")
legend.dates <- as.character(format(dates.l,"%b %d, %Y"))

legend.txt2 <- paste(stations.l,"; ",legend.dates,"; Avg = ",legend.potassium,sep= "")



text(0.08,0.91,"Profile Date", cex=0.9,adj=0)

legend(x=.08,y=.85,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
       cex=legcex,bty='n')

}

#################################
################################

CellC.profile <- function(datain.profile,fromDate,toDate){
  layout(matrix(c(1,2,3,4),2,2,byrow=T),c(5,3),c(4,5))##just the profiles only
  ##### Extract data #####
  
  y.label <- 'Field Specific Conductivity (uS/cm)'
  
  oma=c(0,0,0,1)
  
  ##plot #1-Cell C - Specific Conductivity Profile
  par(mar=c(2,5,4,0))
  
  ## Values for plot
  x.label <- y.label
  xmin <- 0
  xmax <- 2000
  
  
  ##make the plot
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(5,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  if(xmax <= 1000){
    axis(3,cex=.5,cex=.5,cex.axis=0.85)
  }else{
    axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}
  
  axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2)
  mtext('Depth (m)',side=2,line=3)
  
  
  ##Add specific Conductivity
  
  plotdat.all<- datain.profile[, c('Station','Date','Depth.m','SpCond')]
  
  
  ##Make sure data is in correct month/day/year format
  plotdat.all[,'Date'] <- as.Date(plotdat.all[,'Date'], "%m/%d/%Y")
  ##plot only date range selected
  ind.dates <- which(plotdat.all[,'Date'] >= fromDate & plotdat.all[,'Date'] <= toDate)
  plotdat <- plotdat.all[ind.dates,]
  #plotdat <- subset(plotdat.all, format(as.Date(Date),"%Y")==2018)
  
  
  ind5 <- which(plotdat[,'Station'] == 'CellC')
  select.plotdat <- plotdat[ind5,]
  dates <- unique(select.plotdat[,'Date'])
  
  PlotColours<-timPalette(n=length(unique(dates)))
  line.types <- c(1,2,3)
  line.type.l <- NULL
  dates.l <- structure(integer(), class = "Date")
  line.colours.l <- NULL
  stations.l <- NULL
  
  for (sd in 1:length(dates)){
    ind.d <- which(select.plotdat[,'Date']==dates[sd])
    ind.s <- which(select.plotdat[,'Station'] == 'CellC')
    ind <- intersect(ind.d,ind.s)
    
    if(length(ind)!=0){
      lines(select.plotdat[ind,'SpCond'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
      
      line.type.l <- c(line.type.l,line.types[1])
      dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
      line.colours.l <- c(line.colours.l,PlotColours[sd])
      stations.l <- c(stations.l,'')
    }
    
  }
  
  
  ##PLOT legend
  plot.new()
  par(mar=c(2,0,4,1))
  
  plot.window(c(0,1), c(0,1));
  box()
  text(0.03,0.995,'Cell C - Field Specific Conductivity', cex=1.2,adj=0,font = 2)
  legcex <- .7
  
  ## Legend for Conductivity
  
  legend.txt2 <- paste(as.character(format(dates.l,"%b %d, %Y")),stations.l,sep='')
  
  text(0.08,0.95,"Profile Date", cex=0.9,adj=0)
  #1-25
  legend(x=.08,y=.9,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
         cex=legcex,bty='n')
  #26+
  #ll <- length(legend.txt2)
  #legend(x=.5,y=.9,legend.txt2[26:ll],lty=1,col=PlotColours.p[26:ll],lwd=2,
  #      cex=legcex,bty='n')
  
  ##plot #1-Cell C
  par(mar=c(2,5,4,0))
  ## Values for plot
  x.label <- 'Calculated Potassium (mg/L)'
  xmin <- 0
  xmax <- 80
  
  
  ##make the plot
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(5,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  if(xmax <= 1000){
    axis(3,cex=.5,cex=.5,cex.axis=0.85)
  }else{
    axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}
  
  axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2)
  mtext('Depth (m)',side=2,line=3)
  
  
  #Calculate Potassium
  select.plotdat[,'Potassium.field']<- datain.rel[3,'Intercept'] +  (datain.rel[3,'m']*select.plotdat[,'SpCond'])
  
  avg.pred.p <- NULL
  for (sd in 1:length(dates)){
    ind.d <- which(select.plotdat[,'Date']==dates[sd])
    ind.s <- which(select.plotdat[,'Station'] == 'CellC')
    ind <- intersect(ind.d,ind.s)
    
    if(length(ind)!=0){
      lines(select.plotdat[ind,'Potassium.field'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
      average <- mean(select.plotdat[ind,'Potassium.field'])
      avg.pred.p <- c(avg.pred.p,average)
    }
    
    
  }
  
  
  ##PLOT legend1
  plot.new()
  par(mar=c(2,0,4,1))
  
  plot.window(c(0,1), c(0,1));
  box()
  text(0.08,0.999,'Cell C', cex=1.2,adj=0.5,font = 2)
  legcex <- .7
  eq3 <- paste0("Potassium = ", datain.rel[3,'Intercept'],
                ifelse(sign(datain.rel[3,'m'])==1, " + ", " - "), abs(datain.rel[3,'m']), " [SpCond-Field] ")
  
  #legend.txt <- c('Lab Conductivity Relationship','Field Conductivity Relationship','Field Specific Conductivity Relationship')
  #legend(x=.03,y=.975,legend.txt[3],lty=c(1),col='black',lwd=2,
  #        cex=legcex,bty='n')
  text(0.03,0.95,eq3, cex=legcex,adj=0)
  ## Legend for Profile dates
  
  
  avg.pred.p <- round(avg.pred.p,1)
  
  legend.potassium <- paste(c(avg.pred.p),"mg/L",sep =" ")
  legend.dates <- as.character(format(dates.l,"%b %d, %Y"))
  
  legend.txt2 <- paste(legend.dates,"; Avg = ",legend.potassium,sep= "")
  
  
  
  text(0.08,0.91,"Profile Date", cex=0.8,adj=0)
  
  legend(x=.08,y=.85,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
         cex=legcex,bty='n')
}

#####################################
###################################

Leslie.profile <- function(datain.profile,fromDate,toDate){
  
  layout(matrix(c(1,2,3,4),2,2,byrow=T),c(5,3),c(4,5))##just the profiles only
  
  ##### Extract data #####
  
  y.label <- 'Field Specific Conductivity (uS/cm)'
  
  oma=c(0,0,0,1)
  
  ##plot #1-Cell E - Specific Conductivity Profile
  par(mar=c(2,5,4,0))
  
  ## Values for plot
  x.label <- y.label
  xmin <- 0
  xmax <- 2000
  
  
  ##make the plot
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(15,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  if(xmax <= 1000){
    axis(3,cex=.5,cex=.5,cex.axis=0.85)
  }else{
    axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}
  
  axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2)
  mtext('Depth (m)',side=2,line=3)
  
  
  ##Add specific Conductivity
  
  plotdat.all<- datain.profile[, c('Station','Date','Depth.m','SpCond')]
  
  
  ##Make sure data is in correct month/day/year format
  plotdat.all[,'Date'] <- as.Date(plotdat.all[,'Date'], "%m/%d/%Y")
  ##plot only date range selected
  ind.dates <- which(plotdat.all[,'Date'] >= fromDate & plotdat.all[,'Date'] <= toDate)
  plotdat <- plotdat.all[ind.dates,]
  #plotdat <- subset(plotdat.all, format(as.Date(Date),"%Y")==2018)
  
  
  ind5 <- which(plotdat[,'Station'] == 'Leslie')
  select.plotdat <- plotdat[ind5,]
  dates <- unique(select.plotdat[,'Date'])
  
  PlotColours<-timPalette(n=length(unique(dates)))
  line.types <- c(1,2,3,4)
  line.type.l <- NULL
  dates.l <- structure(integer(), class = "Date")
  line.colours.l <- NULL
  
  for (sd in 1:length(dates)){
    ind.d <- which(select.plotdat[,'Date']==dates[sd])
    ind.s <- which(select.plotdat[,'Station'] == 'Leslie')
    ind <- intersect(ind.d,ind.s)
    lines(select.plotdat[ind,'SpCond'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
    
    line.type.l <- c(line.type.l,line.types[1])
    dates.l <- c(dates.l,as.Date(dates[sd], "%m/%d/%Y"))
    line.colours.l <- c(line.colours.l,PlotColours[sd])
    
    
  }
  
  
  
  ##PLOT legend
  plot.new()
  par(mar=c(2,0,4,1))
  
  plot.window(c(0,1), c(0,1));
  box()
  text(0.03,0.995,'Leslie Lake - Field Specific Conductivity', cex=1,adj=0,font = 2)
  legcex <- .7
  
  ## Legend for Conductivity
  
  legend.txt2 <- as.character(format(dates.l,"%b %d, %Y"))
  
  text(0.08,0.95,"Profile Date", cex=0.9,adj=0)
  #1-25
  legend(x=.08,y=.9,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
         cex=legcex,bty='n')
  #26+
  #ll <- length(legend.txt2)
  #legend(x=.5,y=.9,legend.txt2[26:ll],lty=1,col=PlotColours.p[26:ll],lwd=2,
  #      cex=legcex,bty='n')
  
  ##plot #1-Cell D
  par(mar=c(2,5,4,0))
  ## Values for plot
  x.label <- 'Calculated Potassium (mg/L)'
  xmin <- 0
  xmax <- 80
  
  
  ##make the plot
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(15,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  if(xmax <= 1000){
    axis(3,cex=.5,cex=.5,cex.axis=0.85)
  }else{
    axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)}
  
  axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2)
  mtext('Depth (m)',side=2,line=3)
  
  
  #Calculate Potassium
  select.plotdat[,'Potassium.field']<- datain.rel[3,'Intercept'] +  (datain.rel[3,'m']*select.plotdat[,'SpCond'])
  
  avg.pred.p <- NULL
  for (sd in 1:length(dates)){
    ind.d <- which(select.plotdat[,'Date']==dates[sd])
    ind.s <- which(select.plotdat[,'Station'] == 'Leslie')
    ind <- intersect(ind.d,ind.s)
    
    lines(select.plotdat[ind,'Potassium.field'],select.plotdat[ind,'Depth.m'],col=PlotColours[sd],lwd=1,lty=line.types[1])
    average <- mean(select.plotdat[ind,'Potassium.field'])
    avg.pred.p <- c(avg.pred.p,average)
    
    
    
    
  }
  
  ##Add Benchmark
  lines(rep(64,length(c(0:20))),c(0:20),lty=3,col='red')
  text(64.5,15,'Benchmark = 64 mg/L',cex=0.8,adj=01)
  
  ##Add Observed points
  ##Field Specific Conductivity
  field[,'Date'] <- as.Date(field[,'Date'], "%m/%d/%Y")
  
  ind.y <-  which(as.numeric(field[,'Date'],"%m/%d/%Y")>= 17329)
  ind.D <- which(field[,'LakeID']=='Cell E')
  ind.YD <- intersect(ind.y,ind.D)
  
  ##Observed Data- Potassium
  ##TO ADD WHEN PUMPING OCCURS
  #obs.K <- field[ind.YD,c('Date','Potassium')]
  
  #obs.plotdat.K <- subset(obs.K , format(as.Date(Date),"%Y")==2018)
  
  #dates.p <- unique(obs.plotdat.K[,'Date'])
  #PlotColours.p<-timPalette(n=length(dates.p))
  
  #for (sd in 1:length(dates.p)){
  #  ind <- which(obs.plotdat.K[,'Date']==dates.p[sd])
  #  points(obs.plotdat.K[ind,'Potassium'],rep(0.1,length(ind)),col=PlotColours.p[ind], pch=22)
  #}
  
  
  ##PLOT legend1
  plot.new()
  par(mar=c(2,0,4,1))
  
  plot.window(c(0,1), c(0,1));
  box()
  text(0.1,0.999,'Leslie Lake', cex=1,adj=0,font = 2)
  legcex <- .7
  eq3 <- paste0("Potassium = ", datain.rel[3,'Intercept'],
                ifelse(sign(datain.rel[3,'m'])==1, " + ", " - "), abs(datain.rel[3,'m']), " [SpCond-Field] ")
  
  #legend.txt <- c('Lab Conductivity Relationship','Field Conductivity Relationship','Field Specific Conductivity Relationship')
  #legend(x=.03,y=.975,legend.txt[3],lty=c(1),col='black',lwd=2,
  #        cex=legcex,bty='n')
  
  text(0.03,0.95,eq3, cex=legcex,adj=0)
  ## Legend for Profile dates
  
  
  avg.pred.p <- round(avg.pred.p,1)
  
  legend.potassium <- paste(c(avg.pred.p),"mg/L",sep =" ")
  legend.dates <- as.character(format(dates.l,"%b %d, %Y"))
  
  legend.txt2 <- paste(legend.dates,legend.potassium,sep= " ; Avg = ")
  
  
  
  text(0.08,0.91,"Profile Date", cex=0.9,adj=0)
  
  legend(x=.08,y=.85,legend.txt2,col=line.colours.l,lwd=1.2,lty=line.type.l,
         cex=legcex,bty='n')
}

#################################
###############################

Beartooth.wq <- function(datain.b){

layout(matrix(c(1,2,3,3),2,2,byrow=T),c(6,3),c(6,2))


##### Extract data #####

param <- "Potassium.T"
y.label <- "Depth (m)"
stations <- c('BT2014-2','BT2014-3','BT2015','BT2016','BT2016 - Post Fire','BT2016G - Post Fire',
              'BT2017 - Post Fire','BT2017G - Post Fire','BT2018 - Post Fire','BT2018 Discharge Samples')

plotdat <- getData3(datain.b,param,stations)
#make depth numeric
plotdat[,'Depth'] <-as.numeric(levels(plotdat[,'Depth']))[plotdat[,'Depth']]

##plot #2--full plot
par(mar=c(2,6,4,0.5)) 

## Values for plot
x.label <- "Total Potassium"
xmin <- 0  
xmax <- 350

#'BT2014-2','BT2014-3','BT2015','BT2016','BT2016 - Post Fire','BT2016G - Post Fire',
#'BT2017 - Post Fire','BT2017G - Post Fire','BT2018 - Post Fire','BT2018G - Post Fire'
plot.colors <- c("red","blue","orange","purple","green","cyan","pink")
pt.types <- c(21:25)

#select for one station- 2014
ind1 <- which(plotdat[,'Sample.ID'] == stations[1])
select.plotdat <- plotdat[ind1,]

##make the plot
profile.plot (select.plotdat,xmin,xmax,x.label,line.color=plot.colors[1],pt.size=0.85)

#par(new=T)  
#select for one station- 2014
ind2 <- which(plotdat[,'Sample.ID'] == stations[2])
select.plotdat <- plotdat[ind2,]

dates <- unique(select.plotdat[,'Date'])   
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  lines(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],col=plot.colors[2],lwd=1)
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types[sd],col=plot.colors[2],
         cex=0.85)
}

#select for 2015
ind3 <- which(plotdat[,'Sample.ID'] == stations[3])
select.plotdat <- plotdat[ind3,]

##add to plot


dates <- unique(select.plotdat[,'Date'])   

for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  lines(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],col=plot.colors[3],lwd=1)
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types[sd],col=plot.colors[3],
         cex=0.85)
}

#select for 2016
ind4 <- which(plotdat[,'Sample.ID'] == stations[4])
select.plotdat <- plotdat[ind4,]

##add to plot
dates <- unique(select.plotdat[,'Date'])   
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  lines(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],col=plot.colors[4],lwd=1)
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types[sd],col=plot.colors[4],
         cex=0.85)
}

#select for 2016 Post Fire profiles
ind5 <- which(plotdat[,'Sample.ID'] == stations[5])
select.plotdat <- plotdat[ind5,]

##add to plot
dates <- unique(select.plotdat[,'Date'])   
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  lines(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],col=plot.colors[5],lwd=1)
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types[sd],col=plot.colors[5],
         cex=0.85)
}


##The Post-Fire Grab samples 2016

#select for next year
ind6 <- which(plotdat[,'Sample.ID'] == stations[6])
select.plotdat <- plotdat[ind6,]

dates <- unique(select.plotdat[,'Date']) 
pt.types <- c(15)
point.colors <- timPalette(n=length(dates))  
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types,col=point.colors[sd],
         cex=1)
}

#select for 2017 Post Fire profiles
ind7 <- which(plotdat[,'Sample.ID'] == stations[7])
select.plotdat <- plotdat[ind7,]

##add to plot
pt.types <- c(21:25)
dates <- unique(select.plotdat[,'Date'])   
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  lines(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],col=plot.colors[6],lwd=1)
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types[sd],col=plot.colors[6],
         cex=0.85)
}

##The Post-Fire Grab samples 2017

#select for next year
ind8 <- which(plotdat[,'Sample.ID'] == stations[8])
select.plotdat <- plotdat[ind8,]

dates <- unique(select.plotdat[,'Date']) 
pt.types <- c(17)
point.colors <- timPalette(n=length(dates))  
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types,col=point.colors[sd],
         cex=1)
}

#select for 2018 Post Fire profiles
ind9 <- which(plotdat[,'Sample.ID'] == stations[9])
select.plotdat <- plotdat[ind9,]

##add to plot
pt.types <- c(21:25)
dates <- unique(select.plotdat[,'Date'])   
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  lines(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],col=plot.colors[7],lwd=1)
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types[sd],col=plot.colors[7],
         cex=0.85)
}

##The Post-Fire Grab samples 2018

#select for next year
ind10 <- which(plotdat[,'Sample.ID'] == stations[10])
select.plotdat <- plotdat[ind10,]

dates <- unique(select.plotdat[,'Date']) 
pt.types <- c(8)
point.colors <- timPalette(n=length(dates))  
for (sd in 1:length(dates)){ 
  ind <- which(select.plotdat[,'Date']==dates[sd])
  points(select.plotdat[ind,'Impute'],select.plotdat[ind,'Depth'],pch= pt.types,col=point.colors[sd],
         cex=1)
}





#############
##PLOT legend- for profiles
##############

plot.new()
par(mar=c(2,0,3,1))

plot.window(c(0,1), c(0,1));
box()
pt.types <- c(21:25)
legcex <- .7

## Legend for Station BT2014-2
dates <- unique(plotdat[ind1,'Date'])
n.d <- length(dates)
if(length(ind1) > 0){
  legend.txt1 <- as.character(format(dates,"%B %d, %Y"))
  
}else{
  legend.txt1 <- "Not Analyzed"
}

text(0.3,0.99,"BT2014-2", cex=0.8) 
legend(x=.025,y=.985,legend.txt1,lty=1,col=plot.colors[1],pch= pt.types[1:n.d],
       cex=legcex,bty='n')

## Legend for Station BT2014-3
dates <- unique(plotdat[ind2,'Date'])
n.d <- length(dates)
legend.txt2 <- as.character(format(dates,"%B %d, %Y"))


text(0.3,0.90,"BT2014-3", cex=0.8) 
legend(x=.025,y=.895,legend.txt2,lty=1,col=plot.colors[2],pch= pt.types[1:n.d],
       cex=legcex,bty='n')

## Legend for Station BT2015
dates <- unique(plotdat[ind3,'Date'])
n.d <- length(dates)
legend.txt2 <- as.character(format(dates,"%B %d, %Y"))


text(0.3,0.80,"BT2015", cex=0.8) 
legend(x=.025,y=.795,legend.txt2,lty=1,col=plot.colors[3],pch= pt.types[1:n.d],
       cex=legcex,bty='n')

## Legend for Station BT2016
dates <- unique(plotdat[ind4,'Date'])
n.d <- length(dates)
legend.txt2 <- as.character(format(dates,"%B %d, %Y"))
legend.txt3 <- c('Detection Limit')

text(0.3,0.70,"BT2016", cex=0.8) 
legend(x=.025,y=.695,legend.txt2,lty=1,col=plot.colors[4],pch= pt.types[1:n.d],
       cex=legcex,bty='n') 
## Legend for Station BT2016- Post fire
dates <- unique(plotdat[ind5,'Date'])
n.d <- length(dates)
legend.txt2 <- as.character(format(dates,"%B %d, %Y"))
legend.txt3 <- c('Detection Limit')

text(0.3,0.55,"BT2016 - Post Fire", cex=0.8) 
legend(x=.025,y=.545,legend.txt2,lty=1,col=plot.colors[5],pch= pt.types[1:n.d],
       cex=legcex,bty='n') 

## Legend for Station BT2017-post fire
dates <- unique(plotdat[ind7,'Date'])
n.d <- length(dates)
legend.txt2 <- as.character(format(dates,"%B %d, %Y"))
legend.txt3 <- c('Detection Limit')

text(0.3,0.40,"BT2017", cex=0.8) 
legend(x=.025,y=.395,legend.txt2,lty=1,col=plot.colors[6],pch= pt.types[1:n.d],
       cex=legcex,bty='n')           
## Legend for Station BT2018 - post fire
dates <- unique(plotdat[ind9,'Date'])
n.d <- length(dates)
legend.txt2 <- as.character(format(dates,"%B %d, %Y"))


text(0.3,0.20,"BT2018", cex=0.8) 
legend(x=.025,y=.195,legend.txt2,lty=1,col=plot.colors[7],pch= pt.types[1:n.d],
       cex=legcex,bty='n')

#############
##PLOT legend- for grab samples
##############

plot.new()
par(mar=c(1,2,1,2))

plot.window(c(0,1), c(0,1));
box()

## Legend for Station BT2016 Grab-PPFire
dates <- unique(plotdat[ind6,'Date'])
n.d <- length(dates)
point.colors <- timPalette(n=n.d)
legend.txt2 <- as.character(format(dates,"%b %d, %Y"))
legend.txt3 <- c('Detection Limit')

text(0.025,0.98,"BT2016 Grab Post-Fire", cex=0.8,adj=0) 
legend(x=.025,y=0.95,legend.txt2,col=point.colors,pch= 15,
       cex=legcex,bty='n')

## Legend for Station BT2017 Grab-PPFire
dates <- unique(plotdat[ind8,'Date'])
n.d <- length(dates)
point.colors <- timPalette(n=n.d)
legend.txt2 <- as.character(format(dates,"%b %d, %Y"))


text(0.3,0.98,"BT2017 Grab", cex=0.8,adj=0) 
legend(x=.3,y=0.95,legend.txt2,col=point.colors,pch= 17,
       cex=legcex,bty='n')

## Legend for Station BT2018 Grab-PPFire
dates <- unique(plotdat[ind10,'Date'])
n.d <- length(dates)
point.colors <- timPalette(n=n.d)
legend.txt2 <- as.character(format(dates,"%b %d, %Y"))


text(0.5,0.98,"BT2018 Discharge Samples", cex=0.8,adj=0) 
legend(x=.5,y=0.95,legend.txt2,col=point.colors,pch= 8,
       cex=legcex,bty='n')
}

#####################
####################

profile.plot <- function(plotdat,xmin,xmax,x.label,line.color,pt.size)
{
  pt.types <- c(21:25)
  
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(60,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  if(xmax <= 1000)
  {axis(3,cex=.5,cex=.5,cex.axis=0.85)}
  else
  {axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85) }
  axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2)
  mtext('Depth (m)',side=2,line=3)
  # Add lines connecting points #
  dates <- unique(plotdat[,'Date'])   
  for (sd in 1:length(dates)){ 
    ind <- which(plotdat[,'Date']==dates[sd])
    lines(plotdat[ind,'Impute'],plotdat[ind,'Depth'],col=line.color,lwd=1)
    points(plotdat[ind,'Impute'],plotdat[ind,'Depth'],pch= pt.types[sd],col=line.color,
           cex=pt.size)
  }
}

##for the blow-ups
profile.plot2 <- function(plotdat,xmin,xmax,x.label,line.color,pt.size)
{
  pt.types <- c(21:25)
  
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(60,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  if(xmax <= 1000)
  {axis(3,cex=.5,cex=.5,cex.axis=0.85)}
  else
  {axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85) }
  #axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2,adj=0,cex=1.1,font=2)
  #mtext('Depth (m)',side=2,line=3)
  # Add lines connecting points #
  dates <- unique(plotdat[,'Date'])   
  for (sd in 1:length(dates)){ 
    ind <- which(plotdat[,'Date']==dates[sd])
    lines(plotdat[ind,'Impute'],plotdat[ind,'Depth'],col=line.color,lwd=1)
    points(plotdat[ind,'Impute'],plotdat[ind,'Depth'],pch= pt.types[sd],col=line.color,
           cex=pt.size)
  }
}
##for % of Total
profile.plot3 <- function(plotdat,xmin,xmax,x.label,line.color,pt.size)
{
  pt.types <- c(21:25)
  
  # Set up the Axes for the plot with the appropriate limits, but do not actually plot the data #
  plot(0,0,col=NA,xlim=c(xmin,xmax),ylim=c(60,0),xaxt="n",yaxt="n",xlab="",ylab="",cex.axis=0.8)
  
  
  #axis(3,at=pretty(c(xmin, xmax)), labels= formatC(pretty(c(xmin, xmax)), format="d", big.mark=','),cex=.5,cex.axis=0.85)
  
  axis(3, at=pretty(c(xmin, xmax)), labels=sprintf(pretty(c(xmin, xmax)), fmt="%1.0f%%"),cex=.5,cex.axis=0.85)
  axis(side=2,las=2,cex=.5,cex.axis=0.85)
  mtext(x.label,side=3,line=2)
  mtext('Depth (m)',side=2,line=3)
  # Add lines connecting points #
  dates <- unique(plotdat[,'Date'])   
  for (sd in 1:length(dates)){ 
    ind <- which(plotdat[,'Date']==dates[sd])
    lines(plotdat[ind,'%Dissolved'],plotdat[ind,'Depth'],col=line.color,lwd=1)
    points(plotdat[ind,'%Dissolved'],plotdat[ind,'Depth'],pch= pt.types[sd],col=line.color,
           cex=pt.size)
  }
}

########################
#######################
######################
##PPD
###################
PPD.plot <- function(plotdat,label,year1,year2,year3,year4,syear=2000,year=2019,ymin=0,ymax,addpercentiles=TRUE){
  
  y.label <- label
  #ticks
  Year.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year+1,"12-31",sep="-")), by = "year")
  label.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year+1,"12-31",sep="-")), by = "3 year")
  label.name <- as.character(format(label.tck,"%Y"))
  ###
  #get rid of blank rows for calculations
  plotdat <- na.omit(plotdat)
  
  temprow1 <-  matrix(c(rep.int(NA,3)),nrow=3,ncol=3)
  newrow1 <- data.frame(temprow1)
  temprow2 <-  matrix(c(rep.int(NA,2)),nrow=2,ncol=3)
  newrow2 <- data.frame(temprow2)
  colnames(newrow1) <- colnames(plotdat)
  colnames(newrow2) <- colnames(plotdat)
  cal.avg <-rbind(newrow1,plotdat,newrow2)
  roll.avg <- rollapply(cal.avg[,'Impute'], width = 6,FUN = mean, na.rm = TRUE,align='left')
  l.plotdat <- cbind(plotdat,roll.avg)
  
  ###Calculate percentile Group 1
  
  ind <- which(as.numeric(format(plotdat[,'Date'],"%Y")) >= year1 & as.numeric(format(plotdat[,'Date'],"%Y")) < year2)
  g1.median <- median(plotdat[ind,'Impute'])
  g1.quantl <- quantile(plotdat[ind,'Impute'],.9)
  g1.quantu <- quantile(plotdat[ind,'Impute'],.1)
  
  ###Calculate percentile Group 2
  #if(is.null(year3)){
  
  ind <- which(as.numeric(format(plotdat[,'Date'],"%Y")) >= year2 & as.numeric(format(plotdat[,'Date'],"%Y")) < year3)
  g2.median <- median(plotdat[ind,'Impute'])
  g2.quantl <- quantile(plotdat[ind,'Impute'],.9)
  g2.quantu <- quantile(plotdat[ind,'Impute'],.1)
  
  ###Calculate percentile Group 3
  
  ind <- which(as.numeric(format(plotdat[,'Date'],"%Y")) >= year3 & as.numeric(format(plotdat[,'Date'],"%Y")) <= year4)
  g3.median <- median(plotdat[ind,'Impute'])
  g3.quantl <- quantile(plotdat[ind,'Impute'],.9)
  g3.quantu <- quantile(plotdat[ind,'Impute'],.1)
  
  
  ## the plot
  
  plot(0,0, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin,ymax),
       xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2)
  
  censored <- plotdat[,'Censored']
  
  ##Make closed symbols Actual and open symbols imputed from less than detection
  l.col <- 'blue'
  for(ss in 1:length(censored)){
    if(censored[ss]=='TRUE' ){
      points(plotdat[ss,'Date'],plotdat[ss,'Impute'],col=l.col,bg= l.col,pch=22,cex=1) 
    }
    else{
      points(plotdat[ss,'Date'],plotdat[ss,'Impute'],col=l.col,pch=22,cex=1) 
    }
  }
  
  lines(l.plotdat[,c('Date','roll.avg')])
  
  if(addpercentiles==TRUE){
    
    #Group1 lines
    #rect(as.Date("2000-01-01"),0,as.Date("2006-06-30"),ymax,col= '#FF000022')
    segments(as.Date(paste(year1,"01-01",sep="-")), g1.median,as.Date(paste(year2,"06-30",sep="-")), g1.median, col='red')
    segments(as.Date(paste(year1,"01-01",sep="-")), g1.quantl,as.Date(paste(year2,"06-30",sep="-")), g1.quantl, col='red',lty=2)
    segments(as.Date(paste(year1,"01-01",sep="-")), g1.quantu,as.Date(paste(year2,"06-30",sep="-")), g1.quantu, col='red',lty=3)
    
    #Group2 lines
    #rect(as.Date("2006-06-30"),0,as.Date("2016-01-31"),ymax,col= '#FF800022')
    if(!is.na(year3)) {
      segments(as.Date(paste(year2,"06-30",sep="-")), g2.median,as.Date(paste(year3,"01-31",sep="-")), g2.median, col='green')
      segments(as.Date(paste(year2,"06-30",sep="-")), g2.quantl,as.Date(paste(year3,"01-31",sep="-")), g2.quantl, col='green',lty=2)
      segments(as.Date(paste(year2,"06-30",sep="-")), g2.quantu,as.Date(paste(year3,"01-31",sep="-")), g2.quantu, col='green',lty=3)
    }
    #Group3 lines
    #rect(as.Date("2016-01-31"),0,as.Date(paste(year+1,"01-01",sep="-")),ymax,col= '#FFFF0022')
    if(!is.na(year4)) {
      segments(as.Date(paste(year3,"01-31",sep="-")), g3.median,as.Date(paste(year4+1,"01-01",sep="-")), g3.median, col='purple')
      segments(as.Date(paste(year3,"01-31",sep="-")), g3.quantl,as.Date(paste(year4+1,"01-01",sep="-")), g3.quantl, col='purple',lty=2)
      segments(as.Date(paste(year3,"01-31",sep="-")), g3.quantu,as.Date(paste(year4+1,"01-01",sep="-")), g3.quantu, col='purple',lty=3)
    }
  }
  ## Add axes
  ##format large numbers
  if(ymax > 10)
  {axis(2,pos= as.Date(paste(syear,"01-01",sep="-")),at=pretty(ymin:ymax), labels= formatC(pretty(ymin:ymax), format="g", big.mark=',',digits=nchar(as.character(ymax))),cex=.5,cex.axis=.85, las=2)
    segments(as.Date(paste(year+1,"01-01",sep="-")),ymin,as.Date(paste(year+1,"01-01",sep="-")),ymax)}
  if(ymax <= 10)
  {axis(2,pos= as.Date(paste(syear,"01-01",sep="-")), cex=.5,cex.axis=.85, las=2)
    segments(as.Date(paste(year+1,"01-01",sep="-")),ymin,as.Date(paste(year+1,"01-01",sep="-")),ymax)}
  
  
  axis(1,pos= ymin,at=Year.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1)
  axis(1,pos= ymin,at=label.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1,tck=-0.02)
  axis(1,pos= ymin,at=label.tck,labels=label.name,tick=F, cex=.5,cex.axis=.85,lwd.ticks=1)
  
  
  ## Add top and bottom
  segments(as.Date(paste(syear,"01-01",sep="-")),ymax,as.Date(paste(year+1,"01-01",sep="-")),ymax)
  segments(as.Date(paste(syear,"01-01",sep="-")),ymin,as.Date(paste(year+1,"01-01",sep="-")),ymin)
  
  ###Add the periods for analysis
  #Period 1: up to end of 2005, dominated by Koala/Panda open pits
  
  rect(as.Date("2000-01-01"),0,as.Date("2006-06-30"),ymax,col= '#FF000022')
  
  #Period 2 ? 2006 to end of 2017, dominated by Fox and U/G
  rect(as.Date("2006-06-30"),0,as.Date("2016-01-31"),ymax,col= '#FF800022')
  #Period 3 ? 2017 onwards, no Fox and dominated by U/G and Misery
  rect(as.Date("2016-01-31"),0,as.Date(paste(year+1,"01-01",sep="-")),ymax,col= '#FFFF0022')
  
  mtext('Year', side=1, line= 1.5,adj= 0.5, font=2)
  mtext(y.label, side=2, line= 2,adj= 0.5, font=2)
}


######################
#####################

getData <- function(datain,param,syear){
  ## Extract data data for param in the specified stations and
  ## perfrom some initial data manipulation.
  ##keeps depth for the Pit Lake datafile
  ## Load data
  dat <- read.table(datain,sep=',',header=T,na.strings=c('NA','na','-','---','N/S'),strip.white=TRUE ,fill=T)
  
  ## Extract all data on variable
  paramDat <- dat[which(dat[,param]!=''),
                  c('LakeID','ID','Lake','Date','Month',param,'Hardness')]
  
  ##Make sure data is in correct month/day/year format
  paramDat[,'Date'] <- as.Date(paramDat[,'Date'], "%m/%d/%Y")
  
  ##Make sure data starts at same year for figures
  ind <- which(as.numeric(format(paramDat[,'Date'],"%Y"))>= syear)
  paramDat <- paramDat[ind,]
  
  ## Count total number of records
  n <- dim(paramDat)[1]
  
  
  ##### Process response -- particularly deal with values below detection #####
  ## Create three vectors:
  ## 1) 0 (lower bound) if <DL, observed value otw
  ## 2) DL (upper bound) if <DL, observed value otw
  ## 3) 1/2 DL (imputed value) if <DL, observed value otw
  ## 4) Simple indicator: 1 if <DL, 0 otw
  ##
  ## Note: converting factors to numeric is messy, hence the mess!
  
  ## Initialize the output vectors
  lower <- numeric(n)
  upper <- numeric(n)
  impute <- numeric(n)
  censored <- logical(n)
  
  for(i in 1:n){
    ## If the recorded value contains a '<'
    if(regexpr('<',paramDat[i,param]) > 0){
      ## Remove the '<'
      val <- as.numeric(strsplit(as.character(paramDat[i,param]),'<')[[1]][2])
      
      ## Define lower and upper bounds and imputed value
      lower[i] <- 0
      upper[i] <- val
      impute[i] <- val/2
      
      
      ## Define censoring indicator
      censored[i] <- T
    }
    else{
      val <- as.numeric(as.character(paramDat[i,param]))
      ## Define lower and upper bounds and imputed value
      lower[i] <- val
      upper[i] <- val
      impute[i] <- val
      
      ## Define censoring indicator
      censored[i] <- F
    }
  }
  ##Multiply by scale to get appropriate ppb or ppm
  #lower <- lower*p.scale
  #upper <- upper*p.scale
  #impute <- impute*p.scale
  
  ## Add to data
  paramDat[,'Lower'] <- lower
  paramDat[,'Upper'] <- upper
  paramDat[,'Impute'] <- impute
  paramDat[,'Censored'] <- censored  
  ## Return the new data frame
  return(paramDat)
}


####For Beartooth Data

getData3 <- function(datain,param,stations){
  ## Extract data data for param in the specified stations and
  ## perfrom some initial data manipulation.
  ##keeps depth for the Pit Lake datafile
  ## Load data
  dat <- read.table(datain,sep=',',header=T,na.strings=c('NA','na','-','---','N/S'),strip.white=TRUE ,fill=T)
  
  ## Extract all data on variable
  paramDat <- dat[which(dat[,param]!=''),
                  c('Sample.ID','Date','Depth',param,'Hardness')]
  
  
  ## Identify rows which match the specified stations
  
  rows1 <- NULL                                                     
  for(st in stations)
    rows1 <- c(rows1,which(paramDat['Sample.ID'] == st))
  ## Extract data for specified station(s)
  paramDat <- paramDat[rows1,]
  
  ## Count total number of records
  n <- dim(paramDat)[1]
  ##### Perform some initial data processing #####
  #Recode Depth as a factor
  paramDat[,'Depth'] <- as.factor(paramDat[,'Depth'])
  
  ##Make sure data is in correct month/day/year format
  paramDat[,'Date'] <- as.Date(paramDat[,'Date'], "%m/%d/%Y")
  
  ##Make hardness = detection limit if less than
  hardness <- numeric(n)
  for(i in 1:n){
    ## If the recorded value contains a '<'
    if(regexpr('<',paramDat[i,'Hardness']) > 0){
      ## Remove the '<'
      val1 <- as.numeric(strsplit(as.character(paramDat[i,'Hardness']),'<')[[1]][2])
      hardness[i] <- val1
    }
    else{
      val1 <- as.numeric(as.character(paramDat[i,'Hardness']))
      hardness [i] <- val1
    }
  }
  ## Add to data
  paramDat[,'Hardness.a'] <- hardness   
  
  ##### Process response -- particularly deal with values below detection #####
  ## Create three vectors:
  ## 1) 0 (lower bound) if <DL, observed value otw
  ## 2) DL (upper bound) if <DL, observed value otw
  ## 3) 1/2 DL (imputed value) if <DL, observed value otw
  ## 4) Simple indicator: 1 if <DL, 0 otw
  ##
  ## Note: converting factors to numeric is messy, hence the mess!
  
  ## Initialize the output vectors
  lower <- numeric(n)
  upper <- numeric(n)
  impute <- numeric(n)
  censored <- logical(n)
  
  for(i in 1:n){
    ## If the recorded value contains a '<'
    if(regexpr('<',paramDat[i,param]) > 0){
      ## Remove the '<'
      val <- as.numeric(strsplit(as.character(paramDat[i,param]),'<')[[1]][2])
      
      ## Define lower and upper bounds and imputed value
      lower[i] <- 0
      upper[i] <- val
      impute[i] <- val/2
      
      
      ## Define censoring indicator
      censored[i] <- T
    }
    else{
      val <- as.numeric(as.character(paramDat[i,param]))
      ## Define lower and upper bounds and imputed value
      lower[i] <- val
      upper[i] <- val
      impute[i] <- val
      
      ## Define censoring indicator
      censored[i] <- F
    }
  }
  ##Multiply by scale to get appropriate ppb or ppm
  #lower <- lower*p.scale
  #upper <- upper*p.scale
  #impute <- impute*p.scale
  
  ## Add to data
  paramDat[,'Lower'] <- lower
  paramDat[,'Upper'] <- upper
  paramDat[,'Impute'] <- impute
  paramDat[,'Censored'] <- censored  
  ## Return the new data frame
  return(paramDat)
}

######
##No hardness or LakeID for the PPD template
getData.PPD <- function(datain,param,syear){
  ## Extract data data for param in the specified stations and
  ## perfrom some initial data manipulation.
  ##keeps depth for the Pit Lake datafile
  ## Load data
  dat <- read.table(datain,sep=',',header=T,na.strings=c('NA','na','-','---','N/S'),strip.white=TRUE ,fill=T)
  
  ## Extract all data on variable and TSS
  paramDat <- dat[which(dat[,param]!=''),
                  c('Date',param,'TSS')]
  
  ##Make sure data is in correct month/day/year format
  paramDat[,'Date'] <- as.Date(paramDat[,'Date'], "%m/%d/%Y")
  
  ## Count total number of records
  n <- dim(paramDat)[1]
  
  ##Make TSS = detection limit if less than
  TSS <- numeric(n)
  for(i in 1:n){
    ## If the recorded value contains a '<'
    if(regexpr('<',paramDat[i,'TSS']) > 0){
      ## Remove the '<'
      val1 <- as.numeric(strsplit(as.character(paramDat[i,'TSS']),'<')[[1]][2])
      TSS[i] <- val1
    }
    else{
      val1 <- as.numeric(as.character(paramDat[i,'TSS']))
      TSS [i] <- val1
    }
  }
  ## Add to data
  paramDat[,'TSS.a'] <- TSS
  
  ##Make sure data starts at same year for figures
  ind <- which(as.numeric(format(paramDat[,'Date'],"%Y"))>= syear)
  paramDat <- paramDat[ind,]
  
  ## Count total number of records
  n <- dim(paramDat)[1]
  
  
  ##### Process response -- particularly deal with values below detection #####
  ## Create three vectors:
  ## 1) 0 (lower bound) if <DL, observed value otw
  ## 2) DL (upper bound) if <DL, observed value otw
  ## 3) 1/2 DL (imputed value) if <DL, observed value otw
  ## 4) Simple indicator: 1 if <DL, 0 otw
  ##
  ## Note: converting factors to numeric is messy, hence the mess!
  
  ## Initialize the output vectors
  lower <- numeric(n)
  upper <- numeric(n)
  impute <- numeric(n)
  censored <- logical(n)
  
  for(i in 1:n){
    ## If the recorded value contains a '<'
    if(regexpr('<',paramDat[i,param]) > 0){
      ## Remove the '<'
      val <- as.numeric(strsplit(as.character(paramDat[i,param]),'<')[[1]][2])
      
      ## Define lower and upper bounds and imputed value
      lower[i] <- 0
      upper[i] <- val
      impute[i] <- val/2
      
      
      ## Define censoring indicator
      censored[i] <- T
    }
    else{
      val <- as.numeric(as.character(paramDat[i,param]))
      ## Define lower and upper bounds and imputed value
      lower[i] <- val
      upper[i] <- val
      impute[i] <- val
      
      ## Define censoring indicator
      censored[i] <- F
    }
  }
  ##Multiply by scale to get appropriate ppb or ppm
  #lower <- lower*p.scale
  #upper <- upper*p.scale
  #impute <- impute*p.scale
  
  ## Add to data
  paramDat[,'Lower'] <- lower
  paramDat[,'Upper'] <- upper
  paramDat[,'Impute'] <- impute
  paramDat[,'Censored'] <- censored
  ## Return the new data frame
  return(paramDat)
}

#' Export a Formattable as PNG, PDF, or JPEG
#'
#' @param f A formattable.
#' @param file Export path with extension .png, .pdf, or .jpeg.
#' @param width Width specification of the html widget being exported.
#' @param height Height specification of the html widget being exported.
#' @param background Background color specification.
#' @param delay Time to wait before taking webshot, in seconds.
#'
#' @importFrom formattable as.htmlwidget
#' @importFrom htmltools html_print
#' @importFrom webshot webshot
#'
#' @export
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

Observed.LLCF.graph.byStation <- function(param,p,syear,year,dataAll,Cell ='Cell D'){ 
  
  ## Divide plotting area into 1 plots: with plot and Legend
  layout(matrix(c(1),1,1,byrow=T),c(5),c(5))
  
  ###Plot 1- LLCF
  ##############
  par(mar=c(1.5,4.5,2.5,4.5))
  
  
  
  #ticks
  Year.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "year")
  label.tck <- seq(as.Date(paste(syear,"01-01",sep="-")),as.Date(paste(year,"12-31",sep="-")), by = "year")
  label.name <- as.character(format(label.tck,"%Y"))
  
  #limits and ylabel
  label1  <- as.character(param.list[p,'label'])
  
  ymin1 <- 0
  ymax1 <- param.list[p,'max']
  
  #The LLCF data
  ##Observed Data
  param <- as.character(param.list[p,'Variable.O'])
  
  if(Cell=='Cell D'){
    ## labels for plot
  Title <- paste('Cell D',as.character(param.list[p,'title']), sep=" ")  
  plot(0,0, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin1,ymax1),
       xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2,col='blue', pch=22)
  
  ablineclip(h=pretty(ymin1:ymax1), x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  ablineclip(h=pretty(ymin1:ymax1)/2, x1 = as.Date(paste(syear,"01-01",sep="-")), x2 = as.Date(paste(year+1,"01-01",sep="-")), lty = 3, col="gray",lwd=1.5)
  
  ##Make filled points for depth profiles used in calibration (e.g. LLD01 etc)
  ind4 <- which(dataAll[,'LakeID']=='Cell D')
  ind.D <- which(dataAll[,'ID']=='LLD01'| dataAll[,'ID']=='LLD02'| dataAll[,'ID']=='LLD03'| 
                    dataAll[,'ID']=='LLD04'| dataAll[,'ID']=='LLD05'| dataAll[,'ID']=='LLD07')
  ind.open <- intersect(ind4,ind.D)
  obs.plotdat <- dataAll[ind.open,]
  x.date <- obs.plotdat[,'Date']
  y.var <- obs.plotdat[,'Impute']
  points(x.date,y.var,col='blue',pch=22)
  
  ##Find the bottom values
  library(data.table)
  ind.e <- which(obs.plotdat$Lake %like% "B"|obs.plotdat$Lake %like% "deep")
  bottom.plotdat <- droplevels(obs.plotdat[ind.e,])
  x.date <- bottom.plotdat[,'Date']
  y.var <- bottom.plotdat[,'Impute']
  points(x.date,y.var,col='blue',bg='blue',pch=22,cex=1)
  
  ##addline for average 
  for.line <- na.omit(StationMeans(obs.plotdat))
  #lines(for.line$Date,for.line$Impute, col='blue')
  }
  
  if(Cell=='Cell E'){
  #Cell E
  Title <- paste('Cell E',as.character(param.list[p,'title']), sep=" ")   
  plot(0,0, type='p', xlim=c(as.Date(paste(syear,"01-01",sep="-")), as.Date(paste(year+1,"01-01",sep="-"))),ylim=c(ymin1,ymax1),
         xlab='',ylab='',axes=F,cex.lab=1.4,font.lab=2,col='blue', pch=22)
    
  ind4 <- which(dataAll[,'LakeID']=='Cell E')
  ind.E <- which(dataAll[,'ID']=='LLE03'| dataAll[,'ID']=='LLE05'| dataAll[,'ID']=='LLE01')
  ind.open <- intersect(ind4,ind.E)
  
  obs.plotdat <- dataAll[ind.open,]
  x.date <- obs.plotdat[,'Date']
  y.var <- obs.plotdat[,'Impute']
  points(x.date,y.var,col='red',pch=23,cex=1)
  
  ##Find the bottom values
  library(data.table)
  ind.d <- which(obs.plotdat$Lake %like% "-B"|obs.plotdat$Lake %like% "deep")
  bottom.plotdat <- obs.plotdat[ind.d,]
  x.date <- bottom.plotdat[,'Date']
  y.var <- bottom.plotdat[,'Impute']
  points(x.date,y.var,col='red',bg='red',pch=23,cex=1)
  
  ##addline for average 
  for.line <- na.omit(StationMeans(obs.plotdat))
  #lines(for.line$Date,for.line$Impute, col='red')
  }
  
  axis(2,pos= as.Date(paste(syear,"01-01",sep="-")),at=pretty(ymin1:ymax1), labels= formatC(pretty(ymin1:ymax1), format="g", big.mark=',',digits=nchar(as.character(ymax1))),cex=.5,cex.axis=.85, las=2)
  
  axis(1,pos= ymin1,at=Year.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1)
  axis(1,pos= ymin1,at=label.tck,labels=F, cex=.5,cex.axis=.85,lwd.ticks=1,tck=-0.02)
  axis(1,pos= ymin1,at=label.tck,labels=label.name,tick=F, cex=.5,cex.axis=.6,lwd.ticks=1)
  
  ## Add top and bottom
  segments(as.Date(paste(syear,"01-01",sep="-")),ymax1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  segments(as.Date(paste(syear,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymin1)
  segments(as.Date(paste(year+1,"01-01",sep="-")),ymin1,as.Date(paste(year+1,"01-01",sep="-")),ymax1)
  
  mtext(Title, side=3, line= 1,adj= 0.5, font=2)
  text(as.Date("2014-01-01"),.2*ymax1,"Closed symbols indicate bottom\n or deep samples",adj=0,cex=0.8)
  
 
  
}

StationMeans <- function(dat){
  ## Compute upper and lower bound and imputed value for mean
  ## in each station for each sampling day.
  dates <- unique(format(dat[,'Date'],"%Y-%m-%d"))
  lakes <-  unique(dat[,'LakeID'])
  stations <- unique(dat[,'ID'])
  
  
  output <- NULL
  for (dd in dates){
    for(ll in lakes){
    for(ss in stations){
      
      ind1 <- which(dat[,'LakeID']==ll & dat[,'ID']==ss)
      ind2 <- which(format(dat[,'Date'],"%Y-%m-%d")==dd)
      newind <- intersect(ind1,ind2)
      
      if(length(newind)>0){
        upper <- as.numeric(mean(dat[newind,'Upper']))
        lower <- as.numeric(mean(dat[newind,'Lower']))
        impute <- (upper+lower)/2
        
     
      }
      else{
        upper <- NA
        lower <- NA
        impute <- NA
      
      }
      
      
      if(is.null(output)){
        output <- data.frame(ll,ss,dd,lower,upper,impute)
      }
      else
        output <- rbind(output,
                        data.frame(ll,ss,dd,lower,upper,impute))
    }
    }    
  }
  
  
  colnames(output) <- c('LakeID','ID','Date','Lower','Upper','Impute')
  output[,'Date']<- as.Date(output[,'Date'])
  return(output)
}

calculate.benchmarks <- function (param,hardness,lake)
{
  benchmark.line <- NULL
  #chloride
  if(param == 'Chloride')
  {
    for (x in 1:length(hardness[,'Date']))
    {
      if (hardness[x,2] >= 10 & hardness[x,2] <= 160){
        benchmark.line[x] <- 116.63 * log(hardness[x,2]) - 204.09 
        
      }else if (hardness[x,2] < 10){
        
        benchmark.line[x] <- 116.63 * log(10) - 204.09 
        
      }else if (hardness[x,2] > 160){
        
        benchmark.line[x] <- 116.63 * log(160) - 204.09 
      }
    }
  }
  ##sulphate
  
  
  if(param == 'Sulphate')
  {
    for (x in 1:length(hardness[,'Date']))
    {
      if (hardness[x,2] <= 160){
        benchmark.line[x] <- exp(0.9116 * log (hardness[x,2]) + 1.712) 
        
      }else if (hardness[x,2] > 160){
        benchmark.line[x] <- exp(0.9116 * log (160) + 1.712) 
      }
    }
  }  
  
  if(param == 'Nitrate')
  {
    for (x in 1:length(hardness[,'Date']))
    {
      if (hardness[x,2] <= 160){
        benchmark.line[x] <- exp(0.9518*log(hardness[x,2])-2.032) 
        
      }else if (hardness[x,2] > 160){
        benchmark.line[x] <- exp(0.9518*log(160)-2.032) 
      }
      
    }
  }
  
  if(param == 'Phosphate')
    #2016 Phosphate Response Plan
    #Nanuq	0.0025
    #Counts	0.0100
    #Vulture	0.0043
    #Kodiak	0.0180
    #Leslie	0.0096
    #Moose	0.0077
    #Cujo	0.0100
    #Fay Bay	0.0093
    #Nema	0.0091
    #Slipper	0.01
  #Lac de Gras (S2 and S3)	0.0054
  #Cujo	0.01
  #Lac du Sauvage (LdS1 and LdS2)	0.0069
  
  
  {
    for (x in 1:length(hardness[,'Date']))
    {
      if (lake == 'CellE'){
        benchmark.line[x] <- 0.0096 
        
      }else if (lake == 'Leslie'){
        benchmark.line[x] <- 0.0096 
        
      }else if (lake == 'Moose'){
        benchmark.line[x] <- 0.0077 
        
      }else if (lake == 'Nema'){
        benchmark.line[x] <- 0.0091 
        
      }else if (lake == 'Slipper'){
        benchmark.line[x] <- 0.01 
      }
    }
    
  }
  
  if(param == 'Cadmium')
  {
    for (x in 1:length(hardness[,'Date']))
    {
      #minimum of 0.00004 mg/L applicable to hardness 0-16 mg/L, maximum of 0.000037 mg/L applicable to hardness greater than 280 mg/L
      if (hardness[x,2] >= 17 & hardness[x,2] <= 280)
      {benchmark.line[x] <- (10^(0.83*log10(hardness[x,2])-2.46))/1000 
      
      }else if (hardness[x,2] < 17){
        benchmark.line[x] <- 0.00004 
      }else if (hardness[x,2] > 280){
        benchmark.line[x] <- 0.00037 }
      
    }
    
  }
  
  
  
  
  
  if(param == 'Copper')
  {
    for (x in 1:length(hardness[,'Date']))
    {
      
      #Minimum benchmark of 2?g/L  applicable to hardness 0-180 000 ?g/L, 
      #maximum of 40  ?g/L applicable to hardness greater than 180 000 ?g/L.
      #0.2 x e (0.8545[ln(hardness)]-1.465)
      if (hardness[x,2] > 82 & hardness[x,2] <= 180){
        benchmark.line[x] <- (0.2*(exp(0.8545*log(hardness[x,2])-1.465)))/1000 
        
      }else if (hardness[x,2] <= 180){
        benchmark.line[x] <- 0.002 
      }else if (hardness[x,2] > 180){
        benchmark.line[x] <- 0.004 
      }
      
    }
    
  }
  
  if(param == 'Lead')
  {
    
    for (x in 1:length(hardness[,'Date']))
    {
      
      #e (1.273[ln(hardness)]-4.705) with Minimum benchmark of 1 ?g/L applicable 
      #to hardness of 0-60 000 ?g/L and a maximum of 7 ?g/L applicable to hardness greater than 180  000 ?g/L
      if (hardness[x,2] > 60 & hardness[x,2] <= 180){
        benchmark.line[x] <- (exp(1.273*log(hardness[x,2])-4.705))/1000 
        
      }else if (hardness[x,2] <= 60){
        benchmark.line[x] <- 0.001 
      }else if (hardness[x,2] > 180){
        benchmark.line[x] <- 0.007 }
      
    }
    
  }
  
  if(param == 'Manganese')
  {
    
    for (x in 1:length(hardness[,'Date']))
    {
      #4.4 x hardness + 605 
      benchmark.line[x] <- (4.4*hardness[x,2] +605)/1000 
      
    }
    
  }
  
  
  if(param == 'Nickel')
  {
    
    for (x in 1:length(hardness[,'Date']))
    {
      #e 0.76[ln(hardness)]+1.06
      #Minimum of 25 ?g/L applicable to hardness 0-60  000?g/L, maximum of 150 ?g/L at hardness greater than 180  000?g/L
      if (hardness[x,2] > 60 & hardness[x,2] <= 180){
        benchmark.line[x] <- (exp(0.76*log(hardness[x,2])+1.06))/1000 
        
      }else if (hardness[x,2] <= 60){
        benchmark.line[x] <- 0.025 
      }else if (hardness[x,2] > 180){
        benchmark.line[x] <- 0.150 
      }
      
    }
    
  }
  
  hardness <- cbind(hardness,benchmark.line)
  #hardness[,'Date']<- as.Date(hardness[,'Date'], "%m/%d/%Y")  
  
} 


create_station_names <- function(cell.name, stations){
  
  ##cell.name = NA for lake stations, cell.name != NA for cell stations
  if(is.na(cell.name)){
    
    
    for(station in stations){
      
      assign(paste0('current.date.', station), 
             as.Date(as.integer(Cell.current(dataAll,station,"")[1])),
             envir = .GlobalEnv)
      
      assign(paste0('current.',param, station),  
             Cell.current(dataAll,station,"")[2],
             envir = .GlobalEnv)
      
      assign(paste0("n.",station), 
             Cell.current(dataAll,station,"")[3],
             envir = .GlobalEnv)
      
    }
    
    
           
    
    
    
  }else{
    
    for(station in stations){
      
      station.label <- gsub("-",".",station)
      #print(station.label)
      
      assign(paste0('current.date.', station.label), 
             as.Date(as.integer(Cell.current(dataAll,cell.name,station)[1])),
             envir = .GlobalEnv)
      #print(paste0('current.date.', station.label))
      
      assign(paste0('current.',param, station.label), 
             Cell.current(dataAll,cell.name,station)[2],
             envir = .GlobalEnv)
      #print(paste0('current.',param, station.label))
      
      assign(paste0("n.",station.label), 
             Cell.current(dataAll,cell.name ,station)[3],
             envir = .GlobalEnv)
      #print(paste0("n.",station.label))
      
      
    }
    
  
  }
  
  
}


plot_pumped_volume <- function(year.plotted,scenario.num,plot.months.from, para.eqc){
  
  
  par(mar=c(5,8,2,4))
  
  ##Scenario 1
  #Data generation
  
  #formatDate
  LLCFpump.strategies.2020[,'Date'] <- as.Date(LLCFpump.strategies.2020[,'Date'],"%m/%d/%Y")
  
  ##filter by strategy, month and year
  
  #Scenario
  #1
  ind.s <- which(LLCFpump.strategies.2020[,'Strategy']== scenario.num)
  ind.m <- which(LLCFpump.strategies.2020[,'Month1']>= plot.months.from & 
                   LLCFpump.strategies.2020[,'Year1']== year.plotted )
  
  ind.ss <- intersect(ind.s,ind.m)
  Strat <- LLCFpump.strategies.2020[ind.ss,]
  
  ##data for plots
  bars <- Strat[,c('CellDtoE','CellEtoLeslie')]
  potassium.D <- Strat[,c('Date','CellD.K')]
  potassium.E <- Strat[,c('Date','CellE.K')]
  
  
  ymin= 0
  ymax=3500000
  
  plot_title <- paste0("Scenario ", scenario.num, " - ", year.plotted)  
  
  
  
  barplot(as.matrix(t(bars)),beside=TRUE,yaxt = "n", xaxt="n",ylim=c(ymin,ymax),col=c("blue","orange"))
  
  axis(2,at=pretty(ymin:ymax), labels= formatC(pretty(ymin:ymax), format="g", big.mark=',',digits=nchar(as.character(ymax))),cex=.5,cex.axis=.85, las=2)
  
  axis(side = 1,at=c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65,68,71,74,77,80,83),labels = rep(c("1","2","3","4"),7), cex.axis=0.7,tcl=-0.25)
  axis(side = 1,at=seq(12,112,12), labels = FALSE,tcl=-1.5)
  
  mtext(plot_title,side=3,line=0.5,font=2,cex=1.1,adj=0)
  
  box()
  
  
  
  mtext(text=c("June", "July","Aug","Sept","Oct","Nov","Dec"),side = 1, line = c(2), at = seq(7,84,by=12),cex=0.7)
  mtext(text='Week',side=1,line=c(1),at=-2, cex=0.7)
  ylab.text = expression('Pumped Volume (m'^"3"*')')
  
  mtext(ylab.text,side=2,line=4,font=2)
  
  par(new = TRUE)
  
  ymax <- 70
  start_date <- make_date(year=year.plotted, month=1, day=1)
  end_date <- make_date(year=year.plotted, month=12, day=31)
  text_date <- make_date(year=year.plotted, month=8, day=1)
  
  plot(potassium.D, type = "n", xlab = "", ylab = "", yaxt = "n", xaxt="n",ylim = c(0, ymax))
  lines(potassium.D, col = "blue", lwd = 2)
  lines(potassium.E, col = "orange", lwd = 2)
  #add EQC
  #segments(as.Date("2020-01-01"),53,as.Date("2020-12-31"),53,col='orange',lty=2)
  segments(start_date,para.eqc,end_date,para.eqc,col='orange',lty=2)
  
  
  ##add Cumulative Volume
  
  CellE.Vol <- paste('Cell E To Leslie Total = ',formatC(sum(bars[2]), format="g", big.mark=',',digits=nchar(as.character(sum(bars[2]))))) 
  CellD.Vol <- paste('Cell D To Cell E Total = ',formatC(sum(bars[1]), format="g", big.mark=',',digits=nchar(as.character(sum(bars[1])))))
  
  CellE.Vol.T <-  bquote(.(CellE.Vol) ~ m^3)
  CellD.Vol.T <-  bquote(.(CellD.Vol) ~ m^3)
  
  text(text_date,27,CellE.Vol.T,adj= 0,cex=0.9)
  text(text_date,22,CellD.Vol.T,adj= 0,cex=0.9)
  
  axis(4,at=pretty(ymin:ymax),cex=.5,cex.axis=.85, las=2)
  mtext("Potassium (mg/L)", side = 4, line = 3, font = 2)
  
  
  
}


plot_legends <- function(para.eqc){
  
  
  par(mar=c(1,1,1,1))
  plot.new()
  plot.window(c(0,1), c(0,1));
  box()
  l.cex =1.1
  legend.names <- c('Cell D to E', 'Cell E to Leslie Lake', 'Potassium - Cell D', 'Potassium - Cell E')
  legend.col <- c('blue','orange','blue','orange')
  
  
  
  legend(x=0.4,y=.38,paste0('EQC = ', para.eqc ,' mg/L'),lty=2,col= 'orange', bty='n',cex = l.cex,lwd=1)
  
  legend(x=0.02,y=.85,legend.names[1:2],pch=c(15,15),pt.bg=legend.col[1:2],col= legend.col[1:2], pt.cex= 2.5,bty='n',cex = l.cex)
  
  legend(x=0.4,y=.85,legend.names[3:4],lty=c(1,1),col= legend.col[1:2],bty='n',lwd=1.1,cex = l.cex)
  
  
}



manipulate_WL_files <- function(df, year.plotted){
  
  df %>% 
    mutate(Date = as.Date(Date, "%m/%d/%Y",tz="America/New_York"))%>%
    filter(Date >= make_date(year=year.plotted, month = 1, day=1))
  
}

##outputs predicted, observed data
manipulate_data_for_plots <- function(scenario.num,year){
  
  
  Current.measure.date <- "Measured on July 5, 2020"
  last.measure.date <- "Measured on July 5, 2020"
  
  
  #formatDate
  LLCFpump.strategies.2020[,'Date'] <- as.Date(LLCFpump.strategies.2020[,'Date'],"%m/%d/%Y")
  
  ind.s <- which(LLCFpump.strategies.2020[,'Strategy']== scenario.num)
  
  predicted.wl <- LLCFpump.strategies.2020[ind.s, c('Date','CellC.masl','CellD.masl','CellE.masl')]
  
  CellC.masl.a <- manipulate_WL_files(WL_CellC.masl.a,year)
  CellD.masl.a<- manipulate_WL_files(WL_CellD.masl.a,year)
  CellE.masl.a<-  manipulate_WL_files(WL_CellE.masl.a,year)
  
  return(list(predicted.wl,
              CellC.masl.a,
              CellD.masl.a,
              CellE.masl.a))
  
}

plot_observed_vs_predicted <- function(pred_df, obs_df_C, obs_df_D, obs_df_E, scenario.num, year, variable){                                                                                          
  
  arrow.start.x <- obs_df_E$Date[te] %m+% months(4)
  arrow.start.y <- obs_df_E$y[te] + 5
  
  Current.measure.date <- "Measured on July 5, 2020"
  last.measure.date <- "Measured on July 5, 2020"
  
  
  ##predicted lines
  ggplot(pred_df, aes(Date)) + 
    
    geom_line(aes(y = CellD.masl, colour = "Cell D")) +
    geom_line(aes(y = CellE.masl, colour = "Cell E")) +
    geom_hline(yintercept=449.3, linetype="dashed", color = "blue") +
    geom_hline(yintercept=447, linetype="dashed", color = "blue") +
    geom_hline(yintercept=458, linetype="dashed", color = "purple") +
    geom_hline(yintercept=445.5, linetype="dashed", color = "orange") +
    
    ##obs values
    geom_point(data=obs_df_C, aes (y=y),colour="purple") +  
    geom_point(data=obs_df_D, aes (y=y),colour="blue") +  
    geom_point(data=obs_df_E, aes (y=y),colour="orange") +
    
    
    geom_segment(data=obs_df_E, aes(x=Date[te] %m+% months(4), xend=Date[te], y=y[te]+5, yend=y[te]), 
                 arrow = arrow(length = unit(0.1, "cm"))) +
    
    annotate("text", x = arrow.start.x, y = arrow.start.y, label = Current.measure.date, hjust = 0) +
    
    
    annotate("text", x=make_date(year,12,01), y=450, label= "Cell D and E Max = 449.3") +
    annotate("text", x=make_date(year,4,01), y=446, label= "Cell D Min = 447") +
    annotate("text", x=make_date(year,4,01), y=459, label= "Cell C Max = 458") +
    annotate("text", x=make_date(year,4,01), y=444, label= "Cell E Min = 445.5") +
    
    ggtitle(paste0("Scenario ", scenario.num)) +
    scale_x_date(date_breaks = "2 month",labels = date_format("%b-%y"),limits = as.Date(c(make_date(year,01,01),make_date(year,01,01) + years(2)))) +
    scale_y_continuous(limits = c(440,465)) +
    scale_color_manual(values=c("blue","orange"),name="Predicted")+
    scale_fill_manual(name="Actual",values=c("blue","orange")) +
    
    xlab("Date") +
    ylab(variable)  
  
  
  
  
}



manipulate_data_for_Potassium <- function(scenario.num, year){
  
  
  ind.s <- which(LLCFpump.strategies.2020[,'Strategy']== scenario.num)
  predicted <- LLCFpump.strategies.2020[ind.s, c('Date','CellE.K','CellD.K')]
  
  ##Predicted data
  predicted.K <- melt(predicted, id=c("Date")) 
  predicted.K[,'Type'] <- rep('Predicted',nrow(predicted.K))
  predicted.K[,'Date'] <- as.Date(predicted.K[,'Date'],"%m/%d/%Y")
  predicted.K[,'variable'] <- gsub('CellE.K','Cell E',predicted.K$variable)
  predicted.K[,'variable'] <- gsub('CellD.K','Cell D',predicted.K$variable)
  
  
  
  ##observed data
  dataAll.2020 <- getData(observedin,param,year)
  CellD.All <- dataAll.2020[which(dataAll.2020$LakeID=='Cell D'),]
  CellD.average <- na.omit(StationMeans(CellD.All))
  
  CellE.All <- dataAll.2020[which(dataAll.2020$LakeID=='Cell E'| dataAll.2020$LakeID=='161630 (LLCF)'),]
  CellE.average <- na.omit(StationMeans(CellE.All))
  
  
  
  actual.K <- rbind(CellD.average, CellE.average)
  actual.K[,'Type']<- rep('Actual',nrow(actual.K))
  actual.K <- actual.K[c('Date','LakeID','Impute','Type')]
  names(actual.K) <- names(predicted.K)
  ##rename 161630 to cell E for plotting
  actual.K$variable[actual.K$variable =='161630 (LLCF)']<- 'Cell E'
  
  mydata <- rbind(predicted.K,actual.K)
  mydata[,'Legend'] <- paste(mydata[,'variable'],"-",mydata[,'Type'])
  
  
  return(mydata)
  
}



plot_observed_vs_predicted_Potassium <- function(df, scenario.num, year, variable, para.eqc){                                                    
  
  my_shapes = c(16,NA,16,NA)
  
  ggplot(data = df, aes(x = Date, y = value, color=Legend, shape=Legend )) +
    geom_path(data = filter(df, Legend == "Cell D - Predicted") ) +
    geom_path(data = filter(df, Legend == "Cell E - Predicted") ) +
    geom_point(data = filter(df, Legend %in% c("Cell D - Actual", "Cell E - Actual"))) +
    scale_colour_manual(values =  c("blue", "blue", "orange", "orange"),
                        guide = guide_legend(override.aes = list(
                          linetype = c( "blank","solid","blank","solid"),
                          shape = my_shapes))) +
    
    scale_shape_manual(values = my_shapes)+
    geom_hline(yintercept=para.eqc, linetype="dashed", color = "orange") +
    annotate("text", x=make_date(year,8,1), y=para.eqc+3, label= paste0("EQC = ", para.eqc ," mg/L")) +
    scale_x_date(date_breaks = "2 month",labels = date_format("%b-%y"),limits = as.Date(c(make_date(year,1,1),make_date(year,1,1)+years(2)))) +
    scale_y_continuous( limits = c(0,80)) +
    xlab("Date") +
    ylab(variable)+
    labs(title = paste0("Scenario ",scenario.num), 
         caption = "Observed concentrations are an average of the depths measured.\nCell E - Actual includes 1616-30.")+ 
    theme(plot.caption = element_text(hjust = 0))
  
  
}
