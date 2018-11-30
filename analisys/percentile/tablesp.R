#!/usr/bin/Rscript
# eval(parse(filename, encoding="UTF-8"))
# таблицы как я отчитываюсь
library(data.table)
library("Hmisc")
setwd("D:/Simulink/report")
# подготовка исходных данных
# datap<-read.table("protons_B.dat", header = TRUE)

getwd()
setwd("D:/Ivan/_flash backup 2014/SINP/Группировка/2017 Группировка/radmonitoring/radmonitoring/analisys/percentile")
setwd("D:/Ivan/_flash backup 2014/SINP/Группировка/2017 Группировка/radmonitoring/radmonitoring/analisys/percentile/report3")

list.files()

# Start the clock!
ptm <- proc.time()
# datap<-fread("protons_B.dat", header = TRUE)
casing <-'D'
datap<-read.table(paste("protons_",casing,".dat", sep=''), header = TRUE)
# Stop the clock
proc.time() - ptm


breaks <- c(0,2000,4000,9000,15000,30000,53000,100000,160000, Inf)
labels <- c('p0','p1','p2','p3','p4','p5','p6','p7','p8')
datap$bins <- cut(datap$E0.keV, breaks, labels= labels)
least.thresholds = c(150, 120, 800, 200)


# полное энерговыделение в детекторах прибора---------------
datap$sumdE <- datap$dE.det1 + datap$dE.det2 + datap$dE.det3 + datap$dE.det4
datap.fullAbsorption <- datap[(datap$E0.keV - datap$sumdE)/datap$E0.keV< 0.05,]
plot(datap.fullAbsorption$sumdE ~ datap.fullAbsorption$E0.keV)
plot(datap.fullAbsorption$sumdE ~ datap.fullAbsorption$ang_z)

datap.collimator <- datap[datap$ang_z < 17,]
plot(datap.collimator$sumdE ~ datap.collimator$E0.keV, asp = 1)
plot(dE.det4 ~ dE.det3:dE.det2, data=datap.collimator)


# training data -----------------------------------------------------------

datap.filterLow <-datap[   
  (datap$dE.det2 >= 120)
  &(datap$ang_z < 17),]
datap.filterLow <-datap.filterLow[    
  (datap.filterLow$E0.keV >= 15000)|(datap.filterLow$dE.det1 >= 150)    ,]
datap.filterLow <-datap.filterLow[    
  (datap.filterLow$E0.keV <= 9000)|(datap.filterLow$dE.det3 >= 800)    ,]
datap.filterLow <-datap.filterLow[    
  (datap.filterLow$E0.keV <= 53000)|(datap.filterLow$dE.det4 >= 200)    ,]

##### OPTION 1: hexbin from package 'hexbin' #######
# Color housekeeping
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
library(hexbin)
# Create hexbin object and plot
# h <- hexbin(datap.collimator[,1,10 ])
# plot(h)
# plot(h, colramp=rf)
hexbinplot(sumdE/1000~E0.keV/1000, data=datap.collimator, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 50,
           main = paste("Поток протонов через коллиматор, ",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")

hexbinplot(sumdE/1000~E0.keV/1000, data=datap, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=2,
           xbins = 100,
           main = paste("Полный поток протонов через прибор,",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")
ppi <- 300
png(paste("trainproton",casing,".png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
hexbinplot((sumdE/1000)~(E0.keV/1000), data=datap.filterLow, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=2,
           xbins = 100,
           main = paste("поток протонов через прибор, Триггер D2, коллиматор, корпус",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")
dev.off()
# hexbinplot(sumdE~log(E0.keV), data=datap.filterLow, colramp=rf,
#            aspect=1, trans=log, inv=exp,
#            mincnt=2,
#            xbins = 100,
#            main = "поток протонов через прибор, D Триггер D2, коллиматор",
#            ylab= "Суммарное энерговыделение в детекторах прибора")

boxplot(dE.det1~bins, data = datap.filterLow)
boxplot(dE.det2~bins, data = datap.filterLow)
boxplot(dE.det3~bins, data = datap.filterLow)
boxplot(dE.det4~bins, data = datap.filterLow)

hexbinplot(datap$dE.det1~datap$dE.det2, data=datap.filterLow, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1)

hexbinplot(datap$dE.det2~datap$dE.det3, data=datap.filterLow, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1)
hexbinplot(datap$dE.det3~datap$dE.det4, data=datap.filterLow, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1)
# thresholds --------------------------------------------------------------
GetThresholds <- function(data = datap, quant=5 ){
  
  thres<- apply(data[,2:5],2,
                quantile,
                probs=c(quant, 100-quant)/100)
  
  # додумать надо
  
  #   cat("Was:\n", thres)  
  for(i in 1:4){
    # делаем замену обоих порогов в случае малых значений на ноль и младший порог
    if (  (thres[1,i] < least.thresholds[i])&
            (thres[2,i] < least.thresholds[i])){
      thres[1,i] <- 0
      thres[2,i] <- least.thresholds[i]
    }
    
    # и замену младшего порога в случае если только он попал
    if (  (thres[1,i] < least.thresholds[i])&(thres[1,i] > least.thresholds[i]/2)&
            (thres[2,i] > least.thresholds[i])){
      thres[1,i] <- least.thresholds[i]
    }
    # и замену младшего порога в случае если только он попал
    if (  (thres[1,i] < least.thresholds[i]/2)&
            (thres[2,i] > least.thresholds[i])){
      thres[1,i] <- 0
    }
  }
  #   cat("Now:\n", thres)     
  
  return(thres)
}
# 595 ---------------------------------------------------------------------
# должна быть таблица 
# д1   д2   д3   д4
# 5 95 5 95...
# 
# table.595 <- by(datap, datap[,"bins"], GetThresholds,  quant = 5)
# table.595 <- by(datap.collimator, datap.collimator[,"bins"], GetThresholds,  quant = 5)
table.595 <- by(datap.filterLow, datap.filterLow[,"bins"], GetThresholds,  quant = 5)
# table.595 <- by(datap.fullAbsorption, datap.fullAbsorption[,"bins"], GetThresholds,  quant = 5)

# есть таблица!
table.595<-table.595[-1]
# выполняем округление до десятков кэВ
table.595 <- lapply(table.595, round, -1)
print(table.595) 

# 2575 -------------------------------------------------------------------
# table.2575 <- by(datap, datap[,"bins"], GetThresholds,  quant = 25)
# table.2575 <- by(datap.collimator, datap.collimator[,"bins"], GetThresholds,  quant = 25)
table.2575 <- by(datap.filterLow, datap.filterLow[,"bins"], GetThresholds,  quant = 25)
# table.2575 <- by(datap.fullAbsorption, datap.fullAbsorption[,"bins"], GetThresholds,  quant = 25)
table.2575<-table.2575[-1]
# выполняем округление до десятков кэВ
table.2575 <- lapply(table.2575, round, -1)
print(table.2575)  

# classics ----------------------------------------------------------------
GetClassicsThresholds <- function( ){
  p1 <- matrix(c(150,2200,120,3300,0,800,0,200), ncol=4)
  colnames(p1) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p1) <- c("low","high")
  p1 <- as.table(p1)
  ##########################################################
  p2 <- matrix(c(150,1000,3300,9000,0,1800,0,2000), ncol=4)
  colnames(p2) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p2) <- c("low","high")
  p2 <- as.table(p2)
  ##########################################################
  p3 <- matrix(c(0,Inf,3300,9000,1800,10000,0,200), ncol=4)
  colnames(p3) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p3) <- c("low","high")
  p3 <- as.table(p3)
  ##########################################################
  p4 <- matrix(c(0,Inf,1700,3300,10000,28000,0,200), ncol=4)
  colnames(p4) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p4) <- c("low","high")
  p4 <- as.table(p4)
  ##########################################################
  p5 <- matrix(c(0,Inf,1000,1700,28000,Inf,0,200), ncol=4)
  colnames(p5) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p5) <- c("low","high")
  p5 <- as.table(p5)
  ##########################################################
  p6 <- matrix(c(0,Inf,600,1000,28000,Inf,0,200), ncol=4)
  colnames(p6) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p6) <- c("low","high")
  p6 <- as.table(p6)
  ##########################################################
  p7 <- matrix(c(0,Inf,300,1000,10000,28000,1000,Inf), ncol=4)
  colnames(p7) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p7) <- c("low","high")
  p7 <- as.table(p7)
  ##########################################################
  p8 <- matrix(c(500,2200,0,Inf,0,Inf,0,Inf), ncol=4)
  colnames(p8) <- c("dE.det1","dE.det2","dE.det3","dE.det4")
  rownames(p8) <- c("low","high")
  p8 <- as.table(p8)
  ##########################################################
  # print(table.classic)
  table.classic <- list(p1,p2,p3,p4,p5,p6,p7,p8)
  # table.classic<- vector(mode = "list", length = 8)
  # table.classic[[1]] <- p1;
  # table.classic[[2]] <- p2;
  # table.classic[[3]] <- p3;
  # table.classic[[4]] <- p4;
  # table.classic[[5]] <- p5;
  # table.classic[[6]] <- p6;
  # table.classic[[7]] <- p7;
  # table.classic[[8]] <- p8;
  # print(table.classic)
  return(table.classic)
}
table.classic <- GetClassicsThresholds()
print(table.classic)



# plotting ----------------------------------------------------------------
# 
# дополняем ее столбцом по эффективности критерия и селективности
# 
# строим графики по селективности для каждого типа криетриев
# 5-95 25-75 и классики

plotdEfromE4bwruBig <- function(data, main = "", orig, tab){
  
  layout(matrix(rep(1:12), 3, 4, byrow = TRUE), 
         widths=c(3,1,3,1),
         heights=c(4,4,1)
  )
  
  color1 = "navy";
  color2 = "olivedrab";
  color3 = "turquoise";
  color4 = "greenyellow";
  color5 = "red";
  markers1=c(1, 0);
  markers2=c(16, 17);
  #par(fig=c(0,0.25,0,1),new=TRUE)
  
  #par(fig=c(0.2,1,0,1),new=FALSE)
  
  par(mar=c(5.1,4.1,4.1,0.1))  
  plot(orig$E0.keV,orig$dE.det1, 
       ylim=range(c(orig$dE.det1,data$dE.det1)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       axes=TRUE,ann=FALSE,       
       col= ifelse(orig$ang_z < 17, color3,  color4),
       pch = ifelse(orig$ang_z < 17, markers2[1],markers2[2]))
  axis(4)
  par(new=T)  
  plot(data$E0.keV,data$dE.det1,
       # main="детектор 1",
       # xlab="E0, кэВ",     
       # ylab="dE, кэВ",    
       main="detector 1",
       xlab="E0, keV",     
       ylab="dE, keV",
       ylim=range(c(orig$dE.det1,data$dE.det1)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       col= ifelse(data$ang_z < 17, color1,  color2),
       pch = ifelse(data$ang_z < 17, markers1[1],markers1[2]))
  #   axis(1, 900:2000)
  axis(2)
  par(new=F) 
  rect(xleft =min(c(orig$E0.keV,data$E0.keV)),
       xright=max(c(orig$E0.keV,data$E0.keV)), 
       ybottom = tab[1,1],ytop = tab[2,1], border = color5)
  box()
  minor.tick(nx=0, ny=4, tick.ratio=0.5)
  legend("topright", 
  #        c("коллиматор", "корпус",
  #                      "первичный поток в коллиматоре"
  #                      #                        ,"поток ч/з корпус"
  # ), 
  c("collimator selected", 
    "casing selected",
    "collimator original",
    "casing original"),
  pch=c(markers1,markers2), 
  cex=.8, col=c(color1,  color2, color3,  color4))
  
  par(mar=c(5.1,0.1,4.1,2.1))
  boxplot(orig$dE.det1~orig$dE.det1>150,  axes=FALSE,   varwidth=T)
  axis(1,at=1:2,labels= paste(c("dE<150","dE>150")), las=2)
  
  
  
  #par(fig=c(0,0.25,0,1),new=TRUE)
  
  
  #par(fig=c(0.2,1,0,1),new=FALSE)
  
  par(mar=c(5.1,4.1,4.1,0.1))
  plot(orig$E0.keV,orig$dE.det2, 
       ylim=range(c(orig$dE.det2,data$dE.det2)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       axes=FALSE,ann=FALSE,       
       col= ifelse(orig$ang_z < 17, color3,  color4),
       pch = ifelse(orig$ang_z < 17, markers2[1],markers2[2]))
  axis(4)
  par(new=T)
  plot(data$E0.keV,data$dE.det2,
       ylim=range(c(orig$dE.det2,data$dE.det2)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       main="detector 2",
       xlab="E0, keV",     
       ylab="dE, keV",
       col= ifelse(data$ang_z < 17, color1,  color2),
       pch = ifelse(data$ang_z < 17, markers1[1],markers1[2]))
  
  minor.tick(nx=0, ny=4, tick.ratio=0.5)
  #   legend("topright", c("коллиматор", "корпус"), pch=markers, 
  #          cex=.8, col=c(color1,  color2))
  rect(xleft =min(c(orig$E0.keV,data$E0.keV)),
       xright=max(c(orig$E0.keV,data$E0.keV)), 
       ybottom = tab[1,2],ytop = tab[2,2], border = color5)
  par(mar=c(5.1,0.1,4.1,2.1))
  boxplot(orig$dE.det2~orig$dE.det2>120, axes=FALSE,  varwidth=T)
  axis(1,at=1:2,labels= paste(c("dE<120","dE>120")), las=2)
  
  
  #par(fig=c(0,0.25,0,1),new=TRUE)
  
  #par(fig=c(0.2,1,0,1),new=FALSE)
  
  par(mar=c(5.1,4.1,4.1,0.1))
  plot(orig$E0.keV,orig$dE.det3, 
       ylim=range(c(orig$dE.det3,data$dE.det3)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       axes=FALSE,ann=FALSE,       
       col= ifelse(orig$ang_z < 17, color3,  color4),
       pch = ifelse(orig$ang_z < 17, markers2[1],markers2[2]))
  axis(4)
  par(new=T)
  plot(data$E0.keV,data$dE.det3,
       ylim=range(c(orig$dE.det3,data$dE.det3)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       main="detector 3",
       xlab="E0, keV",     
       ylab="dE, keV",
       col= ifelse(data$ang_z < 17, color1,  color2),
       pch = ifelse(data$ang_z < 17, markers1[1],markers1[2]))
  minor.tick(nx=0, ny=4, tick.ratio=0.5)
  #   legend("topright", c("коллиматор", "корпус"), pch=markers, 
  #          cex=.8, col=c(color1,  color2))
  rect(xleft =min(c(orig$E0.keV,data$E0.keV)),
       xright=max(c(orig$E0.keV,data$E0.keV)), 
       ybottom = tab[1,3],ytop = tab[2,3], border = color5)
  par(mar=c(5.1,0.1,4.1,2.1))
  boxplot(orig$dE.det3~orig$dE.det3>800, axes=FALSE,  varwidth=T)
  axis(1,at=1:2,labels= paste(c("dE<800","dE>800")), las=2)
  
  
  
  
  #par(fig=c(0,0.25,0,1),new=TRUE)
  
  #par(fig=c(0.2,1,0,1),new=FALSE)
  
  par(mar=c(5.1,4.1,4.1,0.1))
  plot(orig$E0.keV,orig$dE.det4, 
       ylim=range(c(orig$dE.det4,data$dE.det4)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       axes=FALSE,ann=FALSE,       
       col= ifelse(orig$ang_z < 17, color3,  color4),
       pch = ifelse(orig$ang_z < 17, markers2[1],markers2[2]))
  axis(4)
  par(new=T)
  plot(data$E0.keV,data$dE.det4,
       ylim=range(c(orig$dE.det4,data$dE.det4)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       main="detector 4",
       xlab="E0, keV",     
       ylab="dE, keV", 
       col= ifelse(data$ang_z < 17, color1,  color2),
       pch = ifelse(data$ang_z < 17, markers1[1],markers1[2]))
  minor.tick(nx=0, ny=4, tick.ratio=0.5)
  #   legend("topright", c("коллиматор", "корпус"), pch=markers, 
  #          cex=.8, col=c(color1,  color2))
  rect(xleft =min(c(orig$E0.keV,data$E0.keV)),
       xright=max(c(orig$E0.keV,data$E0.keV)), 
       ybottom = tab[1,4],ytop = tab[2,4], border = color5)
  par(mar=c(5.1,0.1,4.1,2.1))
  boxplot(orig$dE.det4~orig$dE.det4>200, axes=F,  varwidth=T)
  #   axis(1,at=1:1,labels= "dE>200", las=2)
  axis(1,at=1:2,labels= paste(c("dE<200","dE>200")), las=2)
  
  
  
  
  dole <- nrow(data[(data$ang_z < 17),])/nrow(data[(data$ang_z >= 17),])
  a<-  paste("В отобранных событиях коллиматор/боковые:"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
             , signif(dole, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-3, cex=0.8)
  
  # sensetivity <- nrow(data)/nrow(orig)
  sensetivity <- nrow(data[(data$E0.keV >= min(orig$E0.keV)) &
                             (data$E0.keV <= max(orig$E0.keV)),])/nrow(orig)
  a<-  paste("Чувствительность(геометрический фактор):"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
             , signif(sensetivity, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-2, cex=0.8)
  
  # selectivity <- nrow(data[(data$E0.keV >= min(orig$E0.keV)) &
  #                            (data$E0.keV <= max(orig$E0.keV)),])/nrow(data)
  precision <- nrow(data[(data$E0.keV >= min(orig$E0.keV)) &
                           (data$E0.keV <= max(orig$E0.keV)),])/nrow(data)
  a<-  paste("Точность(количество правильно определенных частиц):"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
             , signif(precision, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-1, cex=0.8)
  
  #   dole <- nrow(orig[(orig$ang_z < 17),])/nrow(orig[(orig$ang_z >= 17),])
  #   a<-  paste("Первичные коллиматор/боковые:"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
  #              , signif(dole, digits = 1) ,sep = " ")
  #   
  #   
  #   mtext(a, side=1, outer=TRUE, line=-1)
  
  mtext(main, side=3,  outer=TRUE, line=-1, cex=0.8)
}


plotdEfromE4bwruBig1 <- function(data, main = "", orig, tab){
  
  layout(matrix(rep(1:12), 3, 4, byrow = TRUE), 
         widths=c(3,1,3,1),
         heights=c(4,4,1)
  )
  
  color1 = "navy";
  color2 = "olivedrab";
  color3 = "turquoise";
  color4 = "greenyellow";
  color5 = "red";
  markers1=c(1, 0);
  markers2=c(16, 17);
  
  par(mar=c(5.1,4.1,4.1,0.1))  
  plot(orig$E0.keV,orig$dE.det1, 
       ylim=range(c(orig$dE.det1,data$dE.det1)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       axes=TRUE,ann=FALSE,       
       col= ifelse(orig$ang_z < 17, color3,  color4),
       pch = ifelse(orig$ang_z < 17, markers2[1],markers2[2]))
  axis(4)
  par(new=T)  
  plot(data$E0.keV,data$dE.det1,
       main="детектор 1",
       xlab="E0, кэВ",     
       ylab="dE, кэВ",        
       ylim=range(c(orig$dE.det1,data$dE.det1)),
       xlim=range(c(orig$E0.keV,data$E0.keV)),
       col= ifelse(data$ang_z < 17, color1,  color2),
       pch = ifelse(data$ang_z < 17, markers1[1],markers1[2]))
  #   axis(1, 900:2000)
  axis(2)
  par(new=F) 
  rect(ybottom = tab[1,1],ytop = tab[2,1], border = color5)
  box()
  minor.tick(nx=0, ny=4, tick.ratio=0.5)
  legend("topright", c("коллиматор", "корпус",
                       "первичный поток в коллиматоре"
                       #                        ,"поток ч/з корпус"
  ), pch=c(markers1,markers2), 
  cex=.8, col=c(color1,  color2, color3,  color4))
  
  par(mar=c(5.1,0.1,4.1,2.1))
  boxplot(orig$dE.det1~orig$dE.det1>150,  axes=FALSE,   varwidth=T)
  axis(1,at=1:2,labels= paste(c("dE<150","dE>150")), las=2)
  
  # числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
  dole <- nrow(data[(data$ang_z < 17),])/nrow(data[(data$ang_z >= 17),])
  a<-  paste("В отобранных событиях коллиматор/боковые:"
             , signif(dole, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-3, cex=0.8)
  # числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
  sensetivity <- nrow(data)/nrow(orig)
  a<-  paste("Чувствительность(геометрический фактор):"
             , signif(sensetivity, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-2, cex=0.8)
  # числа боковых зарегистрированных пролетов к числу пролетов через коллиматор:   
  selectivity <- nrow(data[(data$E0.keV >= min(orig$E0.keV)) &
                             (data$E0.keV <= max(orig$E0.keV)),])/nrow(data)
  a<-  paste("Селективность(количество правильно определенных частиц):"
             , signif(selectivity, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-1, cex=0.8)
  
  
  mtext(main, side=3,  outer=TRUE, line=-1, cex=0.8)
}

plotChannel <- function( tab, n){
  
  proton<-datap[((datap$ang_z < 17) & (datap$bins == paste("p", n, sep=""))), ]
#   proton<-datap[( (datap$bins == paste("p", n, sep=""))), ]
  
  channel<-datap[((datap$dE.det1 >= tab[1,1]) & datap$dE.det1 <= tab[2,1])& 
                   ((datap$dE.det2 >= tab[1,2]) & datap$dE.det2 <= tab[2,2])& 
                   ((datap$dE.det3 >= tab[1,3]) & datap$dE.det3 <= tab[2,3])& 
                   ((datap$dE.det4 >= tab[1,4]) & datap$dE.det4 <= tab[2,4]),]
  
  main = paste("Канал",n,":",round(min(proton$E0.keV), -2),round(max(proton$E0.keV), -2),
               "Отбор:", deparse(substitute(tab)),
               "Корпус",casing, sep = " ")
  
  plotdEfromE4bwruBig(channel, main, proton, tab)
  
}
plotChannelSide <- function( tab, n){
  
  #   proton<-datap[((datap$ang_z < 17) & (datap$bins == paste("p", n, sep=""))), ]
  proton<-datap[( (datap$bins == paste("p", n, sep=""))), ]
  
  channel<-datap[((datap$dE.det1 >= tab[1,1]) & datap$dE.det1 <= tab[2,1])& 
                   ((datap$dE.det2 >= tab[1,2]) & datap$dE.det2 <= tab[2,2])& 
                   ((datap$dE.det3 >= tab[1,3]) & datap$dE.det3 <= tab[2,3])& 
                   ((datap$dE.det4 >= tab[1,4]) & datap$dE.det4 <= tab[2,4]),]
  
  main = paste("Канал",n,":",round(min(proton$E0.keV), -2),round(max(proton$E0.keV), -2),
               "Отбор:", deparse(substitute(tab)),
               "Корпус",casing, sep = " ")
  
  plotdEfromE4bwruBig(channel, main, proton, tab)
  
}
savePlotCannel <- function(n=1){
  
  ppi <- 300
  png(paste("proton",n,casing,"classicen.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
  # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
  plotChannelSide(table.classic[[n]], n)
  dev.off()
  
  png(paste("proton",n,casing,"2575en.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
  # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
  plotChannelSide(table.2575[[n]], n)  
  dev.off()
  
  png(paste("proton",n,casing,"595en.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
  # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
  plotChannelSide(table.595[[n]], n)
  dev.off()
}

# строим гистограмму энергии в каждом канале
# for (i in 1:8)
#   savePlotCannel(i)


# тест с другими корпусами ------------------------------------------------

casing <-'A'
datap<-read.table(paste("protons_",casing,".dat", sep=''), header = TRUE)
datap$bins <- cut(datap$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:8)
  savePlotCannel(i)
casing <-'B'
datap<-read.table(paste("protons_",casing,".dat", sep=''), header = TRUE)
datap$bins <- cut(datap$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:8)
  savePlotCannel(i)
casing <-'C'
datap<-read.table(paste("protons_",casing,".dat", sep=''), header = TRUE)
datap$bins <- cut(datap$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:8)
  savePlotCannel(i)
casing <-'D'
datap<-read.table(paste("protons_",casing,".dat", sep=''), header = TRUE)
datap$bins <- cut(datap$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:8)
  savePlotCannel(i)


#  кросс тест с электронами ---------------------------------------------------------------------
# casing <-'D'
# datap<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)
# datap$bins <- cut(datap$E0.keV, breaks, labels= labels)
# plotCrossChannel <- function(n=1){
#   
#   ppi <- 300
#   png(paste("crossEinP",n,casing,"classic.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
#   # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
#   plotChannel(table.classic[[n]], n)
#   # plotChannel(table.2575[[n+1]], n)
#   # plotChannel(table.595[[n+1]], n)
#   dev.off()
#   
#   png(paste("crossEinP",n,casing,"2575.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
#   # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
#   plotChannel(table.2575[[n]], n)  
#   dev.off()
#   
#   png(paste("crossEinP",n,casing,"595.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
#   # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
#   plotChannel(table.595[[n]], n)
#   dev.off()
# }
# # строим гистограмму энергии в каждом канале
# for (i in 1:8)
#   plotCrossChannel(i)
# off active device  ----------------------------------------------------------

dev.off()

# parcoord ----------------------------------------------------------------
library(MASS)
# library(colorRamps)

# data(mtcars)
# k <- blue2red(100)
n = 9
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 0.2))
# k <- adjustcolor(brewer.pal(3, "Set1")[titdf2$Survived], alpha=.2)
datapdf <- as.data.frame(lapply(as.data.frame(datap), as.numeric))
# new columns with jittered values
datapdf[,9:10] <- lapply((datapdf[,c(6,8)]), jitter)
# x <- cut( datap$E0.keV, 100)

ppi <- 300
png(paste("parallprot1",casing,".png", sep=""), width=6*ppi, height=6*ppi, res=ppi)


op <- par(mar=c(3, rep(.1, 3)))
parcoord(datapdf[,c(1,2,9,3,10,4,7)], col=colr[as.numeric(datapdf$bins)],
         main = paste("????? ?????????? ????? ??????, \n , ??????",casing, sep=" ")
)
# par(op)
dev.off()