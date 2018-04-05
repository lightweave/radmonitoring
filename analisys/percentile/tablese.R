#!/usr/bin/Rscript
# eval(parse(filename, encoding="UTF-8"))
# таблицы как я отчитываюсь
library(data.table)
library("Hmisc")
library('hexbin')
library('readr')
library('dplyr')
library('tidyr')
setwd("D:/Simulink/report2")
# подготовка исходных данных
# datae<-read.table("electrons_B.dat", header = TRUE)

# загрузка тренировочного потока D ----------------------------------------


# Start the clock!
ptm <- proc.time()
# datae<-fread("electrons_B.dat", header = TRUE)
casing <-'ABCold'
# datae<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)

datae<-read.table("d:/Simulink/data/alldet/electr_A_L32.dat", header = TRUE)
datae <- rbind(datae, read.table("d:/Simulink/data/alldet/electr_B_L32.dat", header = TRUE))
datae <- rbind(datae, read.table("d:/Simulink/data/alldet/electr_C_L32.dat", header = TRUE))
# Stop the clock
proc.time() - ptm


# breaks <- c(0,150,350,600,1000,2000,4000,10000)
# labels=c('e0','e1','e2','e3','e4','e5','e6')
breaks <- c(150,350,600,1000,2000,4000,10000)
labels=c('e1','e2','e3','e4','e5','e6')
datae$bins <- cut(datae$E0.keV, breaks, labels= labels)
least.thresholds = c(150, 120, 800, 200)


# полное энерговыделение в детекторах прибора---------------
datae$sumdE <- datae$dE.det1 + datae$dE.det2 + datae$dE.det3 + datae$dE.det4
datae.fullAbsorption <- datae[(datae$E0.keV - datae$sumdE)/datae$E0.keV< 0.05,]
plot(datae.fullAbsorption$sumdE ~ datae.fullAbsorption$E0.keV)
# plot(datae.fullAbsorption$sumdE ~ datae.fullAbsorption$ang_z)

# коллиматор---------------------------------------------------
datae.collimator <- datae[datae$ang_z < 17,]
plot(datae.collimator$sumdE ~ datae.collimator$E0.keV)
# 
# plot(dE.det4 ~ dE.det3:dE.det2, data=datae.collimator)

# training data filterlow in det2 -----------------------------------------------------------
datae.filterLow <- datae[(datae$dE.det2 >= 120),]
# datae.filterLow <- datae[(datae$dE.det2 >= 120)&(datae$ang_z < 17),]
# datae.filterLow <- datae.filterLow[(datae.filterLow$dE.det1 >= 150),]
# datae.filterLow <- datae.filterLow[(datae.filterLow$dE.det3 >= 800),]
# datae.filterLow <- datae.filterLow[(datae.filterLow$dE.det4 >= 200),]

##### OPTION 1: hexbin from package 'hexbin' #######
# Color housekeeping
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
library(hexbin)
# Create hexbin object and plot
# h <- hexbin(datae.collimator[,1,10 ])
# plot(h)
# plot(h, colramp=rf)
hexbinplot(sumdE~E0.keV, data=datae, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 100,
           main = paste("Полный поток электронов через прибор,",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")


hexbinplot(sumdE~E0.keV, data=datae.collimator, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 100,
           main = paste("Поток электронов через коллиматор, ",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")

hexbinplot(sumdE~E0.keV, data=datae.filterLow, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 100,
           main = paste("Поток электронов через прибор, Триггер Д2",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")

hexbinplot(sumdE~E0.keV, data=datae.fullAbsorption, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 100,
           main = paste("поток полностью поглощенных электронов через прибор,",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")



ppi <- 300
png(paste("trainelec",casing,".png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
hexbinplot(sumdE~E0.keV, data=datae.filterLow, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 100,
           main = paste("поток электронов через прибор, Триггер D2,\n , корпус",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")
dev.off()
ppi <- 300
png(paste("trainelec1",casing,".png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
hexbinplot(sumdE~E0.keV, data=datae.fullAbsorption, colramp=rf,
           aspect=1, trans=log, inv=exp,
           mincnt=1,
           xbins = 100,
           main = paste("поток электронов через прибор, ~полное поглощение,\n , корпус",casing, sep=" "),
           ylab= "Суммарное энерговыделение в детекторах прибора")
dev.off()
# hexbinplot(sumdE~log(E0.keV), data=datae.filterLow, colramp=rf,
#            aspect=1, trans=log, inv=exp,
#            mincnt=2,
#            xbins = 100,
#            main = "поток электронов через прибор, D Триггер D2, коллиматор",
#            ylab= "Суммарное энерговыделение в детекторах прибора")

boxplot(dE.det1~bins, data = datae.fullAbsorption)
boxplot(dE.det2~bins, data = datae.fullAbsorption)
boxplot(dE.det3~bins, data = datae.fullAbsorption)
boxplot(dE.det4~bins, data = datae.fullAbsorption)

boxplot(dE.det1~bins, data = datae.filterLow)
boxplot(dE.det2~bins, data = datae.filterLow)
boxplot(dE.det3~bins, data = datae.filterLow)
boxplot(dE.det4~bins, data = datae.filterLow)

boxplot(dE.det1~bins, data = datae)
boxplot(dE.det2~bins, data = datae)
boxplot(dE.det3~bins, data = datae)
boxplot(dE.det4~bins, data = datae)

boxplot(dE.det1:dE.det4~bins, data = datae)

hexbinplot(dE.det1~dE.det2, data=datae.fullAbsorption, colramp=rf,
           aspect=1, trans=log, inv=exp,xbins = 100,
           mincnt=1)

hexbinplot(dE.det2~dE.det3, data=datae.fullAbsorption, colramp=rf,
           aspect=1, trans=log, inv=exp,xbins = 100,
           mincnt=1)
hexbinplot(dE.det3~dE.det4, data=datae.fullAbsorption, colramp=rf,
           aspect=1, trans=log, inv=exp,xbins = 100,
           mincnt=1)




# 3d ----------------------------------------------------------------------


library(scatterplot3d)

scatterplot3d(datae.fullAbsorption$dE.det1,   # x axis
              datae.fullAbsorption$dE.det2,     # y axis
              datae.fullAbsorption$E0.keV    # z axis
              )

install.packages(c("rgl", "car"))
library("car", "rgl")
scatter3d(datae$dE.det2,   # x axis
          datae$E0.keV,     # y axis
          datae$dE.det3,    # z axis
              main="")

# thresholds --------------------------------------------------------------
GetThresholds <- function(data = datae, quant=5 ){
  
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
# table.595 <- by(datae, datae[,"bins"], GetThresholds,  quant = 5)
# table.595 <- by(datae.collimator, datae.collimator[,"bins"], GetThresholds,  quant = 5)
table.595 <- by(datae.fullAbsorption, datae.fullAbsorption[,"bins"], GetThresholds,  quant = 5)
# table.595 <- by(datae.fullAbsorption, datae.fullAbsorption[,"bins"], GetThresholds,  quant = 5)

# есть таблица!
table.595<-table.595[-1]
# выполняем округление до десятков кэВ
table.595 <- lapply(table.595, round, -1)
print(table.595) 

# 2575 -------------------------------------------------------------------
# table.2575 <- by(datae, datae[,"bins"], GetThresholds,  quant = 25)
# table.2575 <- by(datae.collimator, datae.collimator[,"bins"], GetThresholds,  quant = 25)
table.2575 <- by(datae.fullAbsorption, datae.fullAbsorption[,"bins"], GetThresholds,  quant = 25)
# table.2575 <- by(datae.fullAbsorption, datae.fullAbsorption[,"bins"], GetThresholds,  quant = 25)
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


# save tables -------------------------------------------------------------


sink("table595.txt")
print(table.595)
sink()
sink("table2575.txt")
print(table.2575)
sink()
table.595 <- read.delim("table595.txt")

# decision tree analisys --------------------------------------------------


library(rpart)
# drop e0
# grow tree 
# datae.fullAbsorption.sample <-sample_n(datae.fullAbsorption, 1000)
datae.fullAbsorption.sample <- datae.fullAbsorption[(datae.fullAbsorption$bins!='e0'),]
plot(table(datae.fullAbsorption.sample$bins))
fit <- rpart(bins ~ dE.det1 + dE.det2 + dE.det3 + dE.det4,
             method="class", data=datae.fullAbsorption.sample)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for datae.fullAbsorption.sample")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


datae.newdata<-datae[(datae$bins!='e0'),]
p <- predict(fit, datae.newdata, type = "class")

plot(table(p))
plot(table(datae.newdata$bins))
boxplot(datae.newdata$E0.keV~p)


# Quantile Regression -----------------------------------------------------
install.packages("quantreg")
library(quantreg)
qs <- 1:9/10
datae.fullAbsorption$sum <- datae.fullAbsorption$dE.det1+datae.fullAbsorption$dE.det2+datae.fullAbsorption$dE.det3+datae.fullAbsorption$dE.det4
qr2 <- rq(datae$E0.keV ~ (datae$dE.det1+datae$dE.det2+datae$dE.det3+datae$dE.det4),
          data=datae.fullAbsorption, tau = qs)
ggplot(datae.fullAbsorption, aes(E0.keV, dE.det1+dE.det2+dE.det3+dE.det4))  + geom_point() + geom_quantile(quantiles = qs)

summary(qr2)
plot(summary(qr2), parm="datae$dE.det1")
plot(summary(qr2))
plot(qr2)

# f <- function(x, a) sum((x-a)^2)
qr2 <- rq(datae$E0.keV ~ datae$dE.det1+datae$dE.det2+datae$dE.det3+datae$dE.det4,
          data=datae, tau = qs)


xyplot(foodexp~income , data =engel, 
       type = c("g"),
       auto.key=list(x=.8,y=.35,cex=.8,cex.title=.8, title="", points=TRUE), 
       scales=list(tck=-1),ylab=list("Food Expenditure",font=3),
       xlab=list("Household Income",font=3),
       panel=function(x,y,...){
         panel.xyplot(x,y)
         panel.grid()
         panel.abline(rq(y ~ x, tau = 0.5))
         panel.points(x, y, cex = 0.5, col = "blue")
         panel.abline(rq(y ~ x, tau = 0.5), col = "blue")
         panel.abline(lm(y ~ x), lty = 2, col = "red")
         taus <- c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95)
         for (i in 1:length(taus)) {
           panel.abline(rq(y ~ x, tau = taus[i]),
                        col = "gray")
         }
         
       }
)

# predict.percent ---------------------------------------------------------
predict.percent <- function(data,  orig, tab){
  return(data[((data$dE.det1 >= tab[1,1]) & data$dE.det1 <= tab[2,1])& 
                 ((data$dE.det2 >= tab[1,2]) & data$dE.det2 <= tab[2,2])& 
                 ((data$dE.det3 >= tab[1,3]) & data$dE.det3 <= tab[2,3])& 
                 ((data$dE.det4 >= tab[1,4]) & datae$dE.det4 <= tab[2,4]),]
  )
  
}

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
  rect(xleft =min(c(orig$E0.keV,data$E0.keV)),
       xright=max(c(orig$E0.keV,data$E0.keV)), 
       ybottom = tab[1,1],ytop = tab[2,1], border = color5)
  box()
  minor.tick(nx=0, ny=4, tick.ratio=0.5)
  legend("topright", c("коллиматор отбор", "корпус отбор",
                       "первичный поток в коллиматоре",
                       "первичный поток ч/з корпус"
  ), pch=c(markers1,markers2), 
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
       main="детектор 2",
       xlab="E0, кэВ",     
       ylab="dE, кэВ", 
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
       main="детектор 3",
       xlab="E0, кэВ",     
       ylab="dE, кэВ", 
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
       main="детектор 4",
       xlab="E0, кэВ",     
       ylab="dE, кэВ", 
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
  
  
  
  # Очень нужна информация об отношении бок - коллиматор зарегистрированных электронов 
  # в D2 и D3 во всех  электронных каналах для трех вариантов корпусов:
  # латунь, дюраль, дюраль с внутренней вставкой из вольфрама 
  # со стробом в D2 и без него..
  dole2 <- nrow(orig[(orig$ang_z <  17)&(orig$dE.det2 > 0)&(orig$dE.det3 > 0) ,])/
    nrow(orig[(orig$ang_z >= 17)&(orig$dE.det2 > 0)&(orig$dE.det3 > 0) ,])
  
  a<-  paste("Исходный поток в канале коллиматор-боковые (dE2,3 больше нуля):",  
             signif(dole2, digits = 2), " = ",
             nrow(orig[(orig$ang_z < 17)&(orig$dE.det2 > 0)&(orig$dE.det3 > 0) ,]),'/',
             nrow(orig[(orig$ang_z >= 17)&(orig$dE.det2 > 0)&(orig$dE.det3 > 0) ,]),
             sep = " ")
  mtext(a, side=1, outer=TRUE, line=-5, cex=0.8)
  
  dole1 <- nrow(orig[(orig$ang_z <  17) ,])/
           nrow(orig[(orig$ang_z >= 17) ,])
  
  a<-  paste("Исходный поток в канале коллиматор-боковые (без ограничений):",  
             signif(dole1, digits = 2), " = ",
             nrow(orig[(orig$ang_z < 17) ,]),'/',
             nrow(orig[(orig$ang_z >= 17) ,]),
             sep = " ")
  mtext(a, side=1, outer=TRUE, line=-4, cex=0.8)
  
  
  
  dole <- nrow(data[(data$ang_z < 17),])/nrow(data[(data$ang_z >= 17),])
  a<-  paste("В отобранных событиях коллиматор-боковые:"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
             , signif(dole, digits = 2), " = ",
             nrow(data[(data$ang_z < 17),]),'/',
             nrow(data[(data$ang_z >= 17),]),
             sep = " ")
  mtext(a, side=1, outer=TRUE, line=-3, cex=0.8)
  
  sensetivity <- nrow(data)/nrow(orig)
  a<-  paste("Чувствительность(геометрический фактор):"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
             , signif(sensetivity, digits = 2) ,sep = " ")
  mtext(a, side=1, outer=TRUE, line=-2, cex=0.8)
  
  selectivity <- nrow(data[(data$E0.keV >= min(orig$E0.keV)) &
                             (data$E0.keV <= max(orig$E0.keV)),])/nrow(data)
  a<-  paste("Селективность(количество правильно определенных частиц):"# числа боковых зарегистрированных пролетов к числу пролетов через коллиматор: 
             , signif(selectivity, digits = 2) ,sep = " ")
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
  proton<-datae[((datae$bins == paste("e", n, sep=""))), ]
  
  # proton<-datae[((datae$ang_z < 17) & (datae$bins == paste("e", n, sep=""))), ]
#   proton<-datae[( (datae$bins == paste("p", n, sep=""))), ]
  
  channel<-datae[((datae$dE.det1 >= tab[1,1]) & datae$dE.det1 <= tab[2,1])& 
                   ((datae$dE.det2 >= tab[1,2]) & datae$dE.det2 <= tab[2,2])& 
                   ((datae$dE.det3 >= tab[1,3]) & datae$dE.det3 <= tab[2,3])& 
                   ((datae$dE.det4 >= tab[1,4]) & datae$dE.det4 <= tab[2,4]),]
  
  main = paste("Канал",n,":",round(min(proton$E0.keV), -2),round(max(proton$E0.keV), -2),
               "Отбор:", deparse(substitute(tab)),
               "Корпус",casing, sep = " ")
  
  plotdEfromE4bwruBig(channel, main, proton, tab)
  
}
plotChannelSide <- function( tab, n){
  
  #   proton<-datae[((datae$ang_z < 17) & (datae$bins == paste("p", n, sep=""))), ]
  proton<-datae[( (datae$bins == paste("e", n, sep=""))), ]
  
  channel<-datae[((datae$dE.det1 >= tab[1,1]) & datae$dE.det1 <= tab[2,1])& 
                   ((datae$dE.det2 >= tab[1,2]) & datae$dE.det2 <= tab[2,2])& 
                   ((datae$dE.det3 >= tab[1,3]) & datae$dE.det3 <= tab[2,3])& 
                   ((datae$dE.det4 >= tab[1,4]) & datae$dE.det4 <= tab[2,4]),]
  
  main = paste("Канал",n,":",round(min(proton$E0.keV), -1),round(max(proton$E0.keV), -1),
               "Отбор:", deparse(substitute(tab)),
               "Корпус",casing, sep = " ")
  
  plotdEfromE4bwruBig(channel, main, proton, tab)
  
}
savePlotCannel <- function(n=1){
  
  ppi <- 600
  
  # png(paste("electron",n,casing,"classic.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
  # # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
  # plotChannelSide(table.classic[[n]], n)
  # dev.off()
  
  png(paste("electron",n,casing,"2575.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
  # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
  plotChannelSide(table.2575[[n]], n) 
  dev.off()
  
  png(paste("electron",n,casing,"595.png", sep=""), width=6*ppi, height=6*ppi, res=ppi)
  # pdf(paste("proton",n,casing,"classic.pdf", sep=""))
  plotChannelSide(table.595[[n]], n)
  dev.off()
}

# строим гистограмму энергии в каждом канале
for (i in 1:8)
  savePlotCannel(i)

# тест с другими корпусами ------------------------------------------------

casing <-'A'
datae<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)
datae$bins <- cut(datae$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:5)
  savePlotCannel(i)
casing <-'B'
datae<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)
datae$bins <- cut(datae$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:5)
  savePlotCannel(i)
casing <-'C'
datae<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)
datae$bins <- cut(datae$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:5)
  savePlotCannel(i)
casing <-'D'
datae<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)
datae$bins <- cut(datae$E0.keV, breaks, labels= labels)
# строим гистограмму энергии в каждом канале
for (i in 1:5)
  savePlotCannel(i)



# таблицы для тулупова по сравнению корпусов  -----------------------------

casing <-'A'
cat('\nКорпус',casing, 'Общее число частиц:', nrow(datae) )

 datae %>%
  group_by( ., E, dE.det2 > 0, dE.det3 > 0) %>%
  summarise( ., number=n())

 setwd("D:/Simulink/report2")
 datae<-read.table('var1_L14.dat', header = TRUE)
 cat('\nКорпус','var1_L14', 'Общее число частиц:', nrow(datae) )
 temp<-datae %>%
   group_by( .,ang_z >5, E0.keV >150, dE.det2 > 12, dE.det3 > 0) %>%
   summarise( ., number=n())
 write.csv2(temp, 'var1_L14.csv')
 
  # таблицы для тулупова по сравнению корпусов 2  -----------------------------
 
 
 cat('\nКорпус',casing, 'Общее число частиц:', nrow(datae) )
 
 datae %>%
   group_by( ., E, dE.det2 > 0, dE.det3 > 0) %>%
   summarise( ., number=n())
 
 setwd("d:/Simulink/report/")
 datae<-read.table('electrons_D.dat', header = TRUE)
 cat('\nКорпус','electrons_D', 'Общее число частиц:', nrow(datae) )
 temp<-datae %>%
   group_by( ., E0.keV >150, dE.det2 > 0, dE.det3 > 0, ang_z >17) %>%
   summarise( ., number=n())
 write.csv2(temp, 'electrons_D.csv')
 
 # таблицы для тулупова по сравнению корпусов 3  -----------------------------
 
 
 cat('\nКорпус',casing, 'Общее число частиц:', nrow(datae) )
 
 datae %>%
   group_by( ., E, dE.det2 > 0, dE.det3 > 0) %>%
   summarise( ., number=n())
 
 setwd("d:/Simulink/data/alldet/")
 datae<-read.table('electr_C_L14.dat', header = TRUE)
 cat('\nКорпус','var1_L34', 'Общее число частиц:', nrow(datae) )
 temp<-datae %>%
   group_by( ., E0.keV >150, dE.det2 > 0, dE.det3 > 0, CF ==TRUE) %>%
   summarise( ., number=n())
 write.csv2(temp, 'electr_C_L14.dat.csv')
 
 
 # таблицы для тулупова по сравнению корпусов 4  -----------------------------
 
 setwd("d:/Simulink/data/alldet/")
 datae<-read.table('electr_B_L32.dat', header = TRUE)
 cat('\nКорпус','electr_B_L32.dat', 'Общее число частиц:', nrow(datae) )
 temp<-datae %>%
   group_by( ., E0.keV >150, dE.det2 > 0, dE.det3 > 0, CF ==TRUE) %>%
   summarise( ., number=n())
 write.csv2(temp, 'electr_B_L32.dat.csv')
 # таблицы для тулупова по сравнению корпусов 5  -----------------------------
 
 setwd("d:/Simulink/data/alldet/")
 datae<-read.table('electr_D_L14.dat', header = TRUE)
 cat('\nКорпус','electr_D_L14.dat', 'Общее число частиц:', nrow(datae) )
 temp<-datae %>%
   group_by( ., E0.keV >150, dE.det2 > 0, dE.det3 > 0, CF ==TRUE) %>%
   summarise( ., number=n())
 write.csv2(temp, 'electr_D_L14.dat.csv')

 # таблицы для тулупова по сравнению корпусов 5  -----------------------------
 
 setwd("d:/Simulink/report/")
 datae<-read.table('electrons_D.dat', header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 temp<-datae %>%
   group_by( ., E0.keV >150, dE.det2 > 0, dE.det3 > 0, CF ==TRUE) %>%
   summarise( ., number=n())
 write.csv2(temp, 'electrons_D.dat.csv')
 
 

 datae<-read.table('electrons_D.dat', header = TRUE)
 
 # таблицы для тулупова по сравнению корпусов 6  -----------------------------
 pdf(paste("elecprotdde2.pdf", sep="" ), width =20, height=20)
 n = 20
 colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 0.5))
 
 setwd("d:/Simulink/report/")
 filename<-'electrons_B.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$dE.det2), breaks.dEdet2)
 
 
 hist.n <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det2))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 setwd("d:/Simulink/report/")
 filename<-'electrons_D.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$dE.det2), breaks.dEdet2)
 
 
 hist.n <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det2))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 setwd("d:/Simulink/report/")
 filename<-'protons_B.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$dE.det2), breaks.dEdet2)
 
 
 hist.n <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             # zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                       col.regions = colr,
                                       colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det2))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             # zlim = c(0,100000),
              col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                       col.regions = colr,
                                       colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 setwd("d:/Simulink/report/")
 filename<-'protons_D.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$dE.det2), breaks.dEdet2)
 
 
 hist.n <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det2))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 dev.off()
 # таблицы для тулупова по сравнению корпусов 7  -----------------------------
 pdf(paste("elecprotdde3.pdf", sep="" ), width =20, height=20)
 n = 20
 colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 0.5))
 
 setwd("d:/Simulink/report/")
 filename<-'electrons_B.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$dE.det3), breaks.dEdet3)
 
 
 hist.n <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det3))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 setwd("d:/Simulink/report/")
 filename<-'electrons_D.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$dE.det3), breaks.dEdet3)
 
 
 hist.n <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det3))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 setwd("d:/Simulink/report/")
 filename<-'protons_B.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$dE.det3), breaks.dEdet3)
 
 
 hist.n <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det3))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 setwd("d:/Simulink/report/")
 filename<-'protons_D.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус','electrons_D.dat', 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 5000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$dE.det3), breaks.dEdet3)
 
 
 hist.n <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n )  
 write.csv2(hist.n, paste0("NdEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             #zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(range(hist.n$n), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.n$n), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det3))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             #zlim = c(0,100000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(range(hist.sum$sum), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(range(hist.sum$sum), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 dev.off()
# plot angle --------------------------------------------------------------
 
 setwd("d:/Simulink/report/")
 filename <- 'electrons_A.dat'
 datae<-read.table(filename, header = TRUE)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 
 
 
 
 plot(data = datae, x = datae$ang_z, y = datae$dE.det2/datae$dE.det3, log = "y")
 
 densityplot( x = datae[ datae$dE.det3>0,]$ang_z, log = "y")
 densityplot( x = datae[ datae$dE.det2>0,]$ang_z, log = "y")
 
 
 plot(data = datae, x = datae$ang_z, y = datae$dE.det1)
 plot(data = datae, x = datae$ang_z, y = datae$dE.det2, log = "y")
 plot(data = datae, x = datae$ang_z, y = datae$dE.det3, log = "y")
 plot(data = datae, x = datae$ang_z, y = datae$dE.det4, log = "y")
 
 plot(data = datae, x = datae$ang_z, y = datae$dE.det3/datae$dE.det2, log = "y")
 
 
 
 
 hexbinplot(dE.det2~dE.det3, data=datae, colramp=rf, log = 'x',
            aspect=1, trans=log, inv=exp,xbins = 100,
            mincnt=1)
 
 hexbinplot(dE.det1~dE.det2, data=datae, colramp=rf, log = 'x',
            aspect=1, trans=log, inv=exp,
            xbins = 100,
            mincnt=1)
 
 
 pdf(paste("elec1.pdf", sep=""))
 filename <- 'electrons_A.dat'
 datae<-read.table(filename, header = TRUE)
 hexbinplot(dE.det3~ang_z, data=datae[datae$dE.det3>0,], colramp=rf, log = 'x',
            aspect=1, trans=log, inv=exp,mincnt=1, maxcnt =100,
            # xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename,nrow(datae), sep=" "))
 hexbinplot(dE.det3~E0.keV, data=datae[datae$dE.det3>0,], colramp=rf,
            aspect=1, trans=log, inv=exp, mincnt=1, maxcnt =100,
            xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename, nrow(datae), sep=" "))
 filename <- 'electrons_B.dat'
 datae<-read.table(filename, header = TRUE)
 hexbinplot(dE.det3~ang_z, data=datae[datae$dE.det3>0,], colramp=rf, log = 'x',
            aspect=1, trans=log, inv=exp,mincnt=1, maxcnt =100,
            # xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename,nrow(datae), sep=" "))
 hexbinplot(dE.det3~E0.keV, data=datae[datae$dE.det3>0,], colramp=rf,
            aspect=1, trans=log, inv=exp, mincnt=1, maxcnt =100,
            xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename, nrow(datae), sep=" "))
 filename <- 'electrons_C.dat'
 datae<-read.table(filename, header = TRUE)
 hexbinplot(dE.det3~ang_z, data=datae[datae$dE.det3>0,], colramp=rf, log = 'x',
            aspect=1, trans=log, inv=exp,mincnt=1, maxcnt =100,
            # xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename,nrow(datae), sep=" "))
 hexbinplot(dE.det3~E0.keV, data=datae[datae$dE.det3>0,], colramp=rf,
            aspect=1, trans=log, inv=exp, mincnt=1, maxcnt =100,
            xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename, nrow(datae), sep=" "))
 filename <- 'electrons_D.dat'
 datae<-read.table(filename, header = TRUE)
 hexbinplot(dE.det3~ang_z, data=datae[datae$dE.det3>0,], colramp=rf, log = 'x',
            aspect=1, trans=log, inv=exp,mincnt=1, maxcnt =100,
            # xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename,nrow(datae), sep=" "))
 hexbinplot(dE.det3~E0.keV, data=datae[datae$dE.det3>0,], colramp=rf,
            aspect=1, trans=log, inv=exp, mincnt=1, maxcnt =100,
            xbins = 100,
            main = paste("Electrons dE.det3 > 0,",filename, nrow(datae), sep=" "))
 dev.off()
 
 
 
# plot angle 3d -----------------------------------------------------------
 
 
 
 
 require(lattice)
 require(latticeExtra)
 # data(VADeaths)
 
 
 # histograms d3
 
 pdf(paste("elec3d.pdf", sep="" ), width =10, height=10)
 filename <- 'electrons_D.dat'
 datae<-read.table(filename, header = TRUE)
 
 
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$dE.det3), breaks.dEdet3)
 
 
 hist.n <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n)
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det3>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det3))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("S", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 dev.off()
 
 
 
 
 
 # histograms d2
 pdf(paste("elec3d2.pdf", sep="" ), width =10, height=10)
 filename <- 'electrons_D.dat'
 datae<-read.table(filename, header = TRUE)
 
 
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$ang_z), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$dE.det2), breaks.dEdet2)
 
 
 hist.n <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n)
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             zlim = c(0,300),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 300), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 300), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$dE.det2>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(dE.det2))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum)
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             zlim = c(0,30000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 30000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 30000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 dev.off()
 
 
 # plot angle  report 3 det--------------------------------------------------------------
 # here I can 
 setwd("d:/Simulink/report3/")
 
 
 pdf(paste("elec3dde3.pdf", sep="" ), width =20, height=20)
 
 # Brass_el_1e8_L32 --------------------------------------------------------
 filename <- 'Brass_el_1e8_L32.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$Det3.keV), breaks.dEdet3)
 
 
 hist.n <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
 print(hist.n)
 write.csv2(hist.n, paste0("NdEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det3.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum)
 write.csv2(hist.sum, paste0("SEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 # Dural_el_1e8_L32 --------------------------------------------------------
 
 filename <- 'Dural_el_1e8_L32.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$Det3.keV), breaks.dEdet3)
 
 
 hist.n <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n) 
  write.csv2(hist.n, paste0("NEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det3.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("SEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 # Brass_el_1e8_L32_trigg --------------------------------------------------------
 
 filename <- 'Brass_el_1e8_L32_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$Det3.keV), breaks.dEdet3)
 
 
 hist.n <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n)  
  write.csv2(hist.n, paste0("NEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det3.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum ) 
 write.csv2(hist.sum, paste0("SEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 # Dural_el_1e8_L32_trigg --------------------------------------------------------
 
 filename <- 'Dural_el_1e8_L32_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet3 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE3 <- cut(as.numeric(datae$Det3.keV), breaks.dEdet3)
 
 
 hist.n <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n )   
  write.csv2(hist.n, paste0("NEdet3", filename))
 
 print(cloud(n ~ bins.angle * bins.dE3, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det3.keV>0,] %>%
   group_by(., bins.angle, bins.dE3) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det3.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )   
 write.csv2(hist.sum, paste0("SEdet3", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE3, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det3',zlab = 'summ dE.det3',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 dev.off()
 
 # plot angle  report 2 det--------------------------------------------------------------
 # here I can 
 setwd("d:/Simulink/report3/")
 
 
 pdf(paste("elec3ddet2.pdf", sep="" ), width =20, height=20)
 
 # Brass_el_1e8_L32 --------------------------------------------------------
 filename <- 'Brass_el_1e8_L32.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$Det2.keV), breaks.dEdet2)
 
 
 hist.n <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n )  
  write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det2.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum )  
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 # Dural_el_1e8_L32 --------------------------------------------------------
 
 filename <- 'Dural_el_1e8_L32.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$Det2.keV), breaks.dEdet2)
 
 
 hist.n <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n)
  write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det2.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 # Brass_el_1e8_L32_trigg --------------------------------------------------------
 
 filename <- 'Brass_el_1e8_L32_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$Det2.keV), breaks.dEdet2)
 
 
 hist.n <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n) 
  write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det2.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 # Dural_el_1e8_L32_trigg --------------------------------------------------------
 
 filename <- 'Dural_el_1e8_L32_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet2 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE2 <- cut(as.numeric(datae$Det2.keV), breaks.dEdet2)
 
 
 hist.n <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n) 
  write.csv2(hist.n, paste0("NdEdet2", filename))
 
 print(cloud(n ~ bins.angle * bins.dE2, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det2.keV>0,] %>%
   group_by(., bins.angle, bins.dE2) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det2.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("SdEdet2", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE2, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det2',zlab = 'summ dE.det2',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 dev.off()
 # plot angle  report 4 det--------------------------------------------------------------
 # here I can 
 setwd("d:/Simulink/report3/")
 
 
 pdf(paste("elec3ddet4.pdf", sep="" ), width =20, height=20)
 
 # Brass_el_1e8_L32 --------------------------------------------------------
 filename <- 'Brass_el_1e8_L32.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet4 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE4 <- cut(as.numeric(datae$Det4.keV), breaks.dEdet4)
 
 
 hist.n <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n) 
  write.csv2(hist.n, paste0("NdEdet4", filename))
 
 print(cloud(n ~ bins.angle * bins.dE4, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 
 hist.sum <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det4.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum)  
 write.csv2(hist.sum, paste0("SdEdet4", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE4, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',zlab = 'summ dE.det4',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 # Dural_el_1e8_L32 --------------------------------------------------------
 
 filename <- 'Dural_el_1e8_L32.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet4 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE4 <- cut(as.numeric(datae$Det4.keV), breaks.dEdet4)
 
 
 hist.n <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n)
  write.csv2(hist.n, paste0("NdEdet4", filename))
 
 print(cloud(n ~ bins.angle * bins.dE4, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det4.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum)
 write.csv2(hist.sum, paste0("SdEdet4", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE4, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',zlab = 'summ dE.det4',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 # Brass_el_1e8_L32_trigg --------------------------------------------------------
 
 filename <- 'Brass_el_1e8_L32_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet4 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE4 <- cut(as.numeric(datae$Det4.keV), breaks.dEdet4)
 
 
 hist.n <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n) 
  write.csv2(hist.n, paste0("NdEdet4", filename))
 
 print(cloud(n ~ bins.angle * bins.dE4, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det4.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("SdEdet4", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE4, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',zlab = 'summ dE.det4',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 # Dural_el_1e8_L32_trigg --------------------------------------------------------
 
 filename <- 'Dural_el_1e8_L32_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 breaks.angle <- seq(0, 180, length.out = 180/5+1)
 breaks.dEdet4 <- seq(0, 3000, length.out = 30+1)
 
 datae$bins.angle <- cut(as.numeric(datae$angz), breaks.angle)
 datae$bins.dE4 <- cut(as.numeric(datae$Det4.keV), breaks.dEdet4)
 
 
 hist.n <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., n =n())
 hist.n <- na.omit(hist.n)
  print(hist.n) 
  write.csv2(hist.n, paste0("NdEdet4", filename))
 
 print(cloud(n ~ bins.angle * bins.dE4, data = hist.n, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',main = filename,
             zlim = c(0,60),
             col.facet = level.colors(hist.n$n, at = do.breaks(c(0, 60), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 60), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 
 
 
 hist.sum <- datae[datae$Det4.keV>0,] %>%
   group_by(., bins.angle, bins.dE4) %>%
   summarise(., sum =sum(na.omit(as.numeric(Det4.keV))))
 hist.sum <- na.omit(hist.sum)
 print(hist.sum) 
 write.csv2(hist.sum, paste0("SdEdet4", filename))
 
 print(cloud(sum ~ bins.angle * bins.dE4, data = hist.sum, panel.3d.cloud=panel.3dbars,
             # col.facet='grey',
             xbase=0.4, ybase=0.4,
             scales=list(arrows=FALSE, col=1), 
             xlab = 'angle', ylab = 'dE.det4',zlab = 'summ dE.det4',main = filename,
             zlim = c(0,3000),
             col.facet = level.colors(hist.sum$sum, at = do.breaks(c(0, 3000), 20),
                                      col.regions = colr,
                                      colors = TRUE),
             colorkey = list(col = colr, at = do.breaks(c(0, 3000), 20)),
             screen = list(z = 40, x = -30),
             par.settings = list(axis.line = list(col = "transparent")))
 )
 dev.off()
 
 # gamma Brass_gamma_1e7_1MeV --------------------------------------------------------
 setwd("d:/Simulink/report3/")
 filename <- 'Brass_gamma_1e7_1MeV.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 cat('\nКорпус ', filename, 'Общее число частиц:', nrow(datae) )
 summary(datae)
 hist(datae$Det1.keV, breaks = 100, col = "lightblue", border = "pink")
 hist(datae$Det2.keV, breaks = 100, col = "lightblue", border = "pink")
 hist(datae$Det3.keV, breaks = 100, col = "lightblue", border = "pink")
 hist(datae$Det4.keV, breaks = 100, col = "lightblue", border = "pink")
 
 
 
 pdf(paste("gammade3.pdf", sep="" ), width =10, height=10)
 
 filename <- 'Brass_gamma_1e7_1MeV.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 hist(datae[datae$Det2.keV>120,]$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 
 filename <- 'Brass_gamma_1e7_1MeV_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 filename <- 'Brass_gamma_1e7_8MeV.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 hist(datae[datae$Det2.keV>120,]$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 filename <- 'Brass_gamma_1e7_8MeV_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 filename <- 'Dural_gamma_1e7_1MeV.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 filename <- 'Dural_gamma_1e7_1MeV_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 filename <- 'Dural_gamma_1e7_8MeV.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 
 filename <- 'Dural_gamma_1e7_8MeV_trigg.csv'
 datae<-read.csv(filename, header = TRUE, skip = 11)
 hist(datae$Det3.keV, main = filename, breaks = 100, col = "lightblue", border = "pink")
 

 dev.off()
# parcoord ----------------------------------------------------------------
library(MASS)
# library(colorRamps)



# data(mtcars)
# k <- blue2red(100)
n = 6
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 0.2))
# k <- adjustcolor(brewer.pal(3, "Set1")[titdf2$Survived], alpha=.2)
dataedf <- as.data.frame(lapply(as.data.frame(datae), as.numeric))
# new columns with jittered values
dataedf[,9:10] <- lapply((dataedf[,c(6,8)]), jitter)
# x <- cut( datae$E0.keV, 100)

ppi <- 300
png(paste("parallelelec1",casing,".png", sep=""), width=6*ppi, height=6*ppi, res=ppi)


op <- par(mar=c(3, rep(.1, 3)))
parcoord(dataedf[,c(1,2,9,3,10,4,7)], col=colr[as.numeric(dataedf$bins)],
         main = paste("поток электронов через прибор, \n , корпус",casing, sep=" ")
)
# par(op)
dev.off()

# alluvial ----------------------------------------------------------------
install.packages(file.choose(), repos=NULL)
library(alluvial)


#Titanic data
tit <- as.data.frame(Titanic)

# 2d
tit2d <- aggregate( Freq ~ Class + Survived, data=tit, sum)
alluvial( tit2d[,1:2], freq=tit2d$Freq, xw=0.0, alpha=0.8,
          gap.width=0.1, col= "steelblue", border="white",
          layer = tit2d$Survived != "Yes" )



casing <-'A'
datae<-read.table(paste("electrons_",casing,".dat", sep=''), header = TRUE)
datae$bins <- cut(datae$E0.keV, breaks, labels= labels)

datae <- datae%>%
  group_by( bins, CF, dE.det2>120, dE.det3>800)%>%
  summarise( freq = n())
datae <- na.omit(datae)


alluvial(datae, freq = datae$freq,
         # col=ifelse( by.bins$CF == 0, "red", "green"),
         col=colr[as.numeric(datae$bins)]
         )

# бок-коллиматор для 2-3 детекторов ---------------------------------------

# Очень нужна информация об отношении бок - коллиматор зарегистрированных электронов 
# в D2 и D3 во всех  электронных каналах для трех вариантов корпусов:
# латунь, дюраль, дюраль с внутренней вставкой из вольфрама 
# со стробом в D2 и без него..

#  кросс тест с протнами ---------------------------------------------------------------------
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

