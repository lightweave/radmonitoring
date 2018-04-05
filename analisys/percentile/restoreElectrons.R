#source('comparators.R')
setwd("D:/Simulink")
library("Hmisc")
# datae<-read.table("data/det2/electron_A_L17_det2.dat", header = TRUE)
# datae<-read.table("data/det2/electr_A_L14_det2.dat", header = TRUE)



datae<-read.table("data/alldet/electr_A_L32.dat", header = TRUE)

# кросс тест с протонами
# datae<-read.table("data/alldet/proton_A_L32.dat", header = TRUE)
#summary(datae)


# sum(datae$CF > 0 & (datae$E0.keV > leftE0[1]) & (datae$E0.keV < rightE0[1]), na.rm=TRUE)


breaks <- c(0,150,350,600,1000,2000,4000,10000,Inf)
datae$bins <- cut(datae$E0.keV, breaks, labels=c('c0','c1','c2','c3','c4','c5','c6','c7'))
# initial <- cut(datae$E0.keV, breaks)
# table(initial)


el <- vector();

ch <- vector();


el14 <- vector();
el17 <- vector();
el32 <- vector();
# исходные спектры при моделировании Geant-----------------------------
el14 <- c(47135060,
          11579606,
          1864838,
          574134,
          101458,
          77)
el17 <- c(43517330,
          10981663,
          1903275,
          199186,
          14846,
          12)
el32 <- c(36161695,
          10351798,
          5461403,
          3738306,
          1108249,
          43189)
# ошибки подсчитаны как (1-селективность)/чуствительность-----------------------
errr <- vector();
errr <- c(1,
          1.6,
          3.2,
          4,
          0.315789474,
          0.8)

# отбор каналов по 5-95 --------------------------------------------------------
  
electron<-datae[((datae$CF > 0) & (datae$bins == "с5")), ]
channel<-datae[((datae$dE.det1 >= e5[1,1]) & datae$dE.det1 <= e5[6,1])& 
                 ((datae$dE.det2 >= e5[1,2]) & datae$dE.det2 <= e5[6,2])& 
                 ((datae$dE.det3 >= e5[1,3]) & datae$dE.det3 <= e5[6,3])& 
                 ((datae$dE.det4 >= e5[1,4]) & datae$dE.det4 <= e5[6,4]),]
el[5] <- nrow(electron)
ch[5] <- nrow(channel)

electron<-datae[((datae$CF > 0) & (datae$bins == "с6")), ]
channel<-datae[((datae$dE.det1 >= e6[1,1]) & datae$dE.det1 <= e6[6,1])& 
                 ((datae$dE.det2 >= e6[1,2]) & datae$dE.det2 <= e6[6,2])& 
                 ((datae$dE.det3 >= e6[1,3]) & datae$dE.det3 <= e6[6,3])& 
                 ((datae$dE.det4 >= e6[1,4]) & datae$dE.det4 <= e6[6,4]),]
el[6] <- nrow(electron)
ch[6] <- nrow(channel)

# отбор каналов по 25-75 -------------------------------------------------------
electron<-datae[((datae$CF > 0) & (datae$bins == "с1")), ]
channel<-datae[  ((datae$dE.det1 >= e1[3,1]) & datae$dE.det1 <= e1[4,1])& 
                 ((datae$dE.det2 >= e1[3,2]) & datae$dE.det2 <= e1[4,2])& 
                 ((datae$dE.det3 >= e1[3,3]) & datae$dE.det3 <= e1[4,3])& 
                 ((datae$dE.det4 >= e1[3,4]) & datae$dE.det4 <= e1[4,4]),]
el[1] <- nrow(electron)
ch[1] <- nrow(channel)
electron<-datae[((datae$CF > 0) & (datae$bins == "с2")), ]
channel<-datae[((datae$dE.det1 >= e2[3,1]) & datae$dE.det1 <= e2[4,1])& 
               ((datae$dE.det2 >= e2[3,2]) & datae$dE.det2 <= e2[4,2])& 
               ((datae$dE.det3 >= e2[3,3]) & datae$dE.det3 <= e2[4,3])& 
               ((datae$dE.det4 >= e2[3,4]) & datae$dE.det4 <= e2[4,4]),]
el[2] <- nrow(electron)
ch[2] <- nrow(channel)
electron<-datae[((datae$CF > 0) & (datae$bins == "с3")), ]
channel<-datae[  ((datae$dE.det1 >= e3[3,1]) & datae$dE.det1 <= e3[4,1])& 
                 ((datae$dE.det2 >= e3[3,2]) & datae$dE.det2 <= e3[4,2])& 
                 ((datae$dE.det3 >= e3[3,3]) & datae$dE.det3 <= e3[4,3])& 
                 ((datae$dE.det4 >= e3[3,4]) & datae$dE.det4 <= e3[4,4]),]
el[3] <- nrow(electron)
ch[3] <- nrow(channel)
electron<-datae[((datae$CF > 0) & (datae$bins == "с4")), ]
channel<-datae[((datae$dE.det1 >= e4[3,1]) & datae$dE.det1 <= e4[4,1])& 
                 ((datae$dE.det2 >= e4[3,2]) & datae$dE.det2 <= e4[4,2])& 
                 ((datae$dE.det3 >= e4[3,3]) & datae$dE.det3 <= e4[4,3])& 
                 ((datae$dE.det4 >= e4[3,4]) & datae$dE.det4 <= e4[4,4]),]
el[4] <- nrow(electron)
ch[4] <- nrow(channel)

# effectiveness -----------------------------------------------------------

x = data.frame( names=c('e1','e2','e3','e4','e5','e6'),el32, ch)

# необходимо раскомментировать для получения градуировки первый раз

# eff = x$ch/x$el14;

x$ch = ceil(x$ch/eff)

mymat <- t(x[-1])
colnames(mymat) <- x[, 1]
# barplot(mymat, beside = TRUE, legend.text=c('пад.','рег.'))
mymat<-mymat[,1:5]# выбор каналов для графика
barx <-barplot(mymat,                 
               ylim=c(0.1, 2*max(na.exclude(x$ch))+max(na.exclude(x$el32))+1),
               log='y',
               legend.text=c('пад.','рег.'),
               beside = TRUE,
               xpd=FALSE,
               main='Восстановление спектра L=3.2',
               ylab='N, частиц',
               xlab='Номер канала')
box()

#standard error
er<- x$ch*errr + x$ch/sqrt(ch)
er<- er[-6]
#error bars
arrows(barx[2,],mymat[2,]+er, barx[2,], mymat[2,], angle=90, code=1)
arrows(barx[2,],mymat[2,]-er, barx[2,], mymat[2,], angle=90, code=1)

#legend (after making the plot, indicate where the legend has 
#to come with the mouse)
# legend(locator(1),c(levels(z)),fill=c(1:w),bty="n",cex=0.8)
