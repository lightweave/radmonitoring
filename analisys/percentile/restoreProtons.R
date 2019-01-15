#source('comparators.R')
setwd("D:/Simulink")
library("Hmisc", lib.loc="C:/Program Files/R/R-3.1.2/library")

# тренировка на L14 -------------------------------------------------------
datap<-read.table("data/det2/proton_A_L14_det2.dat", header = TRUE)
# datap<-read.table("data/det2/proton_A_L17_det2.dat", header = TRUE)

# выбор восстановление разных спектров ------------------------------------------


# datap<-read.table("data/alldet/proton_A_L14.dat", header = TRUE)
# datap<-read.table("data/alldet/proton_A_L17.dat", header = TRUE)
# datap<-read.table("data/alldet/proton_A_L32.dat", header = TRUE)


# кросс тест с электронами
# datap<-read.table("data/det2/electr_A_L32_det2.dat", header = TRUE)
#summary(datap)

#filter zero and high data?


# sum(datap$CF > 0 & (datap$E0.keV > leftE0[1]) & (datap$E0.keV < rightE0[1]), na.rm=TRUE)


breaks <- c(0,2000,4000,9000,15000,30000,53000,100000,160000, Inf)
datap$bins <- cut(datap$E0.keV, breaks, labels=c('c0','c1','c2','c3','c4','c5','c6','c7','c8'))
# initial <- cut(datap$E0.keV, breaks)
# table(initial)


# initial spectrum --------------------------------------------------------


pr <- vector();

ch <- vector();


pr14 <- vector();
pr17 <- vector();
pr32 <- vector();

pr14 <- c(12554154.51,
          24209263.53,
          16530605.37,
          13781462.77,
          7892377.961,
          8285838.076,
          5616662.113,
          6259257.997 )
pr17 <- c(37582161.48,
          34101229.37,
          9438270.918,
          2879011.92,
          336511.7328,
          132327.8831,
          51262.6717,
          36986.07984)
pr32 <- c(47540815.31,
          9296707.318,
          854260.5702,
          189565.6698,
          13628.3297,
          2126.082993,
          95.34004452,
          0)

errr <- vector();
errr <- c(0.027777778,
          0.171428571,
          0.038461538,
          0.041666667,         
          0.80952381,
          0.368421053,
          0.6,
          0.038461538)

# отбор каналов по 5-95 ---------------------------------------------------
# 
# proton<-datap[((datap$CF > 0) & (datap$bins == "c1")), ]
# channel<-datap[((datap$dE.det1>120)&datap$dE.det1<1489)& 
#             ((datap$dE.det2>532)&datap$dE.det2<3020)& 
#             ((datap$dE.det3>-1)&datap$dE.det3<800)& 
#             ((datap$dE.det4>-1)&datap$dE.det4<200),]
# pr[1] <- nrow(proton)
# ch[1] <- nrow(channel)/ 0.75
  
proton<-   datap[((datap$CF > 0) & (datap$bins == "c2")), ]
channel<-  datap[((datap$dE.det1>120)&datap$dE.det1<706)& 
                 ((datap$dE.det2>3251)&datap$dE.det2<8019)& 
                 ((datap$dE.det3>-1)&datap$dE.det3<800)& 
                 ((datap$dE.det4>-1)&datap$dE.det4<200),]
pr[2] <- nrow(proton)
ch[2] <- nrow(channel)



proton<-datap[((datap$CF > 0) & (datap$bins == "c3")), ]
channel<-datap[((datap$dE.det1>120)&datap$dE.det1<387)& 
                 ((datap$dE.det2>3218)&datap$dE.det2<7076)& 
                 ((datap$dE.det3>955)&datap$dE.det3<10723)& 
                 ((datap$dE.det4>-1)&datap$dE.det4<200),]

pr[3] <- nrow(proton)
ch[3] <- nrow(channel)


proton<-datap[((datap$CF > 0) & (datap$bins == "c4")), ]
channel<-datap[((datap$dE.det1>120)&datap$dE.det1<257)& 
                 ((datap$dE.det2>1416)&datap$dE.det2<3696)& 
                 ((datap$dE.det3>6157)&datap$dE.det3<26549)& 
                 ((datap$dE.det4>-1)&datap$dE.det4<200),]
pr[4] <- nrow(proton)
ch[4] <- nrow(channel)
# proton<-datap[((datap$CF > 0) & (datap$bins == "c5")), ]
# channel<-datap[((datap$dE.det1>120)&datap$dE.det1<165)& 
#                  ((datap$dE.det2>186)&datap$dE.det2<2443)& 
#                  ((datap$dE.det3>5055)&datap$dE.det3<49295)& 
#                  ((datap$dE.det4>-1)&datap$dE.det4<200),]
# pr[5] <- nrow(proton)
# ch[5] <- nrow(channel)
# proton<-datap[((datap$CF > 0) & (datap$bins == "c6")), ]
# channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<173)& 
#                ((datap$dE.det2>150)&datap$dE.det2<2459)&
#                ((datap$dE.det3>800)&datap$dE.det3<46385)& 
#                ((datap$dE.det4>200)&datap$dE.det4<6731),]
# pr[6] <- nrow(proton)
# ch[6] <- nrow(channel)
# proton<-datap[((datap$CF > 0) & (datap$bins == "c7")), ]
# channel<-datap[((datap$dE.det1>1)&datap$dE.det1<93)& 
#                  ((datap$dE.det2>150)&datap$dE.det2<1168)& 
#                  ((datap$dE.det3>800)&datap$dE.det3<25635)& 
#                  ((datap$dE.det4>200)&datap$dE.det4<1912),]
# pr[7] <- nrow(proton)
# ch[7] <- nrow(channel)
# proton<-datap[((datap$CF > 0) & (datap$bins == "c8")), ]
# channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<52)& 
#                  ((datap$dE.det2>75)&datap$dE.det2<718)& 
#                  ((datap$dE.det3>800)&datap$dE.det3<15688)& 
#                  ((datap$dE.det4>200)&datap$dE.det4<1236),]
# pr[8] <- nrow(proton)
# ch[8] <- nrow(channel)



# 

# отбор каналов по 25-75 --------------------------------------------------


proton<-datap[((datap$CF > 0) & (datap$bins == "c1")), ]
channel<-datap[((datap$dE.det1>753)&datap$dE.det1<1128)& 
            ((datap$dE.det2>1214)&datap$dE.det2<2563)& 
            ((datap$dE.det3>-1)&datap$dE.det3<800)& 
            ((datap$dE.det4>-1)&datap$dE.det4<200),]
pr[1] <- nrow(proton)
ch[1] <- nrow(channel)

# proton<-datap[((datap$CF > 0) & (datap$bins == "c2")), ]
# channel<-datap[((datap$dE.det1>381)&datap$dE.det1<571)& 
#                  ((datap$dE.det2>4363)&datap$dE.det2<6963),]

# proton<-datap[((datap$CF > 0) & (datap$bins == "c3")), ]
# channel<-datap[((datap$dE.det1>231)&datap$dE.det1<332)& 
#                  ((datap$dE.det2>3912)&datap$dE.det2<5511)& 
#                  ((datap$dE.det3>4171)&datap$dE.det3<8686),]

# proton<-datap[((datap$CF > 0) & (datap$bins == "c4")), ]
# channel<-datap[((datap$dE.det1>116)&datap$dE.det1<215)& 
#                  ((datap$dE.det2>2144)&datap$dE.det2<3012)& 
#                  ((datap$dE.det3>13447)&datap$dE.det3<21379),]

proton<-datap[((datap$CF > 0) & (datap$bins == "c5")), ]
channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<123)& 
                 ((datap$dE.det2>1287)&datap$dE.det2<1692)& 
                 ((datap$dE.det3>30440)&datap$dE.det3<42433),]
pr[5] <- nrow(proton)
ch[5] <- nrow(channel)

# proton<-datap[((datap$CF > 0) & (datap$bins == "c6")), ]
# channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<120)& 
#                  ((datap$dE.det2>770)&datap$dE.det2<1079)& 
#                  ((datap$dE.det3>22761)&datap$dE.det3<35141)& 
#                  ((datap$dE.det4>200)&datap$dE.det4<3455),]
# 
# pr[6] <- nrow(proton)
# ch[6] <- 0.47*nrow(channel)
proton<-datap[((datap$CF > 0) & (datap$bins == "c7")), ]
channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<54)& 
                 ((datap$dE.det2>545)&datap$dE.det2<927)& 
                 ((datap$dE.det3>16030)&datap$dE.det3<22122)& 
                 ((datap$dE.det4>1022)&datap$dE.det4<1629),]

pr[7] <- nrow(proton)
ch[7] <- nrow(channel)

proton<-datap[((datap$CF > 0) & (datap$bins == "c8")), ]
channel<-datap[ 
                 ((datap$dE.det1>-1)&datap$dE.det1<29)& 
                 ((datap$dE.det2>291)&datap$dE.det2<518)& 
                 ((datap$dE.det3>8781)&datap$dE.det3<13487)& 
                 ((datap$dE.det4>452)&datap$dE.det4<980),]
pr[8] <- nrow(proton)
ch[8] <- nrow(channel)

# отбор каналов по по классике --------------------------------------------


# channel[2]<-nrow(datap[((datap$dE.det1>300)&datap$dE.det1<800)& 
#             ((datap$dE.det2>3000)&datap$dE.det2<8000)&
#             ((datap$dE.det3>-1)&datap$dE.det3<3000),])
# 
# channel[3]<-nrow(datap[((datap$dE.det1>250)&datap$dE.det1<420)& 
#             ((datap$dE.det2>4000)&datap$dE.det2<8000)&
#             ((datap$dE.det3>2000)&datap$dE.det3<10000),])
# 
# channel[4]<-nrow(datap[((datap$dE.det1>0)&datap$dE.det1<300)& 
#                  ((datap$dE.det2>2500)&datap$dE.det2<4000)&
#                  ((datap$dE.det3>12000)&datap$dE.det3<30000),])
#
# 
# proton<-datap[((datap$CF > 0) & (datap$bins == "c5")), ]
# channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<200)& 
#                  ((datap$dE.det2>1000)&datap$dE.det2<2000)& 
#                  ((datap$dE.det3>30000)&datap$dE.det3<50000),]
# pr[5] <- nrow(proton)
# ch[5] <- nrow(channel)

# channel[5]<-nrow(datap[((datap$dE.det1>0)&datap$dE.det1<200)& 
#                  ((datap$dE.det2>1000)&datap$dE.det2<2000)&
#                  ((datap$dE.det3>30000)&datap$dE.det3<50000),])

# 
proton<-datap[((datap$CF > 0) & (datap$bins == "c6")), ]
channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<200)& 
                 ((datap$dE.det2>-1)&datap$dE.det2<2000)& 
                 ((datap$dE.det3>20000)&datap$dE.det3<50000)& 
                 ((datap$dE.det4>2000)&datap$dE.det4<15000),]
pr[6] <- nrow(proton)
ch[6] <- nrow(channel)

# channel[6]<-nrow(datap[((datap$dE.det1>0)&datap$dE.det1<200)& 
#                  ((datap$dE.det2>000)&datap$dE.det2<2000)&
#                  ((datap$dE.det3>20000)&datap$dE.det3<50000)&
#                  ((datap$dE.det4>2000)&datap$dE.det4<15000),])

# 
# proton<-datap[((datap$CF > 0) & (datap$bins == "c7")), ]
# channel<-datap[((datap$dE.det1>-1)&datap$dE.det1<200)& 
#                  ((datap$dE.det2>-1)&datap$dE.det2<2000)& 
#                  ((datap$dE.det3>20000)&datap$dE.det3<50000)& 
#                  ((datap$dE.det4>2000)&datap$dE.det4<5000),]

# channel[7]<-nrow(datap[((datap$dE.det1>0)&datap$dE.det1<200)& 
#                  ((datap$dE.det2>000)&datap$dE.det2<2000)&
#                  ((datap$dE.det3>20000)&datap$dE.det3<50000)&
#                  ((datap$dE.det4>2000)&datap$dE.det4<5000),])
# 
# channel[8]<-nrow(datap[((datap$dE.det1>120)&datap$dE.det1<200)& 
#                  ((datap$dE.det2>000)&datap$dE.det2<1000)&
#                  ((datap$dE.det3>15000)&datap$dE.det3<30000)&
#                  ((datap$dE.det4>1000)&datap$dE.det4<2500),]) 
# 
# channel[9]<-nrow(datap[((datap$dE.det1>500)&datap$dE.det1<1500)& 
#             ((datap$dE.det2>200)&datap$dE.det2<4500),])








# effectiveness switch ----------------------------------------------------

# training switch
# comment uncomment!
x = data.frame( names=c('p1','p2','p3','p4','p5','p6','p7','p8'),pr14, ch)
eff = x$ch/x$pr14;


x = data.frame( names=c('p1','p2','p3','p4','p5','p6','p7','p8'),pr17, ch)




x$ch = ceil(x$ch/eff)

mymat <- t(x[-1])
colnames(mymat) <- x[, 1]
# barplot(mymat, beside = TRUE, legend.text=c('пад.','рег.'))
mymat<-mymat[,1:6]
barx <-barplot(mymat,                 
               ylim=c(0.1, max(x$ch)+max(x$pr32)),
               log='y',
               legend.text=c('пад.','рег.'),
               beside = TRUE,
               xpd=FALSE,
               main='¬осстановление спектра L=3.2',
               ylab='N, частиц',
               xlab='Ќомер канала')
box()

#standard error
er<- x$ch*errr + x$ch/sqrt(ch)

#error bars
arrows(barx[2,],mymat[2,]+er, barx[2,], mymat[2,], angle=90, code=1)
arrows(barx[2,],mymat[2,]-er, barx[2,], mymat[2,], angle=90, code=1)

#legend (after making the plot, indicate where the legend has 
#to come with the mouse)
# legend(locator(1),c(levels(z)),fill=c(1:w),bty="n",cex=0.8)


# barplot(x, main="Car Distribution by Gears and VS",
#                xlab="Number of Gears", col=c("darkblue","red"),
#                legend = rownames(names), beside=TRUE)
# barplot ( ch, space=1, width=0.5, names.arg=c('c1','c2','c3','c4','c5','c6','c7','c8'), col='red')
# barplot ( pr, col='blue',space=1,width=0.5,offset = 0.5,  add = TRUE, beside= TRUE)


# print (nrow(channel)/nrow(proton))
# print("primary proton")
# print (nrow(proton))
# print(summary(proton))
# print("conditional channel")
# print (nrow(channel))
# print(summary(channel))
# channel$bins <- cut(channel$E0.keV, breaks, labels=c('c0','c1','c2','c3','c4','c5','c6','c7','c8'))
# channel_target<-channel[((channel$CF > -1) & (channel$bins == "c7")), ]
# print (nrow(channel_target)/nrow(channel))#уровень сепарации