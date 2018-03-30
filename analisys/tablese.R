#!/usr/bin/Rscript
# eval(parse(filename, encoding="UTF-8"))

# library(data.table)
# library("Hmisc")
# library('hexbin')
# library('tidyr')
library(readr)
library(dplyr)
library(ggplot2)


setwd("D:/Ivan/_flash backup 2014/SINP/2017 Группировка/radmonitoring/analisys")




# Start the clock!
ptm <- proc.time()

# таблицы для Чирской фацет по сравнению корпусов  -----------------------------
# tables facet - casing and telescope development  -----------------------------

# data preparing ---------------------------------------------------------------

filename <- 'facet_nt_signal_t0.csv'

header <- strsplit("evtNb E0.keV pixelNo lineNo dE.keV start_angz start_angphi pixel_angz pixel_angphi", split = ' ')
datae<-read.csv(filename,col.names = header[[1]],  skip = 13)
cat('\nData file name:', filename, ':', nrow(datae) )

# data preparing 2--------------------------------------------------------------
filename <- '05_MeV_facet_nt_signal_t0.csv'
filename <- '3_MeV_facet_nt_signal_t0.csv'
filename <- '6_MeV_facet_nt_signal_t0.csv'
filename <- '7_MeV_facet_nt_signal_t0.csv'
filename <- '9_MeV_facet_nt_signal_t0.csv'

header <- strsplit("evtNb E0.keV layerNo pixelNo lineNo dE.keV start_angz start_angphi pixel_angz pixel_angphi", split = ' ')
datae<-read.csv(filename,col.names = header[[1]],  skip = 14)
cat('\nData file name:', filename, ':', nrow(datae) )



# plot agles -------------------------------------------------------------------

names(datae)

plot(datae$start_angphi, datae$start_angz)



# angles on top of casing
ggplot(data = datae)+
  geom_point(aes(x=start_angphi, y= start_angz))+
  labs(title = filename)+
  # scale_x_continuous(breaks=c(0 , 6, 12, 18),
                     # minor_breaks = c(0:24),
                     # labels = c('00','06', '12', '18'))+
  ylim(0,90)+
  coord_polar(start = 0, direction = -1 )+ 
  theme_bw()+ theme(panel.grid.major = element_line(colour = "black"),
                    panel.grid.minor = element_line(colour = "black"))


# angles on all detectors, deprecated!
ggplot(data = datae)+
  geom_point(aes(x=pixel_angphi, y= pixel_angz))+
  ylim(0,90)+
  coord_polar(start = 0, direction = -1 )+ 
  theme_bw()+ theme(panel.grid.major = element_line(colour = "black"),
                    panel.grid.minor = element_line(colour = "black"))

# angles on top detector
ggplot(data = filter(datae, layerNo == 0))+
  geom_point(aes(x=pixel_angphi, y= pixel_angz))+
  ylim(0,90)+
  coord_polar(start = 0, direction = -1 )+ 
  theme_bw()+ theme(panel.grid.major = element_line(colour = "black"),
                    panel.grid.minor = element_line(colour = "black"))

# angles on middle detector
ggplot(data = filter(datae, layerNo == 1))+
  geom_point(aes(x=pixel_angphi, y= pixel_angz))+
  ylim(0,90)+
  coord_polar(start = 0, direction = -1 )+ 
  theme_bw()+ theme(panel.grid.major = element_line(colour = "black"),
                    panel.grid.minor = element_line(colour = "black"))

# angles on bottom detector
ggplot(data = filter(datae, layerNo == 2))+
  geom_point(aes(x=pixel_angphi, y= pixel_angz))+
  ylim(0,90)+
  coord_polar(start = 0, direction = -1 )+ 
  theme_bw()+ theme(panel.grid.major = element_line(colour = "black"),
                    panel.grid.minor = element_line(colour = "black"))


#  draw all tracks--------------------------------------------------------------
ggplot(data = datae)+
  geom_bin2d(aes(x=pixelNo, y= lineNo))


ggplot(data = filter(datae), aes(x=pixelNo, y= lineNo)) +
  geom_tile()




# select single event ----------------------------------------------------------

evtNumber <- select(sample_n(datae, 1), evtNb)[[1]]

# simple draw track-------------------------------------------------------------
ggplot(data = filter(datae, evtNb == evtNumber), aes(x=pixelNo, y= lineNo)) +
  geom_tile(aes(fill =  dE.keV)) +
  coord_fixed(ratio = 1)


# draw track layer by layer ----------------------------------------------------
ggplot(data = filter(datae, evtNb == evtNumber), aes(x=pixelNo, y= lineNo)) +
  geom_tile(data = filter(datae, evtNb == evtNumber & layerNo == 0), aes(fill =  dE.keV)) +
  geom_tile(data = filter(datae, evtNb == evtNumber & layerNo == 1), aes(fill =  dE.keV)) +
  geom_tile(data = filter(datae, evtNb == evtNumber & layerNo == 2), aes(fill =  dE.keV)) +
  coord_fixed(ratio = 1)

# draw track all layers --------------------------------------------------------
ggplot(data = filter(datae, evtNb == evtNumber), aes(x=pixelNo, y= lineNo)) +
  geom_tile(aes(fill =factor(layerNo), alpha = dE.keV))+
  coord_fixed(ratio = 1) #+ scale_fill_manual()

ggsave(paste(evtNumber, '.pdf'), device = "pdf")

# plot each event separately ---------------------------------------------------
for (evtNumber in unique(datae$evtNb))
{
  ggplot(data = filter(datae, evtNb == evtNumber), aes(x=pixelNo, y= lineNo)) +
    geom_tile(aes(fill =  dE.keV)) +
    coord_fixed(ratio = 1)
  ggsave(paste(evtNumber, '.pdf'), device = "pdf")
  
  # specify device when saving to a file with unknown extension
  # (for example a server supplied temporary file)
  # ggsave(file, device = "pdf")
}

# old plot angles (plot angels, haha!)----------------------------------------------
summary(datae)
#bad
plot(datae$start_angz, datae$pixel_angz)
#bad
plot(datae$start_angphi, datae$pixel_angphi)
#bad
plot(datae$dE.keV, datae$start_angz)




# all some other plots ----------------------------------------------------


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



pdf(paste("elecprotdde3.pdf", sep="" ), width =20, height=20)
n = 20
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 0.5))
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



# off active device  ----------------------------------------------------------


dev.off()

