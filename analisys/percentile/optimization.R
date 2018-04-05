setwd("d:/Simulink")

# import data from Monte-Carlo text files
data<-read.table("data/det2/electr_A_L32_det2.dat", header = TRUE)

# defining primary energy intervals
breaks <- c(0, 150, 350,  600,1000,2000,4000,10000,Inf)
labels <- c('c0','c1','c2','c3','c4','c5', 'c6','c7')

# make factor column on primary energy
data$bins <- cut(data$E0.keV, breaks, labels= labels)

get_interval <- function(x=5, data, channel){
  c=(apply(data[(data$bins == channel)&(data$CF == 1),2:5],  # drop all not used info
            2,  # apply to columns
            quantile,  # applyed function
            probs=c(x,  100-x)/100))  #variated quantile number
  return(c)
}

get_interval(5,data, labels[1])
