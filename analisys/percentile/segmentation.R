setwd("C:/Simulink")
# plot(NA, xlim=c(2000,30000), ylim=c(0,1500), xlab="x", ylab="y")
# E0 <- c(2000,4000,
#         9000,15000,
#         30000,53000,
#         100000,160000,
#         Inf)
# 
# dE.det1 <- c(753,1128,381,571,231,332,116,215)
# s <- seq(length(E0)-1)
# segments(E0[s],dE.det1[2*s-1],E0[s+1], col="red")
# segments(E0[s],dE.det1[2*s],E0[s+1], col="blue")

plot(NA, xlim=c(200,160000), ylim=c(0,7000), xlab="x", ylab="y", log = "x")
E0p <- c(2000,4000,
        9000,15000,
        30000,53000,
        100000,160000,
        Inf)
dE.det2 <- c(1214,2563,
             4363,6963,
             3912,5511,
             2144,3012,
             1287,1692,
             770,1079,
             545,927,
             291,518)

s <- seq(length(E0p)-1)
segments(E0p[s],dE.det2[2*s-1],E0p[s+1], col="red")
segments(E0p[s],dE.det2[2*s],E0p[s+1], col="blue")

E0e <- c(150, 350,
         350,600,
         1000,2000,
         4000,10000)
dE.det2 <- c(114,223,
             239,425,
             193,492,
             162,327,
             150,240,
             165,238)

s <- seq(length(E0p)-1)
segments(E0e[s],dE.det2[2*s-1],E0e[s+1], col="magenta")
segments(E0e[s],dE.det2[2*s],E0e[s+1], col="green")
# 


# детектор 3
# plot(NA, xlim=c(600,600000), ylim=c(0,7000), xlab="x", ylab="y", log = "x")
# E0 <- c(53000,
#         100000,160000,
#         600000)
# dE.det3 <- c(0,3455,
#              1022,1629,
#              452,980)
# 
# s <- seq(length(E0)-1)
# segments(E0[s],dE.det3[2*s-1],E0[s+1], col="red")
# segments(E0[s],dE.det3[2*s],E0[s+1], col="blue")
# 
# E0e <- c(600,
#          1000,2000,
#          4000,10000)
# dE.det3 <- c(0,337,
#              89,1012,
#              608,2272,
#              1580,3863
# )
# 
# s <- seq(length(E0)-1)
# segments(E0e[s],dE.det3[2*s-1],E0e[s+1], col="magenta")
# segments(E0e[s],dE.det3[2*s],E0e[s+1], col="green")
