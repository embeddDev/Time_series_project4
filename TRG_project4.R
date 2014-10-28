############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################

sat.dat <- read.csv("Satelliteorbit.csv",header=FALSE,col.names=c("rm","theta_m","V3"))
sat.dat$rm[2] = sat.dat$theta_m[2]
sat.dat$theta_m[2] = sat.dat$V3[2]
sat.dat$V3 = NULL
plot(y = sat.dat$rm * sin(sat.dat$theta_m),
     x = sat.dat$rm * cos(sat.dat$theta_m),
     type='l',
     col=2,
     lwd=3,
     ylab = "y",
     xlab = "x",
     main = "Satellite measurement data")
grid()
 #TASK1 - State space model



set.seed(1213)
