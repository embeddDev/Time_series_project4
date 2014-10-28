############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################
require(graphics)
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

error_r =  rnorm(n=50,mean=0,sd=500^2)
error_theta = rnorm(n=50,mean=0,sd=0.005^2)
error_angular = rnorm(n=50,mean=0,sd=0.005^2)

A = matrix(c(1,0,0, 0,1,1, 0,0,1),nrow=3,ncol=3,byrow=TRUE,dimnames.
           dimnames = list(c("row1", "row2"),
                           c("C.1", "C.2", "C.3")))
set.seed(1213)
