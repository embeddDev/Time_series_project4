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

#we have  >>>  x = t[r theta omega]
#we want >>> x' = A*x +B*u and y = Cx
#B*u is zero, because there is no input to the system
# all states are output, thus, C is an identity matrix, dimension 2x3
A = matrix(c(1,0,0, 0,1,1, 0,0,1),nrow=3,ncol=3,byrow=TRUE,
           dimnames = list(c("row1", "row2","row3"),
                           c("C.1", "C.2", "C.3")))
C = matrix(c(1,0,0, 0,1,0),nrow=2,ncol=3,byrow=TRUE)

#error_r =  rnorm(n=50,mean=0,sd=500^2)
#error_theta = rnorm(n=50,mean=0,sd=0.005^2)
#error_omega = rnorm(n=50,mean=0,sd=0.005^2)
#error = matrix (c(error_r, error_theta, error_omega),nrow=3,ncol=50,byrow=TRUE)

# R_earth = 6.371E6 # fengid af google
# r = R_earth + sat.dat$rm[1]
# g_orbit = 9.81*(R_earth/r)^2
# v_orbit = sqrt(r*g_orbit)

#Part 2

qr(cbind(t(C),t(C%*%A)))$rank
sigma1 = matrix(c(500,0,0, 0, 0.005,0 , 0,0,0.005),nrow=3,ncol=3,byrow=TRUE) #var in diag
sigma2 = matrix(c(2000,0, 0,0.03),nrow=2,ncol=2,byrow=TRUE)
n = length(sat.data)
# Initialize variables for storing results
Kt.store <- matrix(NA,n,6)
Xhr <- Xhp <- Kt.store
Sxx.r <- Sxx.p <- matrix(NA,n,9) # 3x3 flett ut
Syy.p <- rep(NA,n,4)            # 2x2 flett ut

# Initiailzation
Xh.t.tm1 <- matrix(c(sat.dat$rm[1], sat.dat$theta_m[1],0),nrow=3,ncol=1,byrow=T)
Sxx.t.tm1 <- sigma1 #G * sigma1 * t(G)
Syy.t.tm1 <- C %*% Sxx.t.tm1 %*% t(C) +sigma2

set.seed(1213)
