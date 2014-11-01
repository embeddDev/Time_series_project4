############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################
#require(graphics)
sat.dat <- read.csv("Satelliteorbit.csv",header=FALSE,col.names=c("rm","theta_m","V3"))
sat.dat$rm[2] = sat.dat$theta_m[2]
sat.dat$theta_m[2] = sat.dat$V3[2]
y_rm = sat.dat$rm
y_theta= sat.dat$theta_m
y = matrix(c(y_rm,y_theta),nrow=2,ncol=50,byrow=T)
sat.dat$V3 = NULL

 #TASK1 - State space model

#we have  >>>  x = t[r theta omega]
#we want >>> x' = A*x +B*u and y = Cx
#B*u is zero, because there is no input to the system
# all states are output, thus, C is an identity matrix, dimension 2x3
A = matrix(c(1,0,0, 0,1,1, 0,0,1),nrow=3,ncol=3,byrow=TRUE,
           dimnames = list(c("row1", "row2","row3"),
                           c("C.1", "C.2", "C.3")))
C = matrix(c(1,0,0, 0,1,0),nrow=2,ncol=3,byrow=TRUE)

#Part 2

qr(cbind(t(C),t(C%*%A)))$rank
sigma1 = matrix(c(500,0,0, 0, 0.005,0 , 0,0,0.005),nrow=3,ncol=3,byrow=TRUE) #var in diag
sigma2 = matrix(c(2000,0, 0,0.03),nrow=2,ncol=2,byrow=TRUE)
n = length(y_rm)
# Initialize variables for storing results
Kt.store <- matrix(NA,n,6)

Xhr <- Xhp <- matrix(NA,n,3)
Sxx.r <- Sxx.p <- matrix(NA,n,9) # 3x3 flett ut
Syy.p <- matrix(NA,nrow=n,ncol=4)            # 2x2 flett ut

# Initiailzation
Xh.t.tm1 <- matrix(c(sat.dat$rm[1], sat.dat$theta_m[1],0),nrow=3,ncol=1,byrow=T)
Sxx.t.tm1 <- sigma1 #G * sigma1 * t(G)
Syy.t.tm1 <- C %*% Sxx.t.tm1 %*% t(C) +sigma2

# Reconstruction
Kt <- Sxx.t.tm1 %*% t(C) %*% solve(Syy.t.tm1)

Xh.t.t <- Xh.t.tm1 + Kt %*% (y[,1] - C%*%Xh.t.tm1)  
Sxx.t.t = Sxx.t.tm1 - Kt %*% Syy.t.tm1 %*% t(Kt)

#Prediction
Xh.tp1.t <- A %*% Xh.t.t
Sxx.tp1.t <- A %*% Sxx.t.t %*% t(A) + sigma1
Syy.tp1.t <- C %*% Sxx.tp1.t %*% t(C) + sigma2

# Store step result
Kt.store[1,] <- t(Kt)
Xhr[1,] <- t(Xh.t.t)
Sxx.r[1,] <- as.vector(Sxx.t.t)
Xhp[1,] <- t(Xh.tp1.t)
Sxx.p[1,] <- as.vector(Sxx.tp1.t)
Syy.p[1,] <- as.vector(Syy.tp1.t)                       #warning

# Prepare for next iteration
Xh.t.tm1 <- Xh.tp1.t
Sxx.t.tm1 <- Sxx.tp1.t
Syy.t.tm1 <- Syy.tp1.t

for(tt in 2:n){
  # Reconstruction
  if(is.na(y[,tt])){
    yy <- C%*%Xh.t.tm1
  }else{
    yy <- y[,tt]
  }
  #reconstruction
  Kt <- Sxx.t.tm1 %*% t(C) %*% solve(Syy.t.tm1)
  Xh.t.t <- Xh.t.tm1 + Kt %*% (yy - C%*%Xh.t.tm1)
  Sxx.t.t = Sxx.t.tm1 - Kt %*% Syy.t.tm1 %*% t(Kt)
  
  #Prediction
  Xh.tp1.t <- A %*% Xh.t.t
  Sxx.tp1.t <- A %*% Sxx.t.t %*% t(A) + sigma1
  Syy.tp1.t <- C %*% Sxx.tp1.t %*% t(C) + sigma2
  
  # Store step result
  Kt.store[tt,] <- t(Kt)
  Xhr[tt,] <- t(Xh.t.t)
  Sxx.r[tt,] <- as.vector(Sxx.t.t)
  Xhp[tt,] <- t(Xh.tp1.t)
  Sxx.p[tt,] <- as.vector(Sxx.tp1.t)
  Syy.p[tt,] <- as.vector(Syy.tp1.t)
  
  # Prepare for next iteration
  Xh.t.tm1 <- Xh.tp1.t
  Sxx.t.tm1 <- Sxx.tp1.t
  Syy.t.tm1 <- Syy.tp1.t
}

rm_p = Xhp[,1]
theta_p = Xhp[,2]
rm_r = Xhp[,1]
theta_r = Xhp[,2]

#plot xhat
plot(x=(rm_p*cos(theta_p)),
     y= (rm_p * sin(theta_p)),type='l',col=3)
lines(x=(rm_p*cos(theta_p)),
     y= (rm_p * sin(theta_p)),type='l',col=3)
lines(y = sat.dat$rm * sin(sat.dat$theta_m),
     x = sat.dat$rm * cos(sat.dat$theta_m),
     type='l',
     col=2,
     lwd=2,
     ylab = "y",
     xlab = "x"
     )
grid()
legend("topright",
       c("prediction", "observation"),
       lty = 1,
       col=c('green', 'red'),
       cex=0.6)



# ---------------- PART 2 ----------------

dat <- rbind(read.csv2("Elspot2013.csv",encoding = "UTF-8"),read.csv2("Elspot2014.csv",encoding = "UTF-8"))
dat$Time <- seq(as.POSIXct(as.character(dat$Date[1]),format="%e.%m.%Y",tz="CET")+3600,by=3600,length.out=nrow(dat))
dat$Hour <- as.numeric(dat$Hour)
dat$Date <- format(dat$Time,format="%Y-%m-%d")
dat$Price[dat$Price > 600] <- 600
dat$Price[dat$Price < 0] <- 0





