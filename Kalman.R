

y <- c(10171,10046,10082,9962,9702,10012,9786,9748,9933,9418,NA,NA,9330)

n <- length(y)

# System Description
A <- matrix(c(1,0,1,1),2,2)
B <- matrix(-c(.5,1),2,1)
C <- matrix(c(1,0),1,2)
u <- 9.82
Sigma1 <- matrix(c(2,.8,.8,1),2,2)
Sigma2 <- 10000

# Initialize variables for storing results
Kt.store <- matrix(NA,n,2)
Xhr <- Xhp <- Kt.store
Sxx.r <- Sxx.p <- matrix(NA,n,4)
Syy.p <- rep(NA,n)

# Initiailzation
Xh.t.tm1 <- matrix(c(10000,0))
Sxx.t.tm1 <- matrix(0,2,2)
Syy.t.tm1 <- Sigma2

# Reconstruction
Kt <- Sxx.t.tm1 %*% t(C) %*% solve(Syy.t.tm1)
Xh.t.t <- Xh.t.tm1 + Kt %*% (y[1] - C%*%Xh.t.tm1)
Sxx.t.t = Sxx.t.tm1 - Kt %*% Syy.t.tm1 %*% t(Kt)

#Prediction
Xh.tp1.t <- A %*% Xh.t.t + B%*%u
Sxx.tp1.t <- A %*% Sxx.t.t %*% t(A) + Sigma1
Syy.tp1.t <- C %*% Sxx.tp1.t %*% t(C) + Sigma2

# Store step result
Kt.store[1,] <- t(Kt)
Xhr[1,] <- t(Xh.t.t)
Sxx.r[1,] <- as.vector(Sxx.t.t)
Xhp[1,] <- t(Xh.tp1.t)
Sxx.p[1,] <- as.vector(Sxx.tp1.t)
Syy.p[1] <- Syy.tp1.t

# Prepare for next iteration
Xh.t.tm1 <- Xh.tp1.t
Sxx.t.tm1 <- Sxx.tp1.t
Syy.t.tm1 <- Syy.tp1.t

# Loop through the rest

for(tt in 2:n){
  # Reconstruction
  if(is.na(y[tt])){
    yy <- C%*%Xh.t.tm1
  }else{
    yy <- y[tt]
  }

  Kt <- Sxx.t.tm1 %*% t(C) %*% solve(Syy.t.tm1)
  Xh.t.t <- Xh.t.tm1 + Kt %*% (yy - C%*%Xh.t.tm1)
  Sxx.t.t = Sxx.t.tm1 - Kt %*% Syy.t.tm1 %*% t(Kt)

  #Preciction
  Xh.tp1.t <- A %*% Xh.t.t + B%*%u
  Sxx.tp1.t <- A %*% Sxx.t.t %*% t(A) + Sigma1
  Syy.tp1.t <- C %*% Sxx.tp1.t %*% t(C) + Sigma2

  # Store step result
  Kt.store[tt,] <- t(Kt)
  Xhr[tt,] <- t(Xh.t.t)
  Sxx.r[tt,] <- as.vector(Sxx.t.t)
  Xhp[tt,] <- t(Xh.tp1.t)
  Sxx.p[tt,] <- as.vector(Sxx.tp1.t)
  Syy.p[tt] <- Syy.tp1.t

  # Prepare for next iteration
  Xh.t.tm1 <- Xh.tp1.t
  Sxx.t.tm1 <- Sxx.tp1.t
  Syy.t.tm1 <- Syy.tp1.t
}

# Manipulate data for ggplot

dat <- data.frame("Variable"=c(rep("Position",n),rep("Velocity",n)),
                  "Type"="Reconstruction","Time"=rep(0:(n-1),2),"Value"=as.vector(Xhr))

dat <- rbind(dat,data.frame("Variable"=c(rep("Position",n),rep("Velocity",n)),
                            "Type"="Prediction","Time"=rep(1:n,2),"Value"=as.vector(Xhp)))

dat <- rbind(dat,data.frame("Variable"=c(rep("Position",n)),
                            "Type"="Observation","Time"=rep(0:(n-1),1),"Value"=as.vector(y)))

kdat <- data.frame("Variable"=c(rep("Position",n),rep("Velocity",n)),"Type"="Kalman Gain",
                   Time=0:(n-1),Value=as.vector(Kt.store))

require(ggplot2)
plt <- ggplot(data=dat,aes(x=Time,y=Value,color=Type))

plt + geom_line(size=1) + geom_point(size=3.5) + facet_wrap( ~ Variable,scales="free")

plt <- ggplot(data=dat[dat$Variable=="Position",],aes(x=Time,y=Value,color=Type))

plt + geom_line(size=1) + geom_point(size=3.5) + facet_wrap( ~ Variable,scales="free") + ylab("Position [m]")

plt <- ggplot(data=dat[dat$Variable=="Velocity",],aes(x=Time,y=Value,color=Type))

plt + geom_line(size=1) + geom_point(size=3.5) + facet_wrap( ~ Variable,scales="free") + ylab("Velocity [m/s]")


kplt <- ggplot(data=kdat,aes(x=Time,y=Value,color=Type))
kplt + geom_line(size=1) + geom_point(size=3.5) + facet_wrap( ~ Variable,scales="free") + ylab("Gain")

