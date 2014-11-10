############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################

# ---------------- PART 2 ----------------

# Na i gogn i Makka
# dat <- rbind(read.csv2("Elspot2013.csv",encoding = "UTF-8"),read.csv2("Elspot2014forMac.csv",encoding = "UTF-8"))
# Na i gogn i Windows
dat <- rbind(read.csv2("Elspot2013.csv",encoding = "UTF-8"),read.csv2("Elspot2014.csv",encoding = "UTF-8"))
dat$Time <- seq(as.POSIXct(as.character(dat$Date[1]),format="%e.%m.%Y",tz="CET")+3600,by=3600,length.out=nrow(dat))
dat$Hours <- as.numeric(dat$Hours)
#dat$Price = as.numeric(dat$Price)
dat$Date <- format(dat$Time,format="%Y-%m-%d")

plot(ts(dat$Price,start=c(2013,1),f=(365*24)),
     type = 'l',
     main='Price of Electricity',
     xlab='Time Index',
     ylab='Price',
     col="blue"
     )
# plot med godum toppi, viljum losna vid hann

dat$Price[dat$Price > 600] <- 600
dat$Price[dat$Price < 0] <- 0
plot(dat$Price,
     type = 'l',
     main='Price of Electricity',
     xlab='Time Index',
     ylab='Price',
     col="blue")
# litur ut fyrir ad vera nokkurn vegin stationary, notum recursive dot

layout(1:2)
acf(dat$Price)
pacf(dat$Price)
# Greinilega eitthvad seasonal i tessu, notum (1,0,1)^24 og (1,0,0)^168 eins og gefid er i verkefni

# mdl <- lm(dat$Price[169] ~ dat$Wind[169] + 
#             dat$Consumption[169] + 
#             dat$Price[168] +
#             dat$Price[145]+
#             dat$Price[1],
#           x=T,y=T)

#(XX = as.matrix(mdl$x))

#nSampYear= 8759
nSampYear = 4000
nSamp = 1100
#mdl <- lm(rep(1,(nSamp-168)) ~ dat$Wind[169:nSamp] + 
mdl <- lm(dat$Price[169:nSamp] ~ dat$Wind[169:nSamp] + 
            dat$Consumption[169:nSamp] + 
            dat$Price[168:(nSamp-1)] +
            dat$Price[145:(nSamp-24)]+
            dat$Price[1:(nSamp-168)],
          x=T,y=T)
Y = as.matrix(mdl$y)
acf(mdl$residuals)
pacf(mdl$residuals)
Pt = dat$Price[169:nSampYear]
Pt1 = dat$Price[(169-1):(nSampYear-1)]
Lt = dat$Consumption[169:nSampYear]
Pt24 = dat$Price[ (169-24) :(nSampYear-24)]
Pt168 = dat$Price[1:(nSampYear - 168)]
Wt = dat$Wind[169:nSampYear]
P0 = rep(1,(nSampYear-168))
err = c(mdl$residuals, rep(0,(length(Pt)-length(mdl$residuals))))

XX = cbind(P0,Wt,Lt,Pt168,Pt24,Pt1,err)

# Initialize-a th?etu, sem 0 vigur
theta = matrix(0,7,1)
# Initialize-a R = zeros(7x7) matrix
Rt = matrix(0,7,7)
n = nrow(XX)-1
# Initialize empty matix and vector for storing
theta.store <- matrix(NA,nrow(XX),7)
det.store = matrix(NA,nrow(XX),1)
yhat <- rep(NA,nrow(XX))
lmbd <- 1
# gera recursive:
# for t = 169:n
for(tt in 1:n){
  (xt <- XX[tt,])
  #Update Rt
  (Rt <- xt%*%t(xt) + lmbd*Rt)
  det.store[tt] = det(Rt)
  # Use "try" until Rt becomes invertible
  theta.try <- try(theta + solve(Rt)%*%xt%*%(Y[tt] - t(xt)%*%theta),silent=T)
  if(class(theta.try) != "try-error"){
    #Update theta
    theta <- theta.try
    #Store
    theta.store[tt,] <- theta
    #Predict
    yhat[tt+1] <- t(XX[(tt+1),]) %*% theta
    XX[tt+1,7] = (Pt[tt+1]- yhat[tt+1]) 
  }
}

# nota fyrsta arid til ad optimera lambda, profa svo that lambda a seinna arid. 












