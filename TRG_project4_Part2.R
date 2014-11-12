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
dat$Price = ts(dat$Price, start=c(2013,1),frequency=24*365)
dat$Wind = ts(dat$Wind, start=c(2013,1),frequency=24*365)
dat$Consumption = ts(dat$Consumption, start=c(2013,1),frequency=24*365)
plot(dat$Price,
     type = 'l',
     main='Price of Electricity',
     xlab='Time Index',
     ylab='Price',
     col="blue"
)
# plot med godum toppi, viljum losna vid hann

dat$Price[dat$Price > 600] <- 600
dat$Price[dat$Price < 0] <- 0
dat2013 = dat[169:8759,]
dat2014 = dat[(8760:length(dat$Price)),]
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
nSampYear= 8759
#nSampYear = 4000
nSamp = 1000
#mdl <- lm(rep(1,(nSamp-168)) ~ dat$Wind[169:nSamp] +
#Xvar = data.frame(price = dat$Price, W169 = dat$Wind,L169=dat$Consumption,P168=lag(dat$Price,1),P145=lag(dat$Price,24),P1=lag(dat$Price,168))
Xvar2013 = data.frame(price = dat$Price[169:(nSampYear)], 
                      W169 = dat$Wind[169:nSampYear],
                      L169=dat$Consumption[169:nSampYear],
                      P168=dat$Price[168:(nSampYear-1)],
                      P145=dat$Price[145:(nSampYear-24)],
                      P1=dat$Price[1:(nSampYear-168)])
mdl2013 <- lm(price ~ W169+L169+P168+P145+P1,data=Xvar2013,x=T,y=T)
Y = as.matrix(mdl2013$y)
acf(mdl2013$residuals)
pacf(mdl2013$residuals)
#XX = as.matrix(mdl$x)

Pt = dat$Price[169:nSampYear]
Pt1 = dat$Price[(169-1):(nSampYear-1)]
Lt = dat$Consumption[169:nSampYear]
Pt24 = dat$Price[ (169-24) :(nSampYear-24)]
Pt168 = dat$Price[1:(nSampYear - 168)]
Wt = dat$Wind[169:nSampYear]
P0 = rep(1,(nSampYear-168))
err = c(mdl$residuals, rep(0,(length(Pt)-length(mdl$residuals))))
err = as.numeric(err)
XX = cbind(P0,Wt,Lt,Pt168,Pt24,Pt1,err)

recurse_est = function(XX,Y,lmbd){
  # Initialize-a th?etu, sem 0 vigur
  theta = matrix(0,7,1)
  # Initialize-a R = zeros(7x7) matrix
  Rt = matrix(0,7,7)
  n = nrow(XX)-1
  # Initialize empty matix and vector for storing
  theta.store <- matrix(NA,nrow(XX),7)
  det.store = matrix(NA,nrow(XX),1)
  yhat <- rep(NA,nrow(XX))
  # gera recursive:
  # for t = 169:n
  for(tt in 1:n){  
    
    xt <- XX[tt,]
    #Update Rt
    Rt <- xt%*%t(xt) + lmbd*Rt
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
  yhat[yhat > 600]= 600
  return(yhat)  
}
RMSE = function(sim,obs){
  RMSE = mean( (sim[!is.na(sim)] - obs[!is.na(sim)])^2)
}
layout(1)

lambda = 0.3
 y = NA
while(lambda <= 1){
  y_mdl = recurse_est(XX,Y,lambda)
  y = c(y,RMSE(y_mdl,Y))
  lambda = lambda + 0.05
}
barplot(y[2:length(y)],ylab="RMSE",xlab="lambda, 0.3 to 1 increments of 0.05", col=2,main="simulation for different lambdas")
grid()
#Looks as though no forgetting factor gives least RMSE, going for 0.8 because it is plausible that it will improve the model

nSamp = 1000
nSampYear = length(dat2014$Price)-168
Xvar = data.frame(price = dat2014$Price[169:(nSamp)], 
                  W169 = dat2014$Wind[169:nSamp],
                  L169=dat2014$Consumption[169:nSamp],
                  P168=dat2014$Price[168:(nSamp-1)],
                  P145=dat2014$Price[145:(nSamp-24)],
                  P1=dat2014$Price[1:(nSamp-168)])
mdl2014 <- lm(price ~ W169+L169+P168+P145+P1,data=Xvar,x=T,y=T)
Y = matrix(dat2014$Price[169:nSampYear],byrow=T)
Pt = dat2014$Price[169:nSampYear]
Pt1 = dat2014$Price[(169-1):(nSampYear-1)]
Lt = dat2014$Consumption[169:nSampYear]
Pt24 = dat2014$Price[ (169-24) :(nSampYear-24)]
Pt168 = dat2014$Price[1:(nSampYear - 168)]
Wt = dat2014$Wind[169:nSampYear]
P0 = rep(1,(nSampYear-168))
err = c(mdl$residuals, rep(0,(length(Pt)-length(mdl$residuals))))
err = as.numeric(err)
XX = cbind(P0,Wt,Lt,Pt168,Pt24,Pt1,err)
forecast_xx = matrix(rep(XX[6817,],times=17),nrow=17,ncol=7,byrow=T)
XX = rbind(XX, forecast_xx)
Y_forecast = matrix(rep(Y[length(Y)], times=17,nrow=17,ncol=1,byrow=T))
Y = rbind(Y,Y_forecast)
yhat2014 = ts(recurse_est(XX,Y,0.8),start=c(2014,1),frequency=24*365)
plot(,type='l',col=2,ylab="Spot Price")
grid()

