############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################

# ---------------- PART 2 ----------------
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

#R_null = (0 0 0 ; 0 0 0 0) (7x7 fylki af núllum)
Rt 
#R_einn er þá R_null + x_1*x^T
#sum of squares , inniheldur thennan part af covariancinum
#theta(covariancinn held eg) =[ (x^T*X)^-1 ] *x^T*Y (á bladi)

# Initialize-a X, fyrsta lina i X er (1 Wind(169) Consumtion(168) Price(168) Price(145) P(1) eps(145))

#bua til df sem inniheldur Wind(169) Consumtion(168) Price(168) Price(145) P(1) P(0)

# mdl <- lm(P(0) ~ dat$Wind(169) + 
#             dat$Consumtion(168) + 
#             dat$Price(168) +
#             dat$Price(145) + 
#             P(1)
#           ,x=T,y=T)
mdl <- lm(dat$Price ~ dat$Wind(169) + 
            dat$Consumtion(168) + 
            dat$Price(168) +
            dat$Price(145),
          x=T,y=T)
#mdl = arma(dat$Price, order= c(2,0,0),
#          seasonal = (list(order=c(1,0,1),period=24))) )
X <- cbind(mdl$x,NA)
Y <- mdl$y

X[1:1000,7] <- mdl$residuals[1:1000]

# Initialize-a th�etu, sem 0 vigur

# Initialize-a R = zeros(7x7) matrix

# gera recursive:
# for t = 169:n
for(t in 169:n){
  # Reikna R(t) =  lambda*R(t-1) + x(t)*x(t)^T   byrja med lambda = 1, fa til ad virka
  # Uppfaera theta(theta) = theta(t-1)+R(t)^-1*x(t)*(Y(t)-X(t)^T*theta(t-1)) setja try utanum if(class(þeta)!="try-error") ef þetta heldur þá þetta true og þá hægt að reikna restina af dótinu
  # Reikna Y(t+k|t) = Price(t) = langa jafnan
  # a morgun veit eg 
  # Uppfaera X, þe. uppfaera eps   
  
}



# nota fyrsta árið til að optimera lambda, prófa svo það lambda á seinna árið. 












