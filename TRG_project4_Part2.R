############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################

# ---------------- PART 2 ----------------

dat <- rbind(read.csv2("Elspot2013.csv",encoding = "UTF-8"),read.csv2("Elspot2014.csv",encoding = "UTF-8"))
dat$Time <- seq(as.POSIXct(as.character(dat$Date[1]),format="%e.%m.%Y",tz="CET")+3600,by=3600,length.out=nrow(dat))
dat$Hour <- as.numeric(dat$Hour)
dat$Date <- format(dat$Time,format="%Y-%m-%d")

plot(dat$Price,
     type = 'l',
     main='Price of Electricity',
     xlab='Time Index',
     ylab='Price')
# plot med godum toppi, viljum losna vid hann

dat$Price[dat$Price > 600] <- 600
dat$Price[dat$Price < 0] <- 0
plot(dat$Price,
     type = 'l',
     main='Price of Electricity',
     xlab='Time Index',
     ylab='Price')
# litur ut fyrir ad vera nokkurn vegin stationary, notum recursive dot

par(mfrow = c(2,1))
acf(dat$Price)
pacf(dat$Price)
# Greinilega eitthvad seasonal i tessu, notum (1,0,1)^24 og (1,0,0)^168 eins og gefid er i verkefni

#R_null = (0 0 0 ; 0 0 0 0) (7x7 fylki af núllum)
#R_einn er þá R_null + x_1*x^T
#sum of squares , inniheldur þennan part af covariancinum
#þeta(covariancinn held ég) =[ (x^T*X)^-1 ] *x^T*Y (á bladi)

# Initialize-a X, fyrsta lina i X er (1 Wind(169) Consumtion(168) Price(168) Price(145) P(1) eps(145))

búa til df sem inniheldur Wind(169) Consumtion(168) Price(168) Price(145) P(1) P(0)

mdl <- lm(P(0) ~ Wind(169) + Consumtion(168) + Price(168) + Price(145) + P(1),x=T,y=T)

X <- cbind(mdl$x,NA)
Y <- mdl$y

X[1:1000,7] <- mdl$residuals[1:1000]

# Initialize-a Þetu, sem 0 vigur

# Initialize-a R = zeros(7x7) matrix

# gera recursive:
# for t = 169:n
# Reikna R(t) =  lambda*R(t-1) + x(t)*x(t)^T   byrja með lambda = 1, fá til að virka
# Uppfæra þeta(þeta) = þeta(t-1)+R(t)^-1*x(t)*(Y(t)-X(t)^T*þeta(t-1)) setja try utanum if(class(þeta)!="try-error") ef þetta heldur þá þetta true og þá hægt að reikna restina af dótinu
# Reikna Y(t+k|t) = Price(t) = langa jafnan
# á morgun veit ég 
# Uppfæra X, þe. uppfæra eps 


# nota fyrsta árið til að optimera lambda, prófa svo það lambda á seinna árið. 












