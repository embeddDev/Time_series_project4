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







