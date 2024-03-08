getwd()
setwd("C:/Users/andik/OneDrive/Documents/ARIMA KP")
getwd()
library(fpp2)
library(tseries)
library(TSstudio)
library(adfExplorer)
library(TSA)
library(ggplot2)
library(dplyr)
library(zoo)
library(MASS)
library(EnvStats)
library(lmtest)
library(forecast)
library(aTSA)
library(remotes)
library(fit)
library(xlsx)
library(FinTS)
library(rugarch)
library(xts)
library(MLmetrics)
library(padr)
data1<- read.csv('datahargaberas.csv', header =TRUE,sep = 
                   ';')
data1

x=data1$Waktu
class(x)
y=data1$Harga.Beras
require(zoo)
x1<- as.Date(x,"%m/%d/%Y")
x1
x2<-as.yearmon(x,"%Y/%B")
x2
class(x2)
class(x1)
class(y)
data2<-data.frame(x1,y)
colnames(data2)<-(c("waktu","harga"))
data2
data=data2
data
plot(data)
plot.ts(data)
ggplot(data, aes(x=waktu)) +
  geom_line(aes(y=harga)) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Plot Time Series Data Harga Beras 11 Tahun Terakhir",
       subtitle = "(Januari 2013 - Desember 2023)") +
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5),
        legend.position = "bottom",
        panel.background = element_rect(fill=NA))
x3<-c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
x3
timeseries <- ts(data$harga)
plot.ts(timeseries, lty=1, xlab="Tahun", ylab="Harga Beras", main="Plot  Harga Beras 2013 - 2023")
####dicek lagi##
##uji formal stasionaritas##
nilai_lambda <- BoxCox.lambda(data$harga)
nilai_lambda
datas_var <- 1/data$harga
datas_var
nilai_lambda1<- BoxCox.lambda(datas_var)
nilai_lambda1
##2nd transformation for non stationary data
datas_var1<-datas_var^2
datas_var1
nilai_lambda2<-BoxCox.lambda(datas_var1)
nilai_lambda2

datas_var2<-datas_var1^1.140704
datas_var2
nilai_lambda3<-BoxCox.lambda(datas_var2)
nilai_lambda3

acf(datas_var2, lag.max = 35)





tseries::adf.test(datas_var2) #gak stasioner
acf(data$harga,lag.max = 35)
pacf(data$harga,lag.max = 35)
#differencing 1
train.diff<-diff(datas_var2, differences = 1)
train.diff
plot.ts(train.diff, lty=1, xlab="Tahun", ylab=" Harga Beras", main="Plot Difference 1 Harga Beras")
points(train.diff)
acf(train.diff, lag.max=35, main="data difference 1 Harga Beras")
pacf(train.diff, lag.max=35, main="data difference 1 Harga Beras")
tseries::adf.test(train.diff)
train.diff
traindiff<-data.frame(train.diff)
traindiff
write.csv(traindiff, file = 'traindiff.csv')
getwd()

#penentuan ordo ARIMA
acf(train.diff, lag.max=35, main="ACF difference 1 Harga Beras")
pacf(train.diff, lag.max=35, main="PACF difference 1 Harga Beras")
eacf(train.diff)
eacf(train.diff2)
best_arima<-auto.arima(data$harga,trace = TRUE)
fcast=forecast(best_arima)
fcast
forecast(best_arima)
class(best_arima)
plot(fcast)
accuracy(best_arima)
class(best_arima)
class(fcast)

model1<-arima(x=data$harga,order = c(0,1,0))
model1
forecast(model1)
forecast(model1)
class(model1)
accuracy(arima(data$harga,order = c(0,1,0)))
accuracy(fcast)
class(fcast)
pred1<-forecast(model1,h=12)
pred1

#possible model based on eacf###
##checking rmse and mape

measure1<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(0,1,0),method='ML'),h=12))
measure2<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(0,1,1),method='ML'),h=12))
measure3<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(0,1,11),method='ML'),h=12))
measure4<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(1,1,0),method='ML'),h=12))
measure5<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(1,1,1),method='ML'),h=12))
measure6<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(2,1,1),method='ML'),h=12))
measure7<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(2,1,2),method='ML'),h=12))
measure8<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(3,1,1),method='ML'),h=12))
measure9<-accuracy(forecast::forecast(ts(data$harga),model=
                                        arima(ts(data$harga),order = c(3,1,2),method='ML'),h=12))
measure10<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(4,1,0),method='ML'),h=12))
measure11<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(5,1,0),method='ML'),h=12))
measure12<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(5,1,1),method='ML'),h=12))
measure13<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(6,1,0),method='ML'),h=12))
measure14<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(6,1,1),method='ML'),h=12))
measure15<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(7,1,2),method='ML'),h=12))
measure16<-accuracy(forecast::forecast(ts(data$harga),model=
                                         arima(ts(data$harga),order = c(7,1,6),method='ML'),h=12))

measuretotal<- rbind(measure1,measure2,measure3,measure4,measure5,measure6
                     ,measure7,measure8,measure9,measure10,measure11,measure12
                     ,measure13,measure14,measure15,measure16)

measuretotal

##checking AIC
aic1 <- Arima(data$harga, order=c(0,1,0), method="ML")
aic2 <- Arima(data$harga, order=c(0,1,1), method="ML")
aic3 <- Arima(data$harga, order=c(0,1,11), method="ML")
aic4 <- Arima(data$harga, order=c(1,1,0), method="ML")
aic5 <- Arima(data$harga, order=c(1,1,1), method="ML")
aic6 <- Arima(data$harga, order=c(2,1,1), method="ML")
aic7 <- Arima(data$harga, order=c(2,1,2), method="ML")
aic8 <- Arima(data$harga, order=c(3,1,1), method="ML")
aic9 <- Arima(data$harga, order=c(3,1,2), method="ML")
aic10 <- Arima(data$harga, order=c(4,1,0), method="ML")
aic11 <- Arima(data$harga, order=c(5,1,0), method="ML")
aic12 <- Arima(data$harga, order=c(5,1,1), method="ML")
aic13 <- Arima(data$harga, order=c(6,1,0), method="ML")
aic14 <- Arima(data$harga, order=c(6,1,1), method="ML")
aic15 <- Arima(data$harga, order=c(7,1,2), method="ML")
aic16 <- Arima(data$harga, order=c(7,1,6), method="ML")

aictotal<-rbind(aic1$aic,aic2$aic,aic3$aic,aic14$aic,aic5$aic
                ,aic6$aic,aic7$aic,aic8$aic,aic9$aic,aic10$aic,
                aic11$aic,aic12$aic,aic13$aic,aic14$aic,
                aic15$aic,aic16$aic)

aictotal
##checking statistics model for each model
stat2<-lmtest::coeftest(aic2); stat2
stat3<-lmtest::coeftest(aic3)
stat4<-lmtest::coeftest(aic4)
stat5<-lmtest::coeftest(aic5)
stat6<-lmtest::coeftest(aic6)
stat7<-lmtest::coeftest(aic7)
stat8<-lmtest::coeftest(aic8)
stat9<-lmtest::coeftest(aic9)
stat10<-lmtest::coeftest(aic10)
stat11<-lmtest::coeftest(aic11)
stat12<-lmtest::coeftest(aic12)
stat13<-lmtest::coeftest(aic13)
stat14<-lmtest::coeftest(aic14)
stat15<-lmtest::coeftest(aic15)
stat16<-lmtest::coeftest(aic16)

##checking residual for best arima model (2,1,2)
residual1<-aic7$residuals
residual1
par(mfrow = c(2,2))
qqnorm(residual1)
qqline(residual, col = "blue", lwd = 2)
plot(residual1, type="o", 
     ylab = "Residual", xlab = "Order", main = "Residual ARIMA (2,1,2) vs Order")
abline(h = 0, col='red')
acf(residual1)
pacf(residual1)

ks.test(residual1,"pnorm")
Box.test(residual1, type = 'Ljung')
t.test(residual1,mu=0,conf.level = 0.95)
bptest_model1 <- bptest(residual1 ~ fitted(aic7))
bptest_model1
#forecasting best model

# Membuat data yang akan digunakan untuk forecast
data_forecast <- data.frame(dummyvar = dummyvar)
lenghth(dummyvar)
# Melakukan forecast
bestforecast <- forecast::forecast(modelARIMAX, newdata = data_forecast)
bestforecast<-forecast::forecast(modelARIMAX, h=12)
bestforecast
bptest_model1 <- bptest(model7$residuals ~ fitted(model7))
plot(bestforecast)
forecastfinal<-data.frame(bestforecast)
forecastfinal
plot.ts(forecastfinal$Point.Forecast, lty=1, xlab = 'Bulan',
        ylab='Harga (Rupiah)', main='Prediksi Harga
        Beras Jawa Timur 2024')


#Arch Model Identification
model7 <- arima(data$harga, order=c(2,1,2), method="ML")
model7
aTSA::arch.test(model7)


for (i in 1:20) {
  ArchTest <- ArchTest(model7$residuals, lags=i, demean=TRUE)
  cat("P Value LM Test lag ke", i,"adalah" , ArchTest$p.value, "\n") }
ArchTest(model7$residuals)

##checking outlier
detectAO(model7)
detectIO(model7)
data$harga
## outlier ada di t 27,28,61,62,123,130
out <- 27; d1 <- c(rep(0,out-1),1,rep(0,length(data$harga)-out))
out <- 28; d2 <- c(rep(0,out-1),1,rep(0,length(data$harga)-out))
out <- 61; d3 <- c(rep(0,out-1),1,rep(0,length(data$harga)-out))
out <- 62; d4 <- c(rep(0,out-1),1,rep(0,length(data$harga)-out))
out <- 123; d5 <- c(rep(0,out-1),1,rep(0,length(data$harga)-out))
out <- 130; d6 <- c(rep(0,out-1),1,rep(0,length(data$harga)-out))
dummy <- cbind(d1,d2,d3,d4,d5,d6)
# ARIMA dengan Outlier --> ARIMAX MODELLING, i.e. ARIMAX(1,0,0)
modelARIMAX = arima(data$harga, order=c(2,1,2), xreg=dummy, include.mean=FALSE)
length(data$harga)
length(dummy)
dummy
summary(modelARIMAX)
coeftest(modelARIMAX)
resi.ARIMAX=as.ts(modelARIMAX$residuals) #define the residual value
fits.ARIMAX=as.ts(fitted(modelARIMAX))

shapiro.test(resi.ARIMAX)
shapiro.test(model7$residuals)
plot.ts(modelARIMAX$series)
plot(modelARIMAX$residuals)

#forecasting arimax model
forecastARIMAX <- predict(modelARIMAX,n.ahead = 12, newxreg=dummy)
forecastARIMAX
plot.ts(forecastARIMAX$se)


#modelling garch
par(mfrow=c(2,1))
Acf((model7$residuals)^2, 
    main ='ACF Data', 
    lag.max = 40)
Pacf((model7$residuals)^2, 
     main ='PACF Data', 
     lag.max = 40)
eacf((model7$residuals)^2)

coef(garch(model7$residuals, order = c(1,1)))
garch1<-garch(model7$residuals, order = c(1,1))
garch1
class(garch1)
# Forecasting hybrid arima-garch model
specfix <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(2,2), external.regressors = dummy))

modelARIMAX_GARCH <- ugarchfit(specfix, data = dataharga)

forecast_ARIMAX_GARCH <- ugarchforecast(modelARIMAX_GARCH, n.ahead = 10)
print(forecast_ARIMAX_GARCH)



garchFitt <- ugarchfit(spec=garchSpec, data=train.diff)
forc<- ugarchforecast(fitORspec = garchFitt, data = data$harga, n.ahead = 12, n.roll = 0)
plot(forc, which= 1)

#show aic and bic of possible model
spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(2,1,2)))
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
                    mean.model = list(armaOrder = c(2,1,2)))
spec3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0,1)),
                    mean.model = list(armaOrder = c(2,1,2)))

dataharga <- ts(data$harga)

fit1 <- ugarchfit(spec, data = dataharga)
fit2 <- ugarchfit(spec2, data = dataharga)
fit2
fit3 <- ugarchfit(spec3, data = dataharga)


crit1 <- infocriteria(fit1)
crit2 <- infocriteria(fit2)
crit3 <- infocriteria(fit3)

##diagnostic test for best hybrid model##
#normality test

ks.test(residuals(fit1),"pnorm")

Box.test(residuals(fit1), lag = 30, type = 'Ljung-Box')
for (i in 1:30) {
  ArchTest <- ArchTest(fit@fit$residuals, lags=i, demean=TRUE)
  cat("P Value LM Test lag ke", i,"adalah" , ArchTest$p.value, "\n") }

class(fit)
residuals <- residuals(fit1)
squared_residuals <- residuals^2
ljung_box_test <- Box.test(squared_residuals, lag = 30, type = "Ljung-Box")
print(paste("Ljung-Box Test p-value: ", ljung_box_test$p.value))
#forecast value after transform diff data
ramal <- ugarchforecast(fit1, n.ahead = 12)
ramal

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(2,1,2)))

fit <- ugarchfit(spec, data = data$harga)
fit
forecasthybrid <- ugarchforecast(fit, n.ahead = 12)
forecasthybrid
df4<-data.frame(forecasthybrid@forecast$seriesFor)
plot(forecasthybrid@forecast$seriesFor)
plot.ts(forecasthybrid@forecast$seriesFor, lty=1, xlab = 'bulan', ylab = 'ramalan harga',
        main = 'Ramalan Arima (2,1,2)-Garch(1,1) Harga Beras Jawa Timur 2024')
actual_values <- window(data$harga, start = end(data$harga) - 13)
actual_values
class(forecasthybrid)
# Hitung MAPE
forecasthybrid
mape <- mean(abs((actual_values - as.numeric(fitted(forecasthybrid))) / actual_values)) * 100
mape


#checking heteroskedasticity
fit_garch1 <- ugarchfit(spec = spec, data = data$harga)
fit_garch1
fit
bptest_model <- bptest(fit@fit$residuals ~ fitted(fit))
bptest_model
bptest_model <- bptest(forecasthybrid@forecast$seriesFor ~ fitted(forecasthybrid))

# Peramalan
garchSpec <- ugarchspec(
  variance.model=list(model="sGARCH",
                      garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(2,1,2)),
  distribution.model="std")
garchFitt <- ugarchfit(spec=garchSpec, data=dataharga)
forc<- ugarchforecast(fitORspec = garchFitt, data = dataharga, n.ahead = 41, n.roll = 0)
plot(forc, which= 1, xlab='Harga (Rupiah)', main = 'Harga Beras Medium di Jawa Timur')

