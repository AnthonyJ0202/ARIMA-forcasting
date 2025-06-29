cp<-read.csv("//Users//seoinjeong//Downloads//cp.csv")
head(cp)
summary(cp)
library(forecast)


#To assess changing variance
cp[,2] %>% stl(s.window='periodic') %>% seasadj()->cpadj
cp[,3] %>% stl(s.window='periodic') %>% seasadj()->cpadj
cp[,4] %>% stl(s.window='periodic') %>% seasadj()->cpadj
cp[,5] %>% stl(s.window='periodic') %>% seasadj()->cpadj
cp[,6] %>% stl(s.window='periodic') %>% seasadj()->cpadj

par(mfrow=c(3,2))
ts.plot(cp[,2],ylab="Accor.S.A")
ts.plot(cp[,3],ylab="Thales.S.A")
ts.plot(cp[,4],ylab="Michelin")
ts.plot(cp[,5],ylab="Pernod.Ricard.S.A")
ts.plot(cp[,6],ylab="Renault.S.A")

#There is no evidence of changing variance

#Accor.S.A, Impossible to predict
cp[,2] %>% diff() %>% ggtsdisplay(main="Accor.S.A")
auto.arima(cp[,2],stepwise=FALSE)
cp[,2] %>% ets() %>% forecast() %>% autoplot()

#Thales.S.A, possible to predict
cp[,3] %>% diff() %>% ggtsdisplay(main="Thales.S.A")
fit<-auto.arima(cp[,3],stepwise=FALSE)
cp[,3] %>% ets() %>% forecast() %>% autoplot()

#Michelin, Impossible to predict
cp[,4] %>% diff() %>% ggtsdisplay(main="Michelin")
auto.arima(cp[,4],stepwise=FALSE)
cp[,4] %>% ets() %>% forecast() %>% autoplot()

#Pernod.Ricard.S.A, Impossible to predict
cp[,5] %>% diff() %>% ggtsdisplay(main="Pernod.Ricard.S.A")
auto.arima(cp[,5],stepwise=FALSE)
cp[,5] %>% ets() %>% forecast() %>% autoplot()

checkresiduals(fit)

#Renault.S.A, impossible to predict
cp[,6] %>% diff() %>% ggtsdisplay(main="Renault.S.A")
auto.arima(cp[,6],stepwise=FALSE)
cp[,6] %>% ets() %>% forecast() %>% autoplot()

Box.test(diff(cp[,2]),lag=10,type="Ljung-Box")
Box.test(diff(cp[,3]),lag=10,type="Ljung-Box")
Box.test(diff(cp[,4]),lag=10,type="Ljung-Box")
Box.test(diff(cp[,5]),lag=10,type="Ljung-Box")
Box.test(diff(cp[,6]),lag=10,type="Ljung-Box")

autoplot(forecast(fit, h=30), ylab="Closing price of Thales.S.A")
