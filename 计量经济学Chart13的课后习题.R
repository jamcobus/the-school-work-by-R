FERTIL2<-read.csv("D:/R/R-4.2.1/library/example/FERTIL23.csv")
###OLS估计
y<-FERTIL2$age^2
model_1<-lm(children~age+y+educ+electric+urban,data=FERTIL2)
summary(model_1)
#加入宗教哑变量
model_2<-lm(children~age+y+educ+electric+urban+spirit+protest+catholic,data=FERTIL2)
summary(model_2)
###残差和估计因变量回归
r <- residuals.lm(model_2)
a<-(model_2$fitted.values)^2
model_4<-lm(r*r~model_2$fitted.values + a)
summary(model_4)
###wls回归
wt<-1/lm(abs(model_2$residuals)~model_2$fitted.values)$fitted.values^2
wls_model_2<-lm(children~age+y+educ+electric+urban+spirit+protest+catholic, data=FERTIL2, weight=wt)
summary(wls_model_2)
r2<-residuals.lm(wls_model_2)
#检验哑变量联合显著
library(car)
linearHypothesis(model_4, c("a=0", "mode2_1$fitted.values=0"))
linearHypothesis(model_2, c('spirit=0','protest=0','catholic=0'))
linearHypothesis(wls_model_2, c('spirit=0','protest=0','catholic=0'))
#heteroscedasticity robust st.error
library(sandwich)
#calculate robust standard errors for model coefficients
coeftest(model_1, vcov = vcovHC(model_1, type = 'HC0'))
###F检验
qf(0.95,2,4358)
r <- residuals.lm(model_2)
a<-(model_2$fitted.values)^2
model_4<-lm(r*r~model_2$fitted.values + a)
summary(model_4)
library(car)
linearHypothesis(model_4, c("a=0", "model_2$fitted.values=0"))
hist(model_2$residuals)
qqnorm(model_2$residuals);qqline(model_2$residuals)

