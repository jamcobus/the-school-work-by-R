gpa1<-read.csv("D:/R/R-4.2.1/library/example/GPA1.csv")
###OLS估计
model_1<-lm(colGPA~hsGPA+ACT+skipped+PC,data=gpa1)
summary(model_1)
hist(model_1$residuals)
qqnorm(model_1$residuals);qqline(model_1$residuals)
###残差和估计因变量回归
r <- residuals.lm(model_1)
y<-(model_1$fitted.values)^2
model_4<-lm(r*r~model_1$fitted.values + y)
summary(model_4)
###wls回归
wt<-1/lm(abs(model_1$residuals)~model_1$fitted.values)$fitted.values^2
wls_model_1<-lm(colGPA~hsGPA+ACT+skipped+PC,data = gpa1, weight=wt)
summary(wls_model_1)
r2<-residuals.lm(wls_model_1)
sum(wt>=1)
#heteroscedasticity robust st.error
library(sandwich)
#calculate robust standard errors for model coefficients
coeftest(wls_model_1, vcov = vcovHC(wls_model_1, type = 'HC0'))
###怀特检验,下面应该写错了，跑不起来
###library(lmtest)
###bptest(model_1,~hsGPA*ACT*skipped*PC + I(hsGPA^2) + I(ACT^2) + I(skipped^2) + I(PC^2),data=gpa1)
#x<-sum(hsGPA,ACT,skipped,PC)



