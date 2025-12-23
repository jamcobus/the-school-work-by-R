M2<-read.csv("M2_ln.csv")
CPI<-read.csv("cnCPI.csv") #c(1,1,1)
library(vars)
library(forecast)
m2<-(M2$m2)#c(3,1,2)
VARselect(CPI[,2],M2[,2],lag.max = 10,type = "const")[["selection"]]
cname<-c("cpi","m2_ln")
c<-matrix(c(CPI[,2],M2[,2]),nrow=280,ncol=2,dimnames = list(1:280,cname));print(c)
var1<-VAR(c[,1:2],p=1,type="const")
serial.test(var1,lags.pt=10, type="PT.asymptotic")
irf.<-irf(var1,impulse = "m2_ln",response = c("cpi","m2_ln"),boot = TRUE,ci = 0.95, boot.runs=10)
plot(irf.)
#结果
	'''Portmanteau Test (asymptotic)

data:  Residuals of VAR object var1
Chi-squared = 128.87, df = 36, p-value = 2.237e-12'''
