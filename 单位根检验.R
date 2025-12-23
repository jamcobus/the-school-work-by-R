#简单进行的CPI单位根检验，下附结果
library(tseries)
library(forecast)
library(urca)
library(vars)
CPI<-read.csv("C:/Users/LIKAMWA/Desktop/作业/金融计量/cnCPI.csv")
cncpi<-(CPI$CPI.)
diff_cpi<-diff(cncpi, lag = 1, differences = 1)
cpi_fun<-lm(diff_cpi~cncpi[-1])
summary(cpi_fun)
summary(ur.df(y=cncpi, lags=1,type="trend",selectlags="BIC"))
summary(ur.df(y=cncpi, lags=1,type="drift",selectlags="BIC"))
summary(ur.df(y=cncpi, lags=1,type="none",selectlags="BIC"))

ers.gnp <- ur.ers(y=cncpi, type="DF-GLS", model="const", lag.max=1)
summary(ers.gnp)

'''结果如下'''
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.005865 on 275 degrees of freedom
Multiple R-squared:  0.03105,	Adjusted R-squared:  0.024 
F-statistic: 4.406 on 2 and 275 DF,  p-value: 0.01308


Value of test-statistic is: -2.8054 3.9359 

Critical values for test statistics: 
      1pct  5pct 10pct
tau2 -3.44 -2.87 -2.57
phi1  6.47  4.61  3.79

> summary(ur.df(y=cncpi, lags=1,type="none",selectlags="BIC"))

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression none 


Call:
lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0256396 -0.0026831  0.0007684  0.0038274  0.0207665 

Coefficients:
           Estimate Std. Error t value Pr(>|t|)  
z.lag.1    -0.02240    0.01230  -1.821   0.0698 .
z.diff.lag  0.06820    0.05962   1.144   0.2536  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.005902 on 276 degrees of freedom
Multiple R-squared:  0.01514,	Adjusted R-squared:  0.008006 
F-statistic: 2.122 on 2 and 276 DF,  p-value: 0.1218


Value of test-statistic is: -1.8206 

Critical values for test statistics: 
      1pct  5pct 10pct
tau1 -2.58 -1.95 -1.62

> 
> ers.gnp <- ur.ers(y=cncpi, type="DF-GLS", model="const", lag.max=1)
> summary(ers.gnp)

############################################### 
# Elliot, Rothenberg and Stock Unit Root Test # 
############################################### 

Test of type DF-GLS 
detrending of series with intercept 


Call:
lm(formula = dfgls.form, data = data.dfgls)

Residuals:
       Min         1Q     Median         3Q        Max 
-0.0259808 -0.0027336  0.0006741  0.0036346  0.0206453 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
yd.lag       -0.04320    0.01691  -2.555   0.0112 *
yd.diff.lag1  0.07909    0.05959   1.327   0.1856  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.005868 on 276 degrees of freedom
Multiple R-squared:  0.02634,	Adjusted R-squared:  0.01929 
F-statistic: 3.734 on 2 and 276 DF,  p-value: 0.02512


Value of test-statistic is: -2.5549 

Critical values of DF-GLS are:
                 1pct  5pct 10pct
critical values -2.57 -1.94 -1.62

