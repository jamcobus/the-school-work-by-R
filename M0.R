M0<-read.csv("填数据地址/monthM0data.csv")
plot.ts(M0$mt,xlab="t")
mt<-(M0$mt)
diff_m0<-diff(mt, lag = 1, differences = 1)
M02<-read.csv("填数据地址/disposedmonthM0data.csv")
mt2<-(M02$mt)
m0_fun<-lm(diff_m0~mt2)
m0_fun2<-
plot(m0_fun)
plot.ts(m0_fun$residuals,xlab="t")
acf(M0$mt,lag.max=14)
r = (-0.14362)
if (i<-0) {
  r = 1 
} else {
  for(i in 1:20){
    r[i+1] = r[1]^i
  }
}

plot(r,xlab = i)

r<-(m0_fun$residuals)
diff_r<-diff(r,lag = 1, differences = 1)
r_fun<-lm(diff_r~r[-1])
summary(r_fun)
plot(r_fun)
