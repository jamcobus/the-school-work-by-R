#使用D最优选择的选择实验设计，牛奶特征和支付结果由预先实验得到，通过检验各选择的和购买结果的关系，得到消费者最关注的牛奶属性
library("openxlsx")
install.packages("openxlsx")
rm( list=ls() )
CE.Design<- read.csv("sample.csv", header = TRUE)
CE.Design
PRICE<-reshape(CE.Design[,c("SET", "O1", "O2", "O3","M1", "M2","A1","A2")], idvar = "SETS", varying
               = c("O1", "O2", "O3"),
               timevar = "Alt",
               times=c(1, 2, 3),
               sep = "",
               direction = "long")
attach(PRICE)
Price <- Price[ order(SET,Alt),]
detach(PRICE)
rm(list = ls())
path<-r"(C:\Users\LIKAMWA\Desktop)"
setwd(path)
library(openxlsx)
CE.DATA<-read.xlsx("sample.xlsx")
head(CE.DATA)
# install old version of mlogit package
require(devtools)
packageurl <- "https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz" #the link of the old package
#install.packages(packageurl, repos=NULL, type="source") #install the older version
library(mlogit)
mdata<-mlogit.data(data=CE.DATA, choice="chioce", shape = "long", varying = NULL,
                   alt.levels =c("A", "B", "NONE"), id.var = "SETS")
#conditional logit model
m1<-mlogit(chioce~0+PRICE+ NONE+ O1+O3+M1+A1+A2,data=mdata ) #0 indicate no intercept
summary(m1)
