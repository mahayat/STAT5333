rm(list=ls())
graphics.off()
cat("\014")

set.seed(1234)
N=10000
n=200
simudata=rmultinom(N, n, c(0.2,0.2,0.3,0.3))
n11=simudata[1,]
n12=simudata[2,]
n21=simudata[3,]
n22=simudata[4,]
oddsratio=n11*n22/(n12*n21)
par(mfrow=c(2,1))
hist(oddsratio,col="red",breaks=30,main="Original scale")
hist(log(oddsratio),col="red",breaks=30,main="Log scale")


N=10000
n=200
simudata=rmultinom(N, n, c(0.1,0.1,0.4,0.4))
n11=simudata[1,]
n12=simudata[2,]
n21=simudata[3,]
n22=simudata[4,]
oddsratio=n11*n22/(n12*n21)
par(mfrow=c(2,1))
hist(oddsratio,col="red",breaks=30,main="Original scale")
hist(log(oddsratio),col="red",breaks=30,main="Log scale")
