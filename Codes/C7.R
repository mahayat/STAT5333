rm(list=ls())
graphics.off()
cat("\014")

###############Loglinear model###########################
###############by glm##############################
ncount=c(911,538,44,456,3,43,2,279)
factorA=c("Y","Y","Y","Y","N","N","N","N")
factorC=c("Y","Y","N","N","Y","Y","N","N")
factorM=c("Y","N","Y","N","Y","N","Y","N")
fit.glm<-glm(ncount~(factorA+factorC+factorM)^2, family=poisson)
summary(fit.glm)

###############by loglin##########################
ACMdata=read.table("/users/qingyangzhang/desktop/ACMdata.txt",header=T)
table(ACMdata)
fit.12.13.23=loglin(table(ACMdata),margin=list(c(1, 2), c(2,3), c(1, 3)),param=T,fit=T)
fit.12.13.23

fit.123=loglin(table(ACMdata),margin=list(c(1, 2, 3)),param=T,fit=T)
fit.123

fit.12.23=loglin(table(ACMdata),margin=list(c(1, 2), c(2,3)))
fit.12.23

fit.12.13=loglin(table(ACMdata),margin=list(c(1, 2), c(1,3)))
fit.12.13

fit.13.23=loglin(table(ACMdata),margin=list(c(1, 3), c(2,3)))
fit.13.23

fit.1.2.3=loglin(table(ACMdata),margin=list(1, 2, 3))
fit.1.2.3


