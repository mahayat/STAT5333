rm(list=ls())
graphics.off()
cat("\014")

##################cumulative logit models######################
politic=read.table("data_politic.txt",header=T)
politic[,2]=ifelse(politic[,2]=="D",1,0)

library(VGAM)
fit.cumu=vglm(Ideology~Affil, family=cumulative(reverse=F,parallel=T), data=politic)
summary(fit.cumu)

curve(exp(x*0.9745-2.469)/(1+exp(x*0.9745-2.469)),from=-8,to=8,col=1,lwd=2,xlab="x",ylab="PMF") #j=1
curve(exp(x*0.9745-1.474)/(1+exp(x*0.9745-1.474))-exp(x*0.9745-2.469)/(1+exp(x*0.9745-2.469)),add=T,col=2,lwd=2) #j=2
curve(exp(x*0.9745+0.237)/(1+exp(x*0.9745+0.237))-exp(x*0.9745-1.474)/(1+exp(x*0.9745-1.474)),add=T,col=3,lwd=2) #j=3
curve(exp(x*0.9745+1.070)/(1+exp(x*0.9745+1.070))-exp(x*0.9745+0.237)/(1+exp(x*0.9745+0.237)),add=T,col=4,lwd=2) #j=4
curve(1/(1+exp(x*0.9745+1.070)),add=T,col=5,lwd=2) #j=5
legend(-8,0.6,lty=c(1,1,1,1,1),col=c(1:5),c("P(j=1)","P(j=2)","P(j=3)","P(j=4)","P(j=5)"))

phat_cumu_1=predict(fit.cumu,newdata=list(Affil=1),type="response")
phat_cumu_0=predict(fit.cumu,newdata=list(Affil=0),type="response")
phat_cumu_1
phat_cumu_0
##################adjacent-categories logit models######################

fit.adj=vglm(Ideology~Affil, family=acat(reverse=F,parallel=T), data=politic)
summary(fit.adj)

phat_adj_1=predict(fit.adj,newdata=list(Affil=1),type="response")
phat_adj_0=predict(fit.adj,newdata=list(Affil=0),type="response")
phat_adj_1
phat_adj_0
##################continuation-ratio logit models######################

fit.cont=vglm(Ideology~Affil, family=cratio(reverse=F,parallel=T), data=politic)
summary(fit.cont)

phat_cont_1=predict(fit.cont,newdata=list(Affil=1),type="response")
phat_cont_0=predict(fit.cont,newdata=list(Affil=0),type="response")
phat_cont_1
phat_cont_0

###compare three model predictions
par(mfrow=c(1,2))
plot(c(1:5),phat_cumu_1,type="o",col="red",ylim=c(0.05,0.45),ylab="predict")
lines(c(1:5),phat_adj_1,type="o",col="blue")
lines(c(1:5),phat_cont_1,type="o",col="green")
legend(3.5,0.45,col=c("red","blue","green"),c("cumulative logit","adjacent-categories","continuation ratio"),lty=c(1,1,1))

plot(c(1:5),phat_cumu_0,type="o",col="red",ylim=c(0.05,0.45),ylab="predict")
lines(c(1:5),phat_adj_0,type="o",col="blue")
lines(c(1:5),phat_cont_0,type="o",col="green")





