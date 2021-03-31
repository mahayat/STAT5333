rm(list=ls())
graphics.off()
cat("\014")

hscrab=read.table("HorseshoeCrab_data.txt",header=T)
hscrab[,4]=ifelse(hscrab[,4]>0,1,0)

reg_logit=glm(satel~width,data=hscrab,family=binomial(link="logit"))
summary(reg_logit)
prob=predict(reg_logit,type="response")
cbind(prob,hscrab[,4])
plot(hscrab[,4],prob,pch=19,xlab="True y",ylab="Predicted y")
cor(hscrab[,4],prob)


cutoff=0.5
sens=sum(prob>cutoff & hscrab[,4]==1)/sum(hscrab[,4]==1)
spec=sum(prob<cutoff & hscrab[,4]==0)/sum(hscrab[,4]==0)
sens
spec

#cutoff=0.642
cutoff=sum(hscrab[,4])/length(hscrab[,4])
sens=sum(prob>cutoff & hscrab[,4]==1)/sum(hscrab[,4]==1)
spec=sum(prob<cutoff & hscrab[,4]==0)/sum(hscrab[,4]==0)
sens
spec


library(pROC)
ROC=roc(response=hscrab[,4],predictor=prob,ci=T)
ROC
 
thresholds=ROC$thresholds
sens=ROC$sensitivities
spec=ROC$specificities

cbind(thresholds,sens,spec)
best=which.max(sens+spec)
optimalthres=thresholds[best]

plot(ROC,lty=4,lwd=2,col="red")
points(sens[best],spec[best],pch=17,col="blue",cex=2)


## Calculate ROC curve and AUC without R package
thresholds=seq(1,0,-0.001)
n=length(thresholds)
sens=numeric(n)
spec=numeric(n)
for(i in 1:n){
sens[i]=sum(prob>thresholds[i] & hscrab[,4]==1)/sum(hscrab[,4]==1)
spec[i]=sum(prob<thresholds[i] & hscrab[,4]==0)/sum(hscrab[,4]==0)
}

plot(1-spec,sens,type="o",xlim=c(0,1),ylim=c(0,1),lty=4,lwd=2,col="red")
abline(0,1,col="green")

AUC=0
for(i in 1:(n-1)){
trapezoid=(sens[i]+sens[i+1])*(spec[i]-spec[i+1])/2
AUC=AUC+trapezoid
}

AUC








