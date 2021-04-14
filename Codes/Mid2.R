rm(list=ls())
graphics.off()
cat("\014")

GT_PR=read.table("Mid2.txt",header=T)
GT = GT_PR[,1]
PR = GT_PR[,2]


plot(GT,PR,pch=19,xlab="True y",ylab="Predicted y")


library(pROC)
ROC=roc(response=GT,predictor=PR,ci=T)
ROC

thresholds=ROC$thresholds
sens=ROC$sensitivities
spec=ROC$specificities

cbind(thresholds,sens,spec)
best=which.max(sens+spec)
optimalthres=thresholds[best]

plot(ROC,lty=4,lwd=2,col="red")
points(sens[best],spec[best],pch=17,col="blue",cex=2)