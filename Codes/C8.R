rm(list=ls())
graphics.off()
cat("\014")

#############linear by linear association model for ordinal variables##############
ncount=c(81,68,60,38,24,26,29,14,18,41,74,42,36,57,161,157)
TBC=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
PrSex=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
fit.llam=glm(ncount~factor(TBC)+factor(PrSex)+as.numeric(TBC*PrSex),family=poisson)
(fit.llam)