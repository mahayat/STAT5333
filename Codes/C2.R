rm(list=ls())
graphics.off()
cat("\014")

hscrab=read.table("HorseshoeCrab_data.txt",header=T)
dim(hscrab)
head(hscrab)

# modeling the count data using Poisson regression

reg_pois=glm(satel~width,data=hscrab,family=poisson)
summary(reg_pois)
predict(reg_pois,data.frame(width=26),type="response")
logLik(reg_pois)
vcov(reg_pois)


# modeling the binary data (0/1) using logistic and probit regression

hscrab[,4]=ifelse(hscrab[,4]>0,1,0)
head(hscrab)

reg_logit=glm(satel~width,data=hscrab,family=binomial(link="logit"))
summary(reg_logit)
logLik(reg_logit)
vcov(reg_logit)
predict(reg_logit,data.frame(width=26),type="response")

reg_probit=glm(satel~width,data=hscrab, family=binomial(link="probit"))
summary(reg_probit)
predict(reg_probit,data.frame(width=26),type="response")
logLik(reg_probit)
vcov(reg_probit)
