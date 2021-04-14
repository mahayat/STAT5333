rm(list=ls())
graphics.off()
cat("\014")
##### Example 6.2 #####
library(VGAM)
gator=read.table("alligator_foodchoice.txt", header=TRUE)
head(gator)
# gator$food = relevel(gator$food, ref="F")
# head(gator)

fit1=vglm(food~length, data=gator, family=multinomial)
summary(fit1)

# predict(fit1,type="response")

