data(Affairs,package='AER')
summary(Affairs)
Affairs$ynaffair[Affairs$affairs >0] <- 1
Affairs$ynaffair[Affairs$affairs ==0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,levels=c(0,1),labels=c('No','Yes'))
table(Affairs$ynaffair)
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation + rating , 
                  data =Affairs,family=binomial())
summary(fit.full)#结果中性别，是否有孩子，学历和职业对方程贡献都不显著
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness +
rating,data=Affairs,family=binomial())
summary(fit.reduced)
exp(coef(fit.reduced))#优势比

anova(fit.reduced,fit.full,test='Chisq')