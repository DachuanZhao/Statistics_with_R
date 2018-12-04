setwd('C:/Users/hnjyzdc/Desktop/从数据到结论')
w=read.table('bschool.txt',header = TRUE);pairs(w);cor(w);

a=lm(SalaPostMBA~SalaPreMBA,w);summary(a);
plot(SalaPostMBA~SalaPreMBA,w,pch=16);abline(a);
shapiro.test(a$residuals);  0#残差的正太性检验
a=lm(SalaPostMBA~.,w);summary(a)

w=read.table('artif2.txt',header = T);par(mfrow=c(1,3))
plot(y~x,w,main='ALL Data')
plot(y~x,w[w$u=='A',],main='u=A')
plot(y~x,w[w$u=='B',],main='u=B')
a=lm(y~x*u,w);summary(a)#*表示考虑交互作用

#简单线性回归(R Action)
fit <- lm(weight ~ height,data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,xlab = 'Height (in inches)',
     ylab='Weight (in pounds)')
abline(fit)

fit2<- lm(weight ~ height + I(height^2),data=women)#
summary(women)
plot(women$height,women$weight,xlab = 'Height (in inches)',
     ylab= 'Weight (in lbs)')
lines(women$height,fitted(fit2))

fit3 <- lm(weight ~ height + I(height^2) + I(height^3),data=women)
library(car)
scatterplot(weight ~ height,data=women,spread=F,1ty.smooth=2,pch=19,
            main='Women Age 30-39',xlab='Height (inches)'
            ,ylab='Weight (lbs.)')