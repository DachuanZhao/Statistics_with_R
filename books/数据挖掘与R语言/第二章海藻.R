#预测海藻的数量，即求f(x)
#install.packages('DMwR')
library(DMwR)
head(algae)

#基本分析
summary(algae)

library(car)
par(mfrow=c(1,2))
hist(algae$mxPH,prob=T,xlab = 'mxPH',main = 'Histogram of maxium pH value',
     ylim = 0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))#在X轴绘制变量的实际值，方便检测离群点
qqPlot(algae$mxPH,main='Normal QQ plot of maxium pH')

boxplot(algae$oPO4,ylab='Orthophosphate(oPO4)')
rug(jitter(algae$oPO4),side=2)
abline(h=mean(algae$oPO4,na.rm = T),lty=2)


