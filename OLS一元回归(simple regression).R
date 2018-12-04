#安装"car","minqa"#install "car","minqa"
install.packages('car',dep = T)
install.packages('minqa',dep = T)

#简单线性回归#sample ols linear regression
fit <- lm(weight ~ height,data=women)

#展示结果#show the results
summary(fit)

#回归系数的置信区间，置信度为0.05
#95% Confidence Interval of Regression Coefficient
confint(fit,level=0.05)

#画出回归方程和点#Draw the regression equation lines and points
plot(women$height,women$weight,xlab = 'Height (in inches)',
     ylab='Weight (in pounds)')#可视化
abline(fit)

#求预测值(pre_fit)、置信区间(con_int)、预测区间(pre_int),并绘图
#find the predicted value(pre_fit)、Confidence interval(con_int)、Prediction interval(pre_int) and plot
pre_fit<-predict(fit)
attach(women)
con_int<-predict(fit,as.data.frame(height),interval = 'confidence',level=0.05)
pre_int<-predict(fit,as.data.frame(height),interval = 'prediction',level=0.05)
pre<- data.frame(y=weight,pre_fit,lci=con_int[,2],uci=con_int[,3],
                 lpi=pre_int[,2],upi=pre_int[,3])
pre
x0<-seq(min(height),max(height))#绘制置信区间和预测区间图
con_int2<-predict(fit,as.data.frame(x0),interval = 'confidence',level=0.05)
pre_int2<-predict(fit,as.data.frame(x0),interval = 'prediction',level=0.05)
par(cex=0.7)
plot(height,weight,xlab='height',ylab = 'weight')
abline(fit,lwd=2)
lines(x0,con_int2[,2],lty=2,lwd=2,col='blue')
lines(x0,con_int2[,3],lty=2,lwd=2,col='blue')
lines(x0,pre_int2[,2],lty=3,lwd=2,col='red')
lines(x0,pre_int2[,3],lty=3,lwd=2,col='red')
legend(x='topleft',legend=c('回归线','置信区间','预测区间'),lty=1:3,lwd=2,
       cex=0.7)

#二次多项式回归#Secondary Polynomial Regression Model
fit2<- lm(weight ~ height + I(height^2),data=women)#
summary(fit2)
plot(women$height,women$weight,xlab = 'Height (in inches)',
     ylab= 'Weight (in lbs)')
lines(women$height,fitted(fit2))
