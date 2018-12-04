x=c(171,175,159,155,152,158,154,164,168,166,159,164)
y=c(57,64,41,38,35,44,41,51,57,49,47,46)
plot(x,y)
lxy<-function(x,y){n=length(x);sum(x*y)-mean(x)*mean(y)*n}
(r=lxy(x,y)/sqrt(lxy(x,x)*lxy(y,y)))
cor(x,y)
cor.test(x,y)
(beta1=lxy(x,y)/lxy(x,x))
(beta0=mean(y)-beta1*mean(x))
plot(x,y);lines(x,beta0+beta1*x) 
SST=lxy(y,y)
SSR=beta1*lxy(x,y)
SSE=SST-SSR
MSR=SSR/1
MSE=SSE/(length(x)-2)
F=MSR/MSE
c(SST=SST,SSR=SSR,SSE=SSE,MSR=MSR,MSE=MSE,F=F)
Se=sqrt(MSE)
Sbeta1=Se/sqrt(lxy(x,x))
t1<-beta1/Sbeta1
ta1<-qt(1-0.05/2,length(x)-2)
c(Se=Se,Sbeta1=Sbeta1,t1=t1,ta1=ta1)

yX=read.table('C:/Users/hnjyzdc/Desktop/从数据到结论/d4.4.csv',sep=',',header = T)#d4.4 B1到F32
yX=yX[,-1]
attach(yX)
(fm <- lm(y~x1+x2+x3+x4))
lm(formula= y~x1+x2+x3+x4)
anova(fm)
pairs(yX)
cor(yX)

library(leaps)
varsel=regsubsets(y~x1+x2+x3+x4,data=yX)
result=summary(varsel)
data.frame(result$outmat,RSS=result$rss,R2=result$rsq)

fm=lm(y~x1+x2+x3+x4)
fm.step = step(fm,direction = 'forward')#向后剔除
fm.step = step(fm,direction = 'backward')#向前引入
fm.step = step(fm,direction = 'both')#逐步回归