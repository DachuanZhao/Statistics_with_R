install.packages('multcomp',dep = T)

library(multcomp)
attach(cholesterol)
head(cholesterol)

#样本均值#the mean of the sample
aggregate(response,by=list(trt),FUN=mean)

#样本方差#the vacience of the sample
aggregate(response,by=list(trt),FUN=sd)

#单因素方差分析#analysis of variance
fit <- aov(response ~trt)
#fitlm <- lm(response ~trt)

#展示结果#show the results
summary(fit);#summary(fitlm)

#画均值图#draw a mean plot
library(gplots)
plotmeans(response ~ trt,xlab='Treatment',ylab='Response',
          main='Mean Plot\nwith 95%CI')
detach(cholesterol)

#多重比较1#multiple comparisons(the first method)
pairwise.t.test(response,trt,p.adjust.method='holm')
#可选参数holm,hochberg,hommel,bonferroni等
#Optional parameters are "holm","hochberg","hommel","bonferroni"

#多重比较2推荐使用#multiple comparisons(the second method)
#TUkeyHSD与HH包存在兼容问题
#There is an incompatibility problem between "TUkeyHSD" and "HH"
detach('package::HH')
TukeyHSD(fit)
par(las=2)#旋转坐标轴标签
par(mar=c(5,8,4,2))#增大左边界的面积
plot(TukeyHSD(fit))#包含0的不显著

#多重比较3#multiple comparisons(the Third method)
library(multcomp)
par(mar=c(5,4,6,2))#增大了顶部边界的面积
tuk <- glht(fit,linfct=mcp(trt='Tukey'))
plot(cld(tuk,level=.05),col='lightgrey')#设置了显著性水平
#结果分析，有相同字母的组，差异不显著

#模型诊断#Model diagnosis
#正态检验#Normality
library(car)
qqPlot(lm(response ~ trt,data=cholesterol),
       simulate = T,main = 'Q-Q Plot',labels=F)

#方差齐性#homoscedasticity
bartlett.test(response ~ trt,data=cholesterol)

#离群点检测#Outlier detection
library(car)
outlierTest(fit)


#双因素方差分析#Double factor variance analysis
library(multcomp)
attach(ToothGrowth)
head(ToothGrowth)
aggregate(len,by=list(supp,dose),FUN=mean)
aggregate(len,by=list(supp,dose),FUN=sd)
fit1<-aov(len~supp+dose);summary(fit1)
fit2<-aov(len~supp*dose);summary(fit2)
library(gplots)#均值图
plotmeans(len)
detach(cholesterol)

#可视化1#Visualization(the first method)
interaction.plot(dose,supp,len,type='b',col=c('red','blue'),pch=c(16,18),
                 main='Interaction between Dose and Supplement Type')

#可视化2#推荐使用#Visualization(the second method)
library(gplots)
plotmeans(len ~ interaction(supp,dose,sep=' '),connect=list(c(1,3,5),c(2,4,6)),
          col=c('red','darkgreen'),main='Interaction Plot with 95% CIs',
          xlab='Treatment and Dose Combination')

#可视化3#Visualization(the third method)
library(HH)
interaction2wt(len~supp*dose)



