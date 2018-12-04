#安装'corrgram'、'car'#install 'corrgram'、'car'
install.packages("corrgram",dep = T)
install.packages("car",dep = T)

#载入数据#load the data
states<- as.data.frame(state.x77[,c('Murder','Population',
                                    'Illiteracy','Income','Frost')])

#相关性可视化#Relevance visualization
cor(states)

#图形1#figure one
library(car)
scatterplotMatrix(states,spread=F,lty.smooth=2,main='Scatter Plot Matrix')

#图形2#figure two
library(corrgram)
corrgram(states,order=T,lower.panel=panel.shade,
         upper.panel=panel.pie,text.panel=panel.txt)

##多元线性回归#Multiple Linear Regression
fit<-lm(Murder~Population+Illiteracy+Income+Frost,data=states)

#展示结果#show the results
summary(fit)

#模型诊断#Model diagnosis

#正态性#Normality
library(car)
qqPlot(fit,labels=row.names(states),
       id.method='identify',simulate=TRUE,main='QQ plot')

#独立性,DW检验#Independence，DW-test
library(car)
durbinWatsonTest(fit)

#同方差性,次幂接近1#homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)
