X<-read.table('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模/d10.1.csv',
              header = T,sep=',')
row.names(X) <- X[, 1]
X<- X[, -1]
chisq.test(X)
library(MASS)
cal = corresp(X,nf=2)
biplot(cal);abline(v=0,h=0,lty=3)

setwd('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模')
Y=read.table('d10.2.csv',header=T,sep=',')
row.names(Y)=Y[,1]
Y=Y[,-1]
ca2 = corresp(Y,nf=2);ca2
biplot(ca2);abline(v=0,h=0,lty=3)