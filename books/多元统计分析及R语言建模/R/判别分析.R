d6.1=read.table('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模/d6.1.csv',
                header = T,sep = ',')
attach(d6.1)
plot(x1,x2);text(x1,x2,G,adj = -0.5)

library(MASS)
(ld<-lda(G~x1+x2))#线性判别函数
Z=predict(ld)
newG=Z$class
cbind(G,Z$x,newG)

tab=table(G,newG);tab
sum(diag(prop.table(tab)))

setwd('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模')
d6.2=read.table('d6.2.csv',header=T,sep=',')
attach(d6.2)
plot(Q,C);text(Q,C,G,adj=-0.8)
plot(Q,P);text(Q,P,G,adj=-0.8)
plot(C,P);text(C,P,G,adj=-0.8)
library(mvstats)
discrim.dist(cbind(Q,C,P),as.factor(G))#按马氏距离，只有16判错
discrim.dist(cbind(Q,C,P),as.factor(G),c(8.0,7.5,65))#结果属于第一类

#假定协方差相等的线性判别
library(MASS)
(ld=lda(G~Q+C+P))
W.x=predict(ld)$x
cbind(G,W=W.x,newG=ifelse(W.x<0,1,2))#7，13判错

#多总体判别
d6.3=read.table('d6.3.csv',header = T,sep=',')
attach(d6.3)
plot(Q,C);text(Q,C,G,adj=-0.8,cex = 0.75)
plot(Q,P);text(Q,P,G,adj=-0.8,cex = 0.75)
plot(C,Q);TEXT(C,P,G,adj=-0.8,cex = 0.75)
D1 = discrim.dist(cbind(Q,C,P),as.factor(G))#距离判别，异方差
cbind(G,D=t(D1[[1]]),t(D1[[2]]))
D2 = discrim.dist(cbind(Q,C,P),as.factor(G),var.equal = T)#juli判别，等方差
cbind(G,D=t(D2[[1]]),t(D2[[2]]))

library(MASS)#xianxingpanbie,dengfangcha
(ld=lda(G~Q+C+P))
Z = predict(ld)
newG = Z$class
cbind(G,Z$x,newG)
tab = table(G,newG);tab
diag(prop.table(tab,1))
sum(diag(prop.table(tab)))

qd=qda(G~Q+C+P)#er ci pan bie,yi fang cha
z=PREDICT(qd)
newG = Z$class
cbind(G,newG)
(tab=table(G,newG))
sum(diag(prop.table(tab)))