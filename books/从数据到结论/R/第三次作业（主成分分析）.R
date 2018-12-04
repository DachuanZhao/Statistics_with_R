#第三题
setwd('C:/Users/hnjyzdc/Desktop/R/从数据到结论')
w1=read.table('bschool.txt',header = T)
PCA=princomp(w1,cor=F)
PCA$loadings
screeplot(PCA,type='lines')
PCA$scores
library(mvstats)
y=princomp.rank(PCA,m=2)
y<-as.data.frame(y)
y[order(y$rank),]
princomp.rank(PCA,m=2,plot=T)
(FAO1=factanal(w1,1,rotation = 'none'))
(FAO2=factanal(w1,1,rotation = 'varimax'))

#a=PCA$scores
#a[,1:2]
#b=eigen(cor(w1))$values[1:2]
#c=a[,1:2]%*%b/sum(b)
#c说明c即为princomp.rank中的PC(得分)

#第四题
setwd('C:/Users/hnjyzdc/Desktop/R/从数据到结论')
w2=read.table('student.txt',header = T)
PCA=princomp(w2,cor=F)
PCA$loadings
screeplot(PCA,type='lines')
PCA$scores
library(mvstats)
y=princomp.rank(PCA,m=2)
y<-as.data.frame(y)
y[order(y$rank),]
princomp.rank(PCA,m=2,plot=T)

(FAO1=factanal(w2,2,rotation = 'none'))
(FAO2=factanal(w2,2,rotation = 'varimax'))
FAC=factanal(w2,2,scores = 'regression')
plot(FAC$scores,type='n',xlab = '文科',ylab='理科')
text(FAC$scores,row.names(w2),cex=0.5)
