#有效数字
options(digits=10)
setwd("")
data <- read.table('yxy.csv',sep=',',header = TRUE)
PCAdata <- princomp(data,cor=F);pri#协方差矩阵
screeplot(pri,type='lines',main='碎石图');abline(h=1)#画出碎石图，确定保留主成分个数
summary(PCAdata,loadings = T)#查看结果
priscores <-pri$scores[,c(seq(1,length=3,by=1))];#得出前三个主成分得分
coef<-pri$sdev[c(seq(1,length=3,by=1))]#得出主成分得分的标准差，其为平方根的系数
coef<-coef^2/sum(coef^2)#算出系数
totalscores<-apply(priscores*coef,1,sum)
rankscores<-rank(totalscores)
finaltable<-cbind(priscores,totalscores,rankscores)
write.table(finaltable,file='finaltable.csv',sep=',',na='NA',row.names=T,col.names = T)

cortable<-cor(data)
write.table(a,file='cortable.csv',sep=',',na='NA',row.names=T,col.names = T)
#cumulative
