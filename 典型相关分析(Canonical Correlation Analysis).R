xy=read.table('',header=T)
ca=cancor(xy[,1:4],w[,5:7])
ca$xcoef
ca$ycoef
ca$xcenter
ca$ycenter
u=as.matrix(xy[,1:4]%*%ca$xcoef)
v=as.matrix(xy[,5:7]%*%ca$ycoef)
corcoef.test<-function(r,n,p,q,alpha=0.05){
  m<-length(r);Q<-rep(0,m);lambda<-1
  for (k in m:1){
    lambda<-lambda*(1-r[k]^2);
    Q[k]<- -log(lambda)
  }
  s<-0;i<-m
  for (k in 1:m){
    Q[k]<-(n-k+1-1/2*(p+q+3)+s)*Q[k]
    chi<-1-pchisq(Q[k],(p-k+1)*(q-k+1))
    if (chi>alpha){
      i<-k-1;break
    }
    s<-s+1/r[k]^2
  }
  i
}
corcoef.test(r=ca$cor,n=10,p=4,q=3) #相关系数检验(置信水平取0.05)#n为样本容量
par(mfrow=c(1,2)) #将画片横向分成两块
plot(u[,1],v[,1],xlab="u1",ylab="v1") #绘制第一对典型变量得分的散点图，x轴名称为u1,y轴名称为v1
abline(0,1) #在散点图上添加一条y等于x的线，以看散点分布情况
plot(u[,2],v[,2],xlab="u2",ylab="v2") #绘制第二对典型变量得分的散点图，x轴名称为u2,y轴名称为v2
abline(0,1) #在散点图上添加一条y等于x的线，以看散点分布情况

library(mvstats)
cancor.test(w[c1],w[c2])
library(CCA)
cc(w[c1],w[c2]) 
