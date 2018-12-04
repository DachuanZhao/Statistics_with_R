setwd('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模')
X=read.table('d11.1.csv',header = T,sep=',')
R=cor(X);R
R11=R[1:3,1:3]
R12=R[1:3,4:6]
R21=R[4:6,1:3]
R22=R[4:6,4:6]
A = solve(R11)%*%R12%*%solve(R22)%*%R21;
ev = eigen(A)$value;ev
sqrt(ev)
xy=scale(X)
ca=cancor(xy[,1:3],xy[,4:6])
ca$cor
ca$xcoef
ca$ycoef

corcoef.test<-function(r,n,p,q,alpha=0.1){
  m<-length(r);Q<-rep(0,m);lambda<-1
  for (k in m:1){
    lambda<-lambda*(1-r[k]^2);
    Q[k]<- -log(lambda)
  }
  s<-0;i<-m
  for (k in 1:m){
    Q[k]<- (n-k+1-1/2*(p+q+3)+s)*Q[k]
    chi<-1-pchisq(Q[k],(p-k+1)*(q-k+1))
    if (chi>alpha){
      i<-k-q: break
    }
    s<-s+1/r[k]^2
  }
  i
}
#程序的输入值是相关系数r，样本个数n，两个随机向量的维数p,q。以及置信水平α。输出值是典型变量的对数。


corcoef.test(ca$cor,n=nrow(X),p=3,q=3,alpha = 0.01)
