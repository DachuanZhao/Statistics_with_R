DDA2<-function (TrnG1, TrnG2, TstG = NULL, var.equal = FALSE){
  if (is.null(TstG) == TRUE) TstG<-rbind(TrnG1,TrnG2)
  if (is.vector(TstG) == TRUE)  TstG<-t(as.matrix(TstG)) else if (is.matrix(TstG) != TRUE)
    TstG<-as.matrix(TstG)
  if (is.matrix(TrnG1) != TRUE) TrnG1<-as.matrix(TrnG1)
  if (is.matrix(TrnG2) != TRUE) TrnG2<-as.matrix(TrnG2); 
  nx<-nrow(TstG)
  blong<-matrix(rep(0, nx), nrow=1, byrow=TRUE, dimnames=list("blong", 1:nx))
  mu1<-colMeans(TrnG1); mu2<-colMeans(TrnG2) 
  if (var.equal == TRUE  || var.equal == T){
    S<-var(rbind(TrnG1,TrnG2))
    w<-mahalanobis(TstG, mu2, S)-mahalanobis(TstG, mu1, S)
  } else{
    S1<-var(TrnG1); S2<-var(TrnG2)
    w<-mahalanobis(TstG, mu2, S2)-mahalanobis(TstG, mu1, S1)
  }
  for (i in 1:nx){if (w[i]>0) blong[i]<-1 else blong[i]<-2}; blong
}
DDAM<-function (TrnX, TrnG, TstX = NULL, var.equal = FALSE){
  if ( is.factor(TrnG) == FALSE){
    mx<-nrow(TrnX); mg<-nrow(TrnG)
    TrnX<-rbind(TrnX, TrnG)
    TrnG<-factor(rep(1:2, c(mx, mg)))
  }
  if (is.null(TstX) == TRUE) TstX<-TrnX
  if (is.vector(TstX) == TRUE)  TstX<-t(as.matrix(TstX))
  else if (is.matrix(TstX) != TRUE)
    TstX<-as.matrix(TstX)
  if (is.matrix(TrnX) != TRUE) TrnX<-as.matrix(TrnX)
  nx<-nrow(TstX)
  blong<-matrix(rep(0, nx), nrow=1, dimnames=list("blong", 1:nx))
  g<-length(levels(TrnG))
  mu<-matrix(0, nrow=g, ncol=ncol(TrnX))
  for (i in 1:g)
    mu[i,]<-colMeans(TrnX[TrnG==i,]) 
  D<-matrix(0, nrow=g, ncol=nx)
  if (var.equal == TRUE  || var.equal == T){
    for (i in 1:g)
      D[i,]<- mahalanobis(TstX, mu[i,], var(TrnX))
  }
  else{
    for (i in 1:g)
      D[i,]<- mahalanobis(TstX, mu[i,], var(TrnX[TrnG==i,]))
  }
  for (j in 1:nx){
    dmin<-Inf
    for (i in 1:g)
      if (D[i,j]<dmin){
        dmin<-D[i,j]; blong[j]<-i
      }
  }
  blong
}
#案例5.1  距离判别法、Fisher判别法和Bayes判别分析法的比较
#  A. 距离判别法
#打开数据文件case5.1.xls,选取D1:H31区域,然后复制
case5.1=read.table("clipboard", header=T)  #将已复制到剪贴板中的数据读入R
attach(case5.1)   #把数据变量列名放入内存
classG1= case5.1 [1:11,2:5]   #选取训练样本1
classG2= case5.1 [12:27,2:5]  #选取训练样本2
newdata= case5.1 [28:30,2:5]  #选取待测样本用于后面判定
#source("DDA2.R")    #载入自编程序DDA2.R,假设它已放在当前工作目录中
DDA2(classG1,classG2)    #执行程序DDA2.R
DDA2(classG1, classG2, newdata)  #对待判样本newdata进行判定

#  B. Fisher判别法
#打开数据文件case5.1.xls,选取D1:H28区域,然后复制
case5.1=read.table("clipboard", header=T)  #将已复制到剪贴板中的数据读入R
attach(case5.1)   #把数据变量列名放入内存
library(MASS)     #加载MASS程序包
ld=lda(G~x1+x2+x3+x4)  #用线性判别函数lda( )作判别分析
ld  #输出判别结果
Z=predict(ld)  #用函数predict( )对原始数据进行回判分类
newG=Z$class  #单独列出回判分类
cbind(G, newG, Z$x)  #对比原始数据分类、回判分类结果和判别函数的值
tab=table(G, newG)  #列表比较
tab  #显示列表比较结果
sum(diag(prop.table(tab)))  #计算回判正确率
#再对三个待判样本进行判定:打开数据文件case5.1.xls,选取D1:H31区域,然后复制
case5.1=read.table("clipboard", header=T) #将已复制到剪贴板中的数据读入R
newdata=case5.1[28:30, 2:5]    #选取待测样本用于下面判别
predict(ld, newdata= newdata)   #对待测样本进行判别，输出判别结果

#  C. Bayes判别法
#和Fisher判别法类似，打开数据文件case5.1.xls,选取D1:H28区域,然后复制
case5.1=read.table("clipboard", header=T)  #将已复制到剪贴板中的数据读入R
attach(case5.1)   #把数据变量列名放入内存
library(MASS)     #加载MASS程序包
ld=lda(G~x1+x2+x3+x4, prior = c(11/27, 16/27))  #用先验概率进行线性判别
ld  #输出判别结果
Z=predict(ld)  #用函数predict( )对原始数据进行回判分类
newG=Z$class  #单独列出回判分类
cbind(G, newG, Z$x)  #对比原始数据分类、回判分类结果和判别函数的值
tab=table(G, newG)  #列表比较
tab  #显示列表比较结果
sum(diag(prop.table(tab)))  #计算回判正确率
#判别结果和距离判别法与Fisher判别法一致
#再对三个待判样本进行判定:打开数据文件case5.1.xls,选取D1:H31区域,然后复制
case5.1=read.table("clipboard", header=T) #将已复制到剪贴板中的数据读入R
newdata=case5.1[28:30, 2:5]    #选取待测样本用于下面判别
predict(ld, newdata= newdata)   #对待测样本进行判别，输出判别结果
