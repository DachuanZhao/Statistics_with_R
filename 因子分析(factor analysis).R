#精确到4位小数#set 4 decimal places
options(digits=4)

#安装、加载"psych","GPArotation"#install and load "psych","GPArotation"
install.packages("psych")
install.packages('GPArotation')
library(psych)

#加载数据#load the data
covariances <- ability.cov$cov

#计算相关阵#Calculate the correlation matrix
correlations <- cov2cor(covariances)

#画出碎石图，横线以上的点的个数为因子或者主成分的个数
#Draw a Scree plots and the number of points above the horizontal line is the number of factors or principal components
fa.parallel(correlations,fa='both',n.obs=112,
            main = 'Scree plots with parallel analysis')


#碎石图中可知因子有两个。因子分析中有多种方法得到因子,包括最大似然法(ml),主轴迭代法(pa),加权最小二乘法(wls),广义加权最小二乘法(gls),最小残差法(minres)
#Scree plots shows that there are two factors.There are several ways to get factors in factor analysis，including ml,pa,wls,gls and minres
fa <- fa(correlations,nfactors = 2,rotate = 'none',fm='pa',score=TRUE);fa

#正交旋转#Orthogonal rotation
fa.varimax <- fa(correlations,nfactors=2,rotate='varimax',fm='pa');fa.varimax

#斜交旋转#Oblique rotation
fa.promax <- fa(correlations,nfactors=2,rotate='promax',fm='pa');fa.promax

fsm <- function(oblique){
  if (class(oblique)[2]=='fa' & is.null(oblique$Phi)){
    warning('Object doesn’t look like oblique EFA')
  } else {
    P <- unclass(oblique$loading)
    F <- P%*% oblique$Phi
    colnames(F)<- c('PA1','PA2')
    return(F)
  }
}

#得到因子相关矩阵#Get the factor correlation matrix
fsm(fa.promax)

#绘制图形#plot the figure
factor.plot(fa.promax,labels=rownames(fa.promax$loadings))

#得到标准化的权重#obtain the standardized weight
fa.promax$weights


