options(digits=2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances);correlations

library(psych)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
fa.parallel(correlations,n.obs=112,fa='both',n.iter = 100,
            main = 'Scree plots with parallel analysis')
#碎石图,红线拐角之前或特征值大于0
fa <- fa(correlations,nfactors = 2,rotate = 'none',fm='pa',score=TRUE);fa#因子分析
fa.varimax <- fa(correlations,nfactors=2,rotate='varimax',fm='pa');fa.varimax
#正交旋转
fa.promax <- fa(correlations,nfactors=2,rotate='promax',fm='pa');fa.promax
#斜交旋转
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
fsm(fa.promax)#得到因子载荷矩阵
factor.plot(fa.promax,labels=rownames(fa.promax$loadings))#绘制图形

fa.24tests <- fa(Harman74.cor$cov,nfactors=4,rotate='promax')#练习



