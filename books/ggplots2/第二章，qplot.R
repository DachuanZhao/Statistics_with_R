library(ggplot2)
set.seed(1410)
dsmall<- diamonds[sample(nrow(diamonds),100),]

qplot(carat,price,data= diamonds)#散点图

qplot(log(carat),log(price),data=diamonds)#带变换的散点图

qplot(carat,x*y*z,data=diamonds)#重量与体积图


qplot(carat,price,data=dsmall,colour=color)#颜色
qplot(carat,price,data=dsmall,shape= cut)#点的形状

qplot(carat,price,data=diamonds,alpha=I(1/10))#透明度
qplot(carat,price,data=diamonds,alpha=I(1/100))
qplot(carat,price,data=diamonds,alpha=I(1/200))

qplot(carat,price,data=dsmall,geom = c('point','smooth'))#散点图+拟合曲线
qplot(carat,price,data=diamonds,geom = c('point','smooth'),se=F)

qplot(carat,price,data = dsmall,geom = c('point','smooth'),span=0.2)#平滑度
qplot(carat,price,data = dsmall,geom = c('point','smooth'),span=1)

library(mgcv)
qplot(carat,price,data=dsmall,geom=c('point','smooth')
      ,method = 'gam',formula=y~s(x))
qplot(carat,price,data=dsmall,geom=c('point','smooth'),method='gam',
      formula=y~s(x,bs='cs'))#拟合方法，广义可加模型，大数据

library(splines)
qplot(carat,price,data=dsmall,geom=c('point','smooth')
      ,method='lm')#线性拟合
qplot(carat,price,data=dsmall,geom=c('point','smooth')
      ,method='lm',formula=y~poly(x,2))
#二次多项式
qplot(carat,price,data=dsmall,geom=c('point','smooth')
      ,method='lm',formula=y~ns(x,5))#自然样条，第二个参数代表波动度

qplot(color,price / carat,data=diamonds,geom='jitter',alpha=I(1/5))
qplot(color,price / carat,data=diamonds,geom='jitter',alpha=I(1/50))
qplot(color,price / carat,data=diamonds,geom='jitter',alpha=I(1/200))

qplot(carat,data=diamonds,geom='histogram')#直方图
qplot(carat,data=diamonds,geom='density',adjust=0.1)#密度曲线图
qplot(carat,data=diamonds,geom='histogram',binwidth = 1)
qplot(carat,data=diamonds,geom='histogram',binwidth = 0.1)
qplot(carat,data=diamonds,geom='histogram',binwidth = 0.01)#组距

qplot(carat,data=diamonds,geom = 'density',colour=color)
qplot(carat,data=diamonds,geom = 'histogram',fill=color)

qplot(color,data=diamonds,geom = 'bar')
qplot(color,data=diamonds,geom = 'bar',weight = carat) + 
  scale_y_continuous('carat')#按重量加权的条形图

qplot(date,unemploy / pop,data=economics,geom='line')#时间序列数据
qplot(date,uempmed,data=economics,geom='line')

year<-function(x) as.POSIXlt(x)$year+1900
qplot(unemploy/pop,uempmed,data=economics,geom=c('point','path'))
qplot(unemploy/pop,uempmed,data=economics,geom='path',colour=year(date))

qplot(carat,data=diamonds,facets = color~.,geom='histogram',binwidth=0.1
      ,xlim=c(0,3))
qplot(carat,..density..,data=diamonds,facets = color~.,geom='histogram'
      ,binwidth=0.1,xlim = c(0,3))

qplot(carat,price,data=dsmall,xlab='Price($)',ylab='Weight(carats)',
      main='Price-weight relationship')
qplot(carat,price/carat,data=dsmall,ylab=expression(frac(price,carat)),
      xlab='Weight(carats)',main='Small diamonds',xlim=c(.2,1))
qplot(carat,price,data=dsmall,log='xy')