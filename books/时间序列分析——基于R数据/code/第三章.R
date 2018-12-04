#第3章 平稳时间序列分析
#P49
x1<-arima.sim(n=100,list(ar=0.8),sd=1)#n为序列长度，list中有ar,ma,order=c(p,d,q),sd为标准差
x3<-arima.sim(n=100,list(ar=c(1,-0.5)))
e<-rnorm(100)
x2<-filter(e,filter=-1.1,method='recursive')#recursive为ar,convolution为ma,filter为模型系数
x4<-filter(e,filter=c(1,0.5),method = 'recursive')
ts.plot(x1)
ts.plot(x2)
ts.plot(x3)
ts.plot(x4)

#P57
x1<-arima.sim(n=1000,list(ar=0.8))
x2<-arima.sim(n=1000,list(ar=-0.8))
x3<-arima.sim(n=1000,list(ar=c(1,-0.5)))
x4<-arima.sim(n=1000,list(ar=c(-1,-0.5)))
#均呈现拖尾性，平稳序列特征
acf(x1)#负指数衰减到0
acf(x2)#正负相间衰减到0
acf(x3)#余弦衰减到0
acf(x4)

#P60
pacf(x1)#1阶显著不为0,1阶后近似为0
pacf(x2)#1阶显著不为0,1阶后近似为0
pacf(x3)#2阶显著不为0,2阶后近似为0
pacf(x4)#2阶显著不为0,2阶后近似为0

#P64
x1<-arima.sim(n=1000,list(ma=-2))
x2<-arima.sim(n=1000,list(ma=-0.5))
x3<-arima.sim(n=1000,list(ma=c(-4/5,16/25)))
x4<-arima.sim(n=1000,list(ma=c(-5/4,25/16)))
acf(x1)
acf(x2)#自相关图完全相同，因为一个自相关图可对应多个平稳时间序列模型
acf(x3)
acf(x4)

