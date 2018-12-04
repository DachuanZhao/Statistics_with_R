nk=read.table('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模/d5.1.csv',
              header = T,sep=',')
attach(nk)#解析变量
y<- cbind(k,n-k)
glm.logit<- glm(y~x,family = binomial)
summary(glm.logit)

pre = predict(glm.logit,data.frame(x=3.5))
p=exp(pre)/(1+exp(pre));p

x1= -glm.logit$coef[[1]]/glm.logit$coef[[2]]#p=50%

d=seq(0,5,len=100);d
pre <- predict(glm.logit,data.frame(x=d))
p = exp(pre)/(1+exp(pre))
y1 = k/n
plot(x,y1);lines(d,p)