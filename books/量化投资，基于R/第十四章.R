#第14章 资产配置和组合优化
#P140
library('fAssets')
LPP=as.timeSeries(data(LPP2005REC))[,1:5];head(LPP)
assetsLPM(LPP)#mu是LPM向量，#sigma是co-LPM矩阵，投资组合函数的输入数据

#P142,编译不过去

#P152
p<-2
N<-200
set.seed(1)
x<-matrix(rnorm(N*p),ncol=p)
y<-as.numeric((x[,1]^2+x[,2]^2)>1.4)
mydata<-data.frame(x=x,y=y)
mydata[1:3,]
library(nnet)#拟合神经网络
set.seed(3)
nnl<-nnet(y~x.1+x.2,data=mydata,entropy=T,size=3,decay=0,maxit=2000,trace=T)
#entropy极大似然，默认为最小二乘，size隐藏单元，decay参数重量衰变，默认为0
#，maxit最大迭代次数，默认为100，trace切换跟踪最优化，默认为T
yhat<-as.numeric(predict(nnl,type='class'))
par(mfrow=c(1,2))
plot(x,pch=19,col=c('red','blue')[y+1],main='actual labels',asp=1)
plot(x,col=c('red','blue')[(yhat>0.5)+1],pch=19,main='predicted labels',asp=1)
table(actual=y,predicted=predict(nnl,type='class'))

#P154
data(iris)
attach(iris)
head(iris)
## classification mode 分类方法
# default with factor response:  因子反应的缺失
model <- svm(Species ~ ., data = iris)
# alternatively the traditional interface: 可选择的传统界面
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y)
print(model)
summary(model)
# test with train data 测试机测试
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)
# Check accuracy: 检测精确度
table(pred, y)


# compute decision values and probabilities: 计算决定值和可能性
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]
# visualize (classes by color, SV by crosses): 作图
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
## try regression mode on two dimensions 在两个维度用回归
# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)
# estimate model and predict input values
m <- svm(x, y)
new <- predict(m, x)
# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)
## density-estimation
# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)
# traditional way:
m <- svm(X, gamma = 0.1)
# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)
# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)
# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)
# weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)