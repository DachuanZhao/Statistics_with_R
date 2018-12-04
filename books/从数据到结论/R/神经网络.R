set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,
           maxit=1000,linout=T,trace=F)
norm.preds<- predict(nn,norm.data[1001,2000,])
preds <- unscale(norm.preds,norm.data)

library(nnet)
library(neuralnet)

data(iris)
X <- iris[, 1:4]
y <- iris$Species

output <- class.ind(y)
colnames(output) <- paste0('out.', colnames(output))
output.names <- colnames(output)
input.names <- colnames(X)
train <- cbind(iris, output)

f <- paste(paste(output.names, collapse = '+'), '~',
           paste(input.names, collapse = '+'))

net <- neuralnet(f, train, hidden = rep(3, 2))

plot(net)