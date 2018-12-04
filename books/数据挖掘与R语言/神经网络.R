set.seed(1234)
library(nnet)
norm.data<-scale(Tdata.train)
nn<-nnet(Tfrom,norm.data[1:1000,],size = 10,decay = 0.01,
         maxit = 1000,linout = T,trace = F)
norm.preds<-predict(nn,norm.data[1001:2000,])
preds <- unscale(norm.preds,norm.data)