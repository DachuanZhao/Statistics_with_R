w<-read.table('clipboard',header=T)
d<-cmdscale(w,k=2,eig=T)#k为维度
d
sum(abs(d$eig[1:2]))/sum(abs(d$eig))
sum((d$eig[1:2])^2)/sum((d$eig)^2)
x=d$points[, 1]
y=d$points[, 2]
plot(x, y, xlim=c(min(x),max(x)),ylim=c(min(y),max(y)))  #根据两个特征向量的分量大小绘散点图
text(x, y, labels=row.names(eg10.3), adj=c(0, -0.5), cex=0.8) #将拟合点用行名标出