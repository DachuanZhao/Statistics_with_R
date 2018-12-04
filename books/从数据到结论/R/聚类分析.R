setwd('C:/Users/hnjyzdc/Desktop/从数据到结论')
w=read.table('trans.txt',header = T)
set.seed(44);a=kmeans(w,5);a#k均值聚类

w1=w[a$clus!=2,];hh=hclust(dist(w1),'ave')
plot(hh,labels=row.names(w1),xlab = 'Country or Area')#分层聚类

w=read.table