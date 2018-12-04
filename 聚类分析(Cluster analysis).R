setwd('C:/Users/hnjyzdc/Desktop/从数据到结论')
w=read.table('trans.txt',header = T)
set.seed(44);a=kmeans(w,5,nstart = 20,algorithm = 'Hartigan-Wong');
#k均值聚类,随机集合个数为20,其他备选算法为'Lloyd','Forgy'
sort(KM$cluster)

w1=w[a$clus!=2,];
d<-dist(w1,method = 'euclidean',diag=T,upper=F,p=2)
hh=hclust(d,method='ave')
plot(hh,labels=row.names(w1),xlab = 'Country or Area')#分层聚类

