setwd('C:/Users/hnjyzdc/Desktop/R/从数据到结论')
w1=read.table('who.txt',sep=',',header = T)
w1=t(w1)
hh1=hclust(dist(w1),'ave')
plot(hh1,labels=row.names(w1))

setwd('C:/Users/hnjyzdc/Desktop/R/从数据到结论')
library(Hmisc)
w2=spss.get('聚类分析（商厦评分）.sav',use.value.labels=T)
hh2=hclust(dist(w2),'ave')
plot(hh2,labels=row.names(w2))

setwd('C:/Users/hnjyzdc/Desktop/R/从数据到结论')
library(Hmisc)
w3=spss.get('因子分析（各地区年平均收入）.sav',use.value.labels = T)
row.names(w3)=w3[,1]
w3=w3[,2:8]
w3=w3[complete.cases(w3),]
PCA=princomp(w3,cor=F)
screeplot(PCA,type='lines')
(FAO1=factanal(w3,2,rotation = 'none'))
(FAO2=factanal(w3,2,rotation = 'varimax'))