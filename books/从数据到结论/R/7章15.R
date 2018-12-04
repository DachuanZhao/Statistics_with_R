setwd('C:/Users/hnjyzdc/Desktop/R/从数据到结论')
paste('第十五题')
w=read.table('acc.txt',header=T);
e15<- glm(Incidents~Tine+Machine,data=w,family = poisson)
summary(e15)
exp(coef(e15))
library(qcc)
qcc.overdispersion.test(w$Incidents,type='poisson')