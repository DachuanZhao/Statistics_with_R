setwd('C:/Users/hnjyzdc/Desktop/从数据到结论')
w=read.table('HEcolor.txt',header = T)
ftable(xtabs(Freq~.,w),row.vars=1,col.var=2:3)
#三维列联表简化为二维列联表:xlabs(Freq~Hair+Eye,w)
chisq.test(xtabs(Freq~Hair+Eye,w))

library(MASS);a=loglm(Freq~Hair+Eye,w);a$para

m=read.table('acc2.txt',header = T)
m$Machine = factor(m$Machine);m$Person=factor(m$Person)
a=glm(Incidents~Time+Machine+Person,family='poisson',data=m)
summary(a)