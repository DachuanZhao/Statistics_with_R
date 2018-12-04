library(ggplot2)
qplot(displ,hwy,data=mpg,colour=as.factor(mpg$cyl))

qplot(displ,hwy,data=mpg,facets = .~year) + geom_smooth()

p<-qplot(displ,hwy,data=mpg,coulor=factor(mpg$cyl))
summary(p)
save(p,file = 'plot.rdata')
load('plot.rdata')
ggsave('plot.png',width = 5,height = 5)