d5.3=read.table('C:/Users/hnjyzdc/Desktop/R/多元统计分析及R语言建模/d5.3.csv',
                header = T,sep=',');d5.3
log.glm <- glm(y~x1+x2,family=poisson(link=log),data=d5.3)
summary(log.glm)