library(psych)
fa.parallel(USJudgeRatings[,-1],fa='pc'
            ,n.iter = 100,show.legend = FALSE,
            main = 'Scree plot with parallel analysis')#判断主成分个数，碎石图

library(psych)
pc <- principal(USJudgeRatings[,-1],nfactors = 1);pc
#成分载荷，公因子方差，成分唯一性

library(psych)
fa.parallel(Harman23.cor$cov,n.obs=302,fa='pc',n.iter = 100,
            show.legend = FALSE,main='Scres plot with parallel analysis')
library(psych)
PC <- principal(Harman23.cor$cov,nfactors=2,rotate = 'none');PC
rc <- principal(Harman23.cor$cov,nfactors=2,rotate = 'varimax');rc#主成分旋转
library(psych)
pc <- principal(USJudgeRatings[,-1],nfactora=1,score=TRUE)
head(pc$scores)
cor(USJudgeRatings$CONT,pc$scores)#计算主成分与变量间的相关系数
