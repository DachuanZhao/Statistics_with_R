setwd('C:/Users/hnjyzdc/Desktop/从数据到结论')
x=scan('gs.txt')
pbinom(sum(x>100),25,0.5)  
#符号检验，sum表示了大于100的个数，25为样本量，0.5为想要检验的概率，输出为p值
wilcox.test(x,m=100,alt='less')
#Wilcoxon符号秩检验，m为检验均值，alt表示单侧，双侧，分别为less(小于)，greater（大于），
install.packages('tseries')
library(tseries)
y = scan('run1.txt')
runs.test(factor(y))
#随机游程检验，用来检验哑变量是不是随机取值
z = scan('run2.txt')
runs.test(factor(z>median(z)))
#随机游程检验，用来检验一般变量是不是随机取
w = read.table('gdp.txt')
wilcox.test(w[w[,2]==1,1],w[w[,2]==2,1],paired=F,alt = 'less')
#两个独立样本的wilcoxon秩和检验，Mann-Whitney U 检验


