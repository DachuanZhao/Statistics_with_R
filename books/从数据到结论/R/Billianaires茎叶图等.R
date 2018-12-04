#The second class
setwd("C:/Users/hnjyzdc/Desktop/从数据到结论")
v <- read.table('Billianaires.txt',sep=',',header = T,na.strings ='-')
par(mfrow=c(1,2))
hist(v$Age,main='',xlab = 'Age')
hist(v$Net.Worth,main='worth',xlab = 'Net Worth') #1 histogram end

w=v[v[,6]=='United States'|v[,6]=='China'|v[,6]=='Japan',]
w[,6]=as.character(w[,6])
boxplot(Age~Country.of.Citizenship,w) #2 boxplot end

stem(v[v[,6]=='China',4]) #3 stem-and-leaf plots end

setwd("C:/Users/hnjyzdc/Desktop/从数据到结论")
v = read.table('g100.txt',sep=',',header = T)
plot(v$Assets,v$Sales,pch=1,col=1,xlab='Assets(Billion $)',
ylab='Sales(Billion $)',ylim=c(0,600),xlim=c(-100,3000),cex=log(v$Profits))
title('Global 100 Companys’Assets,Sales and log Profits(size of points)')
idetify(v$Assets,v$Sales,labels=v$company)  #4scatter plot end

setwd("C:/Users/hnjyzdc/Desktop/从数据到结论")
w=read.table('global2000.txt',sep=',',header = T)
ws=sort(table(w$Country),de=T)
pie(ws[1:10])
title('Number of Companies Among top 10')

par(mfrow=c(2,1))
barplot(ws[1:10],cex.names=.8,main='')
barplot(ws[1:10])
