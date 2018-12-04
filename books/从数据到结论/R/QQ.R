data1=read.csv('C:/Users/hnjyzdc/Desktop/temp/本底值.csv',header=F)
parameter1=(ncol(data1)-ncol(data1)%%2)/2+1
parameter2name=paste('QQ.jpg')
setwd("C:/Users/hnjyzdc/Desktop/temp")
jpeg(file=parameter2name,width = 5000,height = 2500*parameter1,quality = 4000)
par(mfrow=c(parameter1,2))
for (i in 1:(ncol(data1))){
  qqnorm(data1[,i])
}
dev.off()

qqnorm(data1[,1])
qqline(data1[,1])