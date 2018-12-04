data1=read.csv('C:/Users/hnjyzdc/Desktop/temp/本底值.csv',header=F)
data2=read.csv('C:/Users/hnjyzdc/Desktop/temp/一周后.csv',header=F)
output1=rep(0,length(aa))
for (i in 1:ncol(aa)){
  output1[i]=t.test(data1[,i],data2[,i],paired=TRUE)$p.value
}
setwd("C:/Users/hnjyzdc/Desktop/temp")
write.table(output1, "excel.csv")


