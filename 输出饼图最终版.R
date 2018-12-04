a1=read.csv('C:/Users/zdc/Desktop/何一凡老师项目/何老师8.5/数据a1.csv',header=T)
b1=read.csv('C:/Users/zdc/Desktop/何一凡老师项目/何老师8.5/数据b1.csv',header=T)
setwd("C:/Users/zdc/Desktop/何一凡老师项目/何老师8.5/饼图手法")
for(i in 1:nrow(b1))
{
  b11=b1[i,-1]
  if(length(which(is.na(b11))) == 0 )
  {
    b12=b11
  }
  else
  {
    b12=b11[-which(is.na(b11))]
  }
  b13=as.numeric(b12)
  a11=as.matrix(a1[i,2:(ncol(b12)+1)])
  temp=which(b13==0,arr.ind = TRUE)
    if(length(temp)==0)
      {
        b14=b13
        a12=a11
      }
    else
      {
        b14=b13[-temp]
        a12=a11[-temp]
      }
  pct <- round(b14/sum(b14)*100)
  lbls <- paste(a12,pct,'%',sep = '')
  tempname <- paste('饼图手法',i,'图片','.jpg') #通过paste将文件名和后缀连接起来
  jpeg(file=tempname,width = 850,height = 525,quality = 400,)
  pie(b14,labels=lbls,col=rainbow(length(lbls)),main = a1[i,1])
  dev.off()
}