#P21
library(xts)
x1<-xts(rnorm(100),seq(as.POSIXct('2000-01-01'),length=100,by='day'))
x1[1:5]
x2<-xts(rnorm(100),seq(as.POSIXct('2000-01-01 13:00'),length=100,by='min'))
x2[1:4]
x3<-xts(rnorm(3),as.Date(c('2005-01-01','2005-01-10','2005-01-12')))
x3

x1[as.POSIXct('2010-01-04')]
x1['2000-01-05']
x1['20000105']
x1['2000-04']
x1['2000-03-27/']
x1['2000-02-06/2000-03-03']
x1['/20000103']

mts.vals<-matrix(round(rnorm(25),2),5,5)
colnames(mts.vals)<-paste('ts',1:5,sep='')
mts<-xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04','2003-01-05','2003-01-06','2003-02-16')))
mts
mts['2003-01',c('ts2','ts5')]
index(mts)
coredata(mts)

#p29

LENGTH<-600000
myformat<-list(date='',exdate='',cp='',strike=0,bid=0,ask=0,volume=0,impvolat=0,
               id=0,cjadj=0,ss=0)
date=character(LENGTH)
exdate=character(LENGTH)
price=numeric(LENGTH)
strike=numeric(LENGTH)
f<-file('301328226.csv')
k
titles<-readLines(f,n=1) #skip the first line
i<-1
repeat{
  b<-scan(f,what=myformat,sep=',',nlines=1,quiet=T)
  if(length(b$date)==0)break
  if(b$cp=='P'){
    date[i]<-b$date
    exdate[i]<-b$exdate
    price[i]<-(b$bid+b$ask)/2
    strike[i]<-b$strike
    i<-i+1
  }
}
close(f)


