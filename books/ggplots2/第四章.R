library(ggplot2)
p <- ggplot(diamonds,aes(carat,price,colour=cut))

p <- p + layer(geom='point')

layer(geom,geom_params,stat,stat_params,data,mapping,position)

p <- ggplot(diamonds,aes(x=carat))
p <- p+layer(
  geom='bar',
  geom_params=list(fill='steelblue'),
  stat='bin',
  stat_params=list(binwidth=2)
)
p

ggplot(msleep,aes(sleep_rem / sleep_total,awake)) + geom_point()
qplot(sleep_rem / sleep_total,awake,data=msleep)

qplot(sleep_rem/sleep_total,awake,data=msleep) + geom_smooth()
qplot(sleep_rem/sleep_total,awake,data=msleep,
      geom = c('point','smooth'))
ggplot(msleep,aes(sleep_rem/sleep_total,awake)) + geom_point() +
  geom_smooth()

p<- ggplot(msleep,aes(sleep_rem/sleep_total,awake))
summary(p)
p<- p+geom_point()
summary(p)

library(scales)
bestfit <- geom_smooth(method='lm',se=F,
                       colour=alpha('steelblue',0.5),
                       size=5)
qplot(sleep_rem,sleep_total,data=msleep)+bestfit
qplot(awake,brainwt,data=msleep,log='y')+bestfit
qplot(bodywt,brainwt,data=msleep,log='xy')+bestfit

p<-ggplot(mtcars,aes(mpg,wt,colour=cyl))+geom_point()
p
mtcars<-transform(mtcars,mpg=mpg^2)
p %+% mtcars

aes(x=weight,y=height,colour=sqrt(age))

p<-ggplot(mtcars)
summary(p)
p<-p + aes(wt,hp)
summary(p)

p<-ggplot(mtcars,aes(x=mpg,y=wt))
p + geom_point()

p + geom_point(aes(colour=as.factor(cyl)))
p + geom_point(aes(y = disp))

p<-ggplot(mtcars,aes(mpg,wt))
p + geom_point(colour='darkblue')
p + geom_point(aes(colour='darkblue'))

library(nlme)
p<-ggplot(Oxboys,aes(age,height,group=Subject)) + geom_line()
#group分组
p + geom_smooth(aes(group=1),method='lm',size=2,se=F)

boysbox<- ggplot(Oxboys,aes(Occasion,height)) + geom_boxplot()
boysbox + geom_line(aes(group = Subject),colour = '#3366FF')

xgrid<-with(df,seq(min(x),max(x),length = 50))
interp <- data.frame(
  x= xgrid
  y= approx(df$x,df$y,xout = xgrid)$y
  colour = approx(df$x,df$colour,xout= xgrid)$y
)
qplot(x,y,data=df,colour = colour,size = I(5)) + 
  geom_line(data=interp,size = 2)

ggplot(diamonds,aes(carat)) +
  geom_histogram(aes(y = ..density..),binwidth=0.1)
qplot(carat,..density..,data=diamonds,geom='histogram',
      binwidth=0.1)

#4.9
d<-ggplot(diamonds,aes(carat)) + xlim(0,3)
d + stat_bin(aes(ymax=..count..),binwidth = 0.1,geom = 'area')
d + stat_bin(
  aes(size = ..density..),binwidth = 0.1,
  geom = 'point',position = 'identity'
)
d + stat_bin(
  aes(y = 1,fill=..count..),binwidth = 0.1,
  geom = 'tile',position='identity'
)

require(nlme,quiet = T,warn.conflicts = F)
model<- lme(data=Oxboys,height ~ age,
            random = ~1+age | Subject)
oplot <- ggplot(Oxboys,aes(age,height,group = Subject)) +
  geom_line()
age_grid<-seq(from=-1,to=1,length=10)
subjects<-unique(Oxboys$Subject)
preds<-expand.grid(age=age_grid,subject=subjects)
preds$height<-predict(model,preds)