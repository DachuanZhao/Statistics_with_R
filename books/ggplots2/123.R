library(ggplot2)
df<-data.frame(
  x=c(3,1,5),
  y=c(2,4,6),
  label=c('a','b','c')
)
p<- ggplot(df,aes(x,y)) + xlab(NULL) + ylab(NULL)
p + geom_point() + labs(title='geom_point')
p + geom_bar(stat='identity') +
  labs(title='geom_bar(stat=\'idetity\')')
p + geom_line() + labs(title='geom_line')
p + geom_area() + labs(title='geom_area')
p + geom_path() + labs(title='geom_path')
p + geom_text(aes(label = label)) + labs(title = 'geom_text')
p + geom_tile() + labs(title='geom_tile')
p + geom_polygon() + labs(title = 'geom_polygon')