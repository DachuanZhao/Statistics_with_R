library(MASS)
(a=lda(Species~.,iris))

set.seed(1010)
samp=c(sample(1:50,25),sample(51:100,25),sample(101:150,25))
a=lda(Species~.,data=iris,subset=samp);
pred=predict(a,iris[-samp,])$class
table(iris[-samp,5],pred)