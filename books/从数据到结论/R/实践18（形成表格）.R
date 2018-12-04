library(MASS)
quine
attach(quine)
table(Age)
table(Sex,Age);tab=xtabs(~Sex+Age,quine);unclass(tab)
tapply(Days,Age,mean)
tapply(Days,list(Sex,Age),mean)
detach(quine)