library(MASS)
w<-caith
chisq.test(w)#进行卡方检验
corresp(w,nf=2)
biplot(corresp(w,nf=2));abline(v=0,h=0,lty=3)