library(MASS)
w<-caith
chisq.test(w)#进行卡方检验,p<0.05
corresp(w,nf=2)
biplot(corresp(w,nf=2),cex=0.7);abline(v=0,h=0,lty=3)