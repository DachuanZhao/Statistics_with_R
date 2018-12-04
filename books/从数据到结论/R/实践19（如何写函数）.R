ss=function(n=100){z=2;
for(i in 2:n)if(any(i%%2:(i-1)==0)==F)z=c(z,i);return(z)}
fit(ss)#用来修改或者编写一个新的函数
ss()
t1=Sys.time()
ss(10000)
Sys.time()-t1
system.time(ss(10000))
#函数可以不写return，这时最后一个值为return的值