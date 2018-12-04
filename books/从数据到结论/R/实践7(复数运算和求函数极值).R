paste('实践7(复数运算和求函数极值)')
(2+4i)^-3.5+(2i+4.5)*(-1.7-2.3i)/(2.6-7i)*(-4+5.1i)
(z <- complex(real=rnorm(10),im=rnorm(10)))
complex(re=rnorm(3),im=rnorm(3))
Re(z)
Im(z)
Mod(z)
Arg(z)
choose(3,2)
factorial(6)
#解方程
f=function(x) x^3-2*x-1
uniroot(f,c(0,2))
f= function(x) x^2+2*x+1
optimize(f,c(-2,2))