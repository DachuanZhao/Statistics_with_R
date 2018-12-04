#读取数据（数据集为“城镇消费.csv”）

a=read.table("C:/Users/hnjyzdc/Desktop/从数据到结论/城镇消费.csv",header=T,
             sep=',');a;
names(a);attach(a);
a1=a[1:5,];a1;
a2=a[6:18,];a2;
a3=a[20:29,];a3;

#计算协方差阵、逆矩阵、和各组均值
v=cov(a[,3:10]);v;
v_1=solve(v);v_1;
#m=mean(a[,3:10]);m;
#m1=mean(a1[3:10]);m1;
#m2=mean(a2[3:10]);m2;
#m3=mean(a3[3:10]);m3;
m=apply(a[,3:10],2,mean);m
m1=apply(a1[3:10],2,mean);m1;
m2=apply(a2[3:10],2,mean);m2;
m3=apply(a3[3:10],2,mean);m3;

#计算与三组的距离
d1=c(1:31);
i=1
for(i in 1:31)
{if(i<=31)
  d1[i]=as.matrix(a[i,3:10]-m1)%*%matrix(v_1,8,8)%*%matrix(t(a[i,3:10]-m1),8,1)
d1[i];
i=i+1}
d1;

d2=c(1:31);
j=1;
for(j in 1:31)
{if(j<=31)
  d2[j]=as.matrix(a[j,3:10]-m2)%*%matrix(v_1,8,8)%*%matrix(t(a[j,3:10]-m2),8,1)
d2[j];
j=j+1
}
d2;

d3=c(1:31);
k=1
for(k in 1:31)
{
  if(k<=31)
    d3[k]=as.matrix(a[k,3:10]-m3)%*%matrix(v_1,8,8)%*%matrix(t(a[k,3:10]-m3),8,1)
  d3[k];
  k=k+1
}
d3;

#预测及回判
s1=c(1:31)
distance=c(1:31)
z=1;
for(z in 1:31)
{
  if(d1[z]==min(d1[z],d2[z],d3[z]))
    s1[z]=1;
  if(d2[z]==min(d1[z],d2[z],d3[z]))
    s1[z]=2;
  if(d3[z]==min(d1[z],d2[z],d3[z]))
    s1[z]=3;
  s1[z];
  distance[z]=min(d1[z],d2[z],d3[z]);
  z=z+1
}
s1;

#输出结果
output=matrix(c(s,distance,s1),31,3);
row.name=area;
col.name=c("原始组","最小距离","预测组");
dimnames(output)=list(row.name,col.name);
output;

#预测精度
o=1;
total=0;
t1=0;
t2=0;
t3=0;
for(o in 1:29)
{
  if(s1[o]==s[o])
    total=total+1;
  o+1
}
p=total/29;
p;
#注：为写程序方便,在录入数据时,将“地区”指标用其英文“area”代替,
#用“s”代替“Group”