library(nleqslv)
n<-1000           #sample size
samples<-c()
for(i in 1:n)
{
  samples[i]<-rnorm(1,2,3) # 2nd parameter meu and 3rd parameter sigma
}

samples

min=min(samples)
min
max=max(samples)
max

interval<-5

inc<-(max-min)/interval;
#inc
m2=min

O<-c()
p<-c()
f1<-function(x)
{
  return (pnorm(x,2,3))
}
for(i in 1:interval)
{
  cnt=0;
  m1=m2
  m2=m1+inc
  for(j in 1:n)
  {
    if(samples[j]>=m1 && samples[j]<m2)
      cnt=cnt+1
  }
  p[i]<-(f1(m2)-f1(m1))
  O[i]<-cnt
}
E<-c()
for(i in 1:interval)
{
  E[i]<-sum(O)*p[i]
}
O
E
W=0
for(i in 1:interval)
{
  W = W +(O[i]-E[i])*(O[i]-E[i])/E[i]
}
W
if(W<=9.49){
  print("The hypothesis can be accepted at the 5% level of significance")
}else
  print("Reject the null hypothesis")
