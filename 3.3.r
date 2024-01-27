library(nleqslv) 
generate_sample<-function(alpha , beta, samples)
{
  x<-c()
  for(i in 1:samples)
  {
    y<-runif(1)
    x[i]<-(1-(1-y)^(1/beta))^(1/alpha)
  }
  return (x)
}
n<-1000
x<-generate_sample(1,2,1000)
x
samples<-x
alpha=1
beta=2

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
  return ((1-(1-x^alpha)^beta))
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
  E[i]<-n*p[i]
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


