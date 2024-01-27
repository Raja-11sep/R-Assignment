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
x<-generate_sample(1,1,1000)
x
f1<-function(alpha)
{
  n<-1000
  sum1<-sum(log(x))
  sum2<-sum(log(1-(x)^alpha))
  sum3<-sum((x^alpha)*log(x))
  sum4<-sum(1-(x)^alpha)
  result<-((n/alpha)+sum1+(n/sum2 + 1)*(sum3/sum4))
  return(result)
}
alpha <- nleqslv(1.2,f1)$x
s<-sum(log(1-(x^alpha)))
beta<-((-n)/s)
print(paste("The value of alpha",alpha))
print(paste("The value of beta",beta))

