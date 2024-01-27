library(nleqslv) 
generate_sample<-function(alpha , beta, n)
{
  x<-c()
  for(i in 1:n){
    y<-runif(1)
    x[i]<- (-1/beta)*log(1-y^(1/alpha))
  }
  return (x)
}
x<-generate_sample(4,6,1000)
f1<-function(beta)
  {
  n<-1000
  
  sum1<-sum(x)
  sum2=sum(log(1-exp(-beta*x)))
  sum3=sum(x*exp(-beta*x)/(1-exp(-beta*x)))
  
 
  
   return  ((n/beta)-(sum1)-((n/sum2)+1)*sum3)
}
beta <- nleqslv(1.2,f1)$x
beta

sum4 <- sum(log(1-exp(-beta*x)))
n<-1000
alpha <- (-n/sum(log(1-exp(-beta*x))))
print(paste("Value of alpha",alpha))
print(paste("value of beta",beta))

