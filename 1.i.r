library(nleqslv)
my_function <- function(t,samples) 
{
  a=c()
  for(i in 1: samples)
  {
    y=runif(1)
    f<-function(x){
      return(y-1+((t+1+t*x)/(t+1))*exp(-t*x))
    }
    a[i]<-nleqslv(1.2,f)$x
  }
  return(a)
}

samples = 1000
t = 4

data<-my_function(t,samples)
data

my_function1 <- function(samples,data) 
{
     s<-sum(data)
     n<-samples
     f<-function(t)
      {
      return(s-(n/t)*((t+2)/(t+1)))
      }
    root<-nleqslv(1.2,f)$x
  
  return(root)
}
my_function1(samples,data)
