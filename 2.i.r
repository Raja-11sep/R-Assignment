generate_sample<-function(samples)
{
  mu<-1
  sig<-2
  x<-rnorm(samples,mu,sig)
  return (x)
}

samples<-1000

x<-generate_sample(samples)
print(paste("sample",x))

#Calculate the mue
mue = mean(x)

print(paste("Value of Mue",mue))
 
#Calculating the sigma
 
sum<-0

for(i in 1:samples)
{
  sum <- sum + (x[i]-mue)*(x[i]-mue)
}

sigsq<-sum/samples
sigsq
sig<-sqrt(sigsq)
print(paste("Value of sigma",sig))


 
 