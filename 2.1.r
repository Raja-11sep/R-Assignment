#Let's first generate 1000 samples in the range 2<x<5

x<-c()
x<-runif(1000,2,5)
x
#find the minimum and maximum value of the sample which we know in this case.
min<-2
max<-5
# let's take 3 interval 2-3 , 3-4, 4-5

interval = 3

# Now count number of element in each interval
 O<-c();
 fun<-function(x,min,max)
 {
   cnt=0
   for(i in 1:1000)
   {
     if(x[i]>=min && x[i]<max)
       {
       cnt=cnt+1
       }
   }
   return(cnt)
 }
 for(i in 1:interval)
 {
   O[i]=fun(x,i+1,i+2) # i+1 starting range , i+2 ending range for each parts
 }
 # Now with the help of cdf find pi
 p<-c()
 E<-c()
 
 for(i in 1:interval)
 {
   a=i+2
   b=i+1
   p[i]=(a-2)/3 - (b-2)/3
   E[i]=sum(O)*p[i]
 }
 W=0
 for(i in 1:interval)
 {
   W = W +(O[i]-E[i])*(O[i]-E[i])/E[i]
 }
 
 if(W<=5.99 && sum(O)==sum(E) && sum(O)>50){
   print("The hypothesis can be accepted at the 5% level of significance")
 }else
   print("Reject the null hypothesis")
 