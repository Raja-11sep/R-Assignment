# Question 1

O<-c(12,8,20,2,14,10,15,6,9,4)
O
E<-c()
samples<-sum(O)
p<-1/10
for(k in 1:10)
{
  E[k]=samples*p;
}
E
chisq=0;
for(k in 1:10)
{
  chisq<-chisq+(O[k]-E[k])*(O[k]-E[k])/E[k]
}
chisq
if(chisq<=16.92 && sum(O)==sum(E) && sum(O)>50){
  print("The hypothesis can be accepted at the 5% level of significance")
}else
  print("Reject the null hypothesis")

# Let's check all the 3 conditions like 
# sum(O)=Sum(E)
# sum(O)>50 
# Ei >= 5