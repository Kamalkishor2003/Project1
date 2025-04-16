#Pract2(Problems based on normal distribution)
#Problems based on normal distribution
install.packages("ggplot2")
install.packages("crayon")
library(ggplot2)
library(crayon)
cat(red("enter mean value mu"))
mean=scan(nmax=1)
cat(red("enter sigma value"))
sd=scan(nmax=1)
cat(red("enter value of q for p(x<=q) or p(x>=q)"))
val=scan(nmax=1)
print("Type 1 for p(x<=q)")
print("Type 2 for p(x>=q)")
Type=readline()
2
if(Type==1){
  pnorm(q=val, mean, sd,lower.tail=T)
}else if(Type==2){
  pnorm(q=val, mean, sd,lower.tail=F)
}
