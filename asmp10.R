#Pract10
#wilcoxon for small sample:
cat(red("enter the values of the sample"))#12
x=scan(nmax="")
cat(red("median of the population is"))#3
mu=scan(nmax = "1")
cat(red("Type 1 for Two.sided\n"))
cat(red("Type 2 for greater\n"))
cat(red("Type 3 for less\n"))
choice=readline()
wilcox.test()
2
if(choice==1){
  wilcox.test(x,y=NULL,mu, alternative = "two.sided", paired = F, exact
              = F)
}else if(choice==2){
  wilcox.test(x,y=NULL,mu, alternative = "greater", paired = F, exact =
                F)
}else if(choice==3){
  wilcox.test(x,y=NULL,mu, alternative = "less", paired = F, exact = F)
}

#Wilcoxon for Large Samples
cat("enter the values of X sample")#3
x=scan()
cat("enter the values of Y sample")#3
y=scan()
len_x = length(x)
len_y = length(y)
cat("Median of population")
mu1=scan(nmax=1)
if (len_x == len_y & len_x >= 30 & len_y >= 30){
  cat("Type 1 for Two.sided\n")
  cat("Type 2 for greater\n")
  cat("Type 3 for less\n")
}
choice = readline()
1
if(choice == 1){
  wilcox.test(x, y, mu, alternative = "two.sided", paired = T, exact = F)
}else if(choice == 2){
  wilcox.test(x, y, mu, alternative = "greater", paired = T, exact = F)
}else if(choice == 3){
  wilcox.test(x, y, mu, alternative = "less", paired = T, exact = F)
}else{
  cat("Enter Correct Values")
}