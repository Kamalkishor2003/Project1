#Pract8(Non parametric  tests- I,II)
# Sign test
install.packages("BSDA")
library(BSDA)
cat("Enter the sample size")#10
n = scan(nmax = 1)
cat("Enter the values in the sample")
x = scan(nmax = n)#=2.5,2,3.4,3,2,4.5,3,1.2,2.4,4.2
cat("Enter the median to test")#3
md = scan(nmax = 1)
cat("Enter type of test\ntwo.sided\ngreater\nless")
choice=readline()
cat ("Enter alpha value")
alpha = scan(nmax = 1)#0.05

test = SIGN.test(x, y = NULL, md, alternative = choice)
print(test)
if(test$p.value<alpha){
  cat("Reject Null Hypothesis; Accept Alternate Hypothesis")
}else{
  cat("Accept Null Hypothesis; Reject Alternate Hypothesis")
}

# Runs test
library(crayon)
cat(green("enter n1 value"))
n1=scan(nmax = "1")#=4
cat(green("enter n2 value"))
n2=scan(nmax="1")#=4
#R Bar
cat(green("enter number of runs"))
R=scan(nmax = "1")#=2
print("Expected number of runs is:")
x=(2*n1*n2)
y=(n1+n2)
R.bar=((x/y)+1)
R.bar

#standard deviation of the runs
x=(2*n1*n2)
a=(n1-n2)
c=((n1+n2)^2)
d=(n1+n2-1)

print("standard deviation of the runs is:")
Nume=(x*(x-(a)))
Denom=(c*d)
S2=(Nume/Denom)
S2
#Test statistics for Run test
print("Calculated Z value is:")
z=((R - R.bar)/(S2))
z
if(z<1.96){
  cat(red("We Accept H0"))
  cat(red("Hence, Data is Random"))
}else if(z>1.96){
  print("We Reject H0")
  print("Hence, Data is non-Random")
} 