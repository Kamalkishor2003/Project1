#Pract6(t test, normal test, F test)
## one sample t-test
cat(red("Enter no of Observations of Sample"))
n=scan(nmax = "1")#=6
cat(red("Enter Observations of Sample"))
x=scan(nmax=n)#=22,32,34,24,23,25,26
cat(red("mean of the population is"))
popu_mean=scan(nmax = "1")#=25
t.test(x,y=NULL, mu=popu_mean)

# Two sample t-test(Dependent sample same independent n1=8,n2=6)
cat(red("Enter no of obervations of 1st data"))
n1=scan(nmax="1")#=12
cat(red("enter observations of 1st data"))
x=scan(nmax=n1)#=8,8,8,9,9,11,12,13,13,14,15,19
cat(red("Enter no of obervations of 2nd data"))
n2=scan(nmax="1")#=12
cat(red("enter observations of 2nd data"))
y=scan(nmax=n2)#11,12,13,13,14,14,14,15,16,18,18,19
if(n1!=n2){
  print("Independent samples")
  print(t.test(x,y,mu=0,paired = FALSE))
}else{
  print("Dependent samples")
  print(t.test(x,y,mu=0,paired = TRUE))
}

# F test(Wilcoxon’s signed rank test)
A = c(16, 17, 25, 26, 32, 34, 38, 40, 42)
B = c(600, 590, 590, 630, 610, 630)
cat(red("Enter no of Observations of Sample1"))
n1=scan(nmax = "1")
cat(red("Enter Observations of Sample1"))
S1=scan(nmax=n1)
cat(red("Enter no of Observations of Sample2"))
n2=scan(nmax = "1")
cat(red("Enter Observations of Sampl21"))
S2=scan(nmax=n2)
Variance_Ratio=var.test(S1,S2)
Variance_Ratio