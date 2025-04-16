#pract1(Problems based on binomial distribution)
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
cat("enter x value for p(x)\n") #probability
x=scan(nmax = "")

cat("enter the size\n")
size=scan(nmax = 1)

dbinom(x,size,prob = 0.05)

probability=dbinom(x,size,prob = 0.05) %>% sum()
cat("sum of probabilities is:" ,{probability})

cat("enter x value for p(x)\n") #probability
x=scan(nmax = "")
cat("enter the size\n")
size=scan(nmax = 1)

dbinom(x,size,prob = 0.05)
probability=dbinom(x,size,prob = 0.05) %>% sum()
cat("sum of probabilities is:" ,{probability})
