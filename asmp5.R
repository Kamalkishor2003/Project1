#Pract5(Plotting pdf, cdf, pmf, for discrete and continuous distribution)
#Discrete Distributions:PMF(Probability Mass Function), CDF(Cumulative Distribution Function)
#Continuous Distributions:PDF(Probability Density Function) ,CDF(Cumulative Distribution Function) 

#PMF For Binomial Distribution
n =13
p = 0.7
dbinom(6, size = n, prob = p)
x <- 0:n
plot(x, dbinom(x, size = n, prob = p), main = "Probability mass function for Bin(13,0.7)")

#CDF For Binomial Distribution
pbinom(9, size = n, prob = p)
plot(x, pbinom(x, size = n, prob = p), type="s", main = "Cumulative distribution function for Bin(13,0.7)")

#PDF for Uniform Distribution
a <- 0
b <- 1
# plots similar to what we had before, but using a line rather than points.
curve(dunif(x, min = a, max = b), from = -1, to = 2, xlab='y', ylab='f(y)', main='Probability density function for Unif(0,1)')

#PDF for Normal Distribution
mu <- 0
sigma <- 1 # standard deviation
curve(dnorm(x, mean = mu, sd = sigma), # notice the 3rd argument is the sd
      from = -4, to = 4,
      main = "PDF for a standard normal")

#CDF for a Normal Distribution
curve(pnorm(x, mean = mu, sd = sigma),
      from = -4, to = 4,
      main = "CDF for a standard normal",
      ylab = "F(x)")
