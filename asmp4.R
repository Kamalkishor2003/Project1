#Pract4(Property plotting of normal distribution)
cat("Plotting normal distribution\n")
cat("Enter the mean of the distribution")
mean = scan(nmax = "1")#=0
cat("Enter the standard deviation of the distribution")
sd = scan(nmax = "1")#=1
cat("Enter number of observations to be generated (for random sample)")
n = scan(nmax = "1")#=1000
generated = rnorm(n, mean, sd)
minimum = min(generated)
maximum = max(generated)
h = hist(generated,
         plot = FALSE)
plot(h,
     freq = FALSE,
     xlim = c(minimum, maximum),
     ylim = c(0, 1),
     xlab = "Observed Value",
     ylab = "Probability",
     main = "Normal Probability Distribution",
     col = "#FFD700")
line_range = seq(minimum, maximum, by = (maximum-minimum)/100)
lines(line_range, dnorm(line_range, mean(generated),
                        sd(generated)))

