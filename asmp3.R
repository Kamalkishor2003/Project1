#Pract3(Property plotting of binomial distribution)
cat(red("enter the size"))
size=scan(nmax = "1") # size =25
cat(red("enter the Probability of success"))
p=scan(nmax = "1") # probability of success =0.3
cat(red("enter number of random sample"))
n=scan(nmax = "1") # number of random samples = 1000
set.seed(3) # set seed for reproducibility
random.binom.numbers=rbinom(n, size, p)
h=hist(random.binom.numbers,
       breaks = length(unique(random.binom.numbers)),
       plot = FALSE)
plot(h,
     freq = FALSE,
     space = NULL,
     xlim = c(0,size),
     xlab = 'Students passing the final exam', # label for x-axis
     ylab = "Probability", # label for y-axis
     main = "Binomial Probability Distribution \nfor size=25 and p=0.3", 
     # add title
     col = "yellow", # add color "#fc9d12"
     xaxt = "n")# do not show ticks of the x-axis
axis(side=1, at=seq(0.5,size, by=1), labels=seq(1,size, by=1))
