library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)

x<-death_prob %>% 
  filter(age==50 & sex=="Female")
x$prob

n <- 1000

ep <- -150000*x$prob + 1150*(1-x$prob)
ep

se <- abs(1150--150000)*sqrt(x$prob*(1-x$prob))
se

n*ep
sqrt(n)*se

pnorm(0,n*ep,sqrt(n)*se)

y<-death_prob %>% 
  filter(age==50 & sex=="Male")
y$prob

b <- ((700000/1000) - -150000*y$prob)/(1-y$prob)
b

err <- sqrt(1000) * abs(b - -150000) *sqrt(y$prob*(1-y$prob))
err

pnorm(0, 1000*(-150000*y$prob + b*(1-y$prob)), err)


#################################################
p <- 0.015
n <- 1000

ep <- -150000*p + 1150*(1-p)
ep

se <- abs(1150--150000)*sqrt(p*(1-p))
se

n*ep
sqrt(n)*se

pnorm(0,n*ep,sqrt(n)*se)
pnorm(-1000000,n*ep,sqrt(n)*se)

p <- seq(.01, .03, .001)
n <- 1000

ep <- -150000*p + 1150*(1-p)
ep

se <- abs(1150--150000)*sqrt(p*(1-p))
se

n*ep
sqrt(n)*se

data.frame(p,pnorm(0,n*ep,sqrt(n)*se))

#########################################

p <- seq(.01, .03, .0025)
n <- 1000

ep <- -150000*p + 1150*(1-p)
ep

se <- abs(1150--150000)*sqrt(p*(1-p))
se

n*ep
sqrt(n)*se

data.frame(p,pnorm(-1000000,n*ep,sqrt(n)*se))

########################################
set.seed(25, sample.kind = "Rounding")

p_loss <- .015
n <- 1000
X <- sample(c(1,0), n, replace = TRUE, prob = c(p_loss,1-p_loss))

loss <- -150000*sum(X==1)/10^6
profit <- 1150*sum(X==0)/10^6
loss+profit

#######################################
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# What is the expected profit per policy at this rate?
l*p + x*(1-p)

# What is the expected profit over 1,000 policies?
n*(l*p + x*(1-p))

# Run a Monte Carlo simulation with B=10000 to determine the probability of losing
#  money on 1,000 policies given the new premium x, loss on a claim of $150,000,
#  and probability of claim p=0.015. Set the seed to 28 before running your 
#  simulation.
# What is the probability of losing money here?
set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  loss <- l*sum(X==1)/10^6 # in millions
  profit <- x*sum(X==0)/10^6
  loss+profit
})
sum(S<0)/10000


######### QUESTION 6a/b #########
# The company cannot predict whether the pandemic death rate will stay stable. Set
#  the seed to 29, then write a Monte Carlo simulation that for each of B=10000
#  iterations:
#  - randomly changes p by adding a value between -0.01 and 0.01 with
#    sample(seq(-0.01, 0.01, length = 100), 1)
#  - uses the new random p to generate a sample of n=1000 policies with premium x
#    and loss per claim l=-150000
#  - returns the profit over n policies (sum of random variable)
# The outcome should be a vector of B total profits
set.seed(29)
n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268
X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})

# What is the expected value over 1,000 policies?
mean(X)

# What is the probability of losing money?
sum(X<0)/B

# probability of losing more than one million dollars?
mean(X<-1000000)