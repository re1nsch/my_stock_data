mu <- 0.2*1+0.8*-0.25

er <- abs(1--0.25)*sqrt(0.2*0.8)*sqrt(44)

1-pnorm(8,mu,er)

####################################

set.seed(21, sample.kind = "Rounding")

n <- 44
B <- 10000
S <- replicate(B, {
  X <- sample(c(-0.25,1), n, replace = TRUE, prob = c(0.8, 0.2))
  sum(X)    
})

sum(X>=8)/10000

####################################

mu <- 0.25*1+0.75*0
44*mu

####################################
fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
}

data.frame(p,sapply(p, FUN=fu))