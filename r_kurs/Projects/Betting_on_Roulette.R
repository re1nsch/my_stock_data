mu <- 6*(5/38) + -1*(1-(5/38))
mu

er <- abs(1--6)*sqrt(5/38*(1-(5/38)))
er

er_avg <- (abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))/sqrt(500)

er_sum <- (abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))*sqrt(500)

Mu_sum <- 500*(6*(5/38) + -1*(1-(5/38)))
Mu_sum

pnorm(0,Mu_sum,er_sum)