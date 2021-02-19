library(gtools)
library(tidyverse)

entree<-c(1:6)
sides<-c(1:6)
drink<-c(1:3)
sides_comb <- combinations(6,2)

nrow0 <- dim(sides_comb)[1]

meal <- expand.grid(entree1 = entree, sides1 = c(1:nrow0), drink1 = drink)

#######################################
entree<-c(1:6)
sides<-c(1:6)
drink<-c(1:3)
sides_comb <- combinations(6,3)

nrow0 <- dim(sides_comb)[1]

meal <- expand.grid(entree1 = entree, sides1 = c(1:nrow0), drink1 = drink)

#######################################
sides<-c(1:6)
drink<-c(1:3)
sides_comb <- combinations(6,2)
nrow0 <- dim(sides_comb)[1]

B <- c(1:12)
compute_meals <- function(B){
  meal <- expand.grid(entree1 = c(1:B), sides1 = c(1:nrow0), drink1 = drink)
  nrow(meal)
}

tab <- matrix(c(sapply(B, compute_meals)))
tab

#######################################
entree<-c(1:6)
sides<-c(1:6)
drink<-c(1:3)


B <- c(2:12)
compute_meals <- function(B){
  sides_comb <- combinations(B,2)
  nrow0 <- dim(sides_comb)[1]
  meal <- expand.grid(entree1 = entree, sides1 = c(1:nrow0), drink1 = drink)
  nrow(meal)
}

tab <- matrix(c(sapply(B, compute_meals)))
tab

########################################