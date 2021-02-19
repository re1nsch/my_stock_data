library(gtools)
library(tidyverse)

set.seed(1, sample.kind="Rounding")

n1 <- permutations(8,3)
nrow(n1)

n2 <- permutations(3,3)
nrow(n2)

nrow(n2)/nrow(n1)

########################################
B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

result <- replicate(B, {
  n3 <- sample(runners, 3)
  any(n3!="Jamaica")
})

mean(!result)