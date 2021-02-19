library(dslabs)
data(heights)

c <-ifelse(heights$sex == "Female",1,2)
sum(c)

d <-ifelse(heights$height > 72, heights$height,0)
mean(d)



inches_to_ft <- function(x){
  x/12
}

inches_to_ft(144)

e <-ifelse(inches_to_ft(heights$height) < 5, 1,0)
sum(e)