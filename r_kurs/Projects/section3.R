library(dslabs)
avg <- mean(heights$height)
ind <- (heights$height > avg)

sum(ind & (heights$sex == "Female"))

mean((heights$sex == "Female"))

heights$height[which.min(heights$height)]

which.min(heights$height)

heights$height[which.max(heights$height)]

x <- 50:82

sum(!x%in%heights$height)

heights2 <- mutate(heights, ht_cm = heights$height*2.54)
heights2$ht_cm[18]
mean(heights2$ht_cm)

females <- filter(heights2,sex=="Female")
length(females)
nrow(females)

mean(females$ht_cm)