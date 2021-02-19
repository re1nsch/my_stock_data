library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits

head(stars)

mean(stars$magnitude)
sd(stars$magnitude)

stars %>% ggplot(aes(magnitude)) + geom_density()

stars %>% ggplot(aes(temp,magnitude, color=type)) + 
  geom_point() + 
  scale_x_log10()