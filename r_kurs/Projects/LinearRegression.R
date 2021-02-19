library(Lahman)
library(tidyverse)
library(HistData)

#head(Teams)
#dt <- Teams %>% 
#  filter(yearID>=1961 & yearID<=2001) %>%
#  ggplot(aes(x = X2B/G, y = X3B/G)) + geom_point()
#R <- Teams %>% 
#  filter(yearID>=1961 & yearID<=2001) %>%
#  summarize(r = cor(X2B/G, X3B/G)) %>% 
#  pull(r)

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Mean of mothers' heights
mu_mom <- mean(female_heights$mother) 

# Standard deviation of mothers' heights
sd_mom <- sd(female_heights$mother)  

# Mean of daughters' heights
mu_dau <- mean(female_heights$daughter)

# Standard deviation of daughters' heights
sd_dau <- sd(female_heights$daughter)  

# Correlation coefficient
cc <- cor(female_heights$mother, female_heights$daughter)


# Calculate the slope and intercept of the regression line predicting
#  daughters' heights given mothers' heights. Given an increase in mother's
#  height by 1 inch, how many inches is the daughter's height expected to change?

# Slope of regression line predicting daughters' height from mothers' heights
m <- cc*sd_dau/sd_mom  

# Intercept of regression line predicting daughters' height from mothers' heights
b <- mu_dau - m*mu_mom

# Change in daughter's height in inches given a 1 inch increase in the mother's height
(m*2+b) - (m*1+b) # essentially the slope

# percent variablity in daughter due to mother
cc*cc*100

# The conditional expected value of her daughter's height given the mother's height (60)
x = 60
m*x+b