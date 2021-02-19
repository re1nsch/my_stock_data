library(dplyr)
library(dslabs)
data(gapminder)

gapminder %>%
  filter(continent == "Africa" & fertility <= 3 & year==2012 & life_expectancy>70) %>%
  select(country,region) %>%
  df