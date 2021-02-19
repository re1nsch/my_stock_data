library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

min_c <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

max_c <- temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

min_c_c <- temp_carbon %>% 
  filter(year==min_c) %>% 
  select(carbon_emissions)

max_c_c <- temp_carbon %>% 
  filter(year==max_c) %>% 
  select(carbon_emissions)

max_c_c/min_c_c

#-------------------------------------

min_t <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()
min_t

max_t <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()
max_t

min_t_t <- temp_carbon %>% 
  filter(year==min_c) %>% 
  select(temp_anomaly)
min_t_t

max_t_t <- temp_carbon %>% 
  filter(year==max_c) %>% 
  select(temp_anomaly)
max_t_t

#---------------------------------

greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept=1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#---------------------------------

temp_carbon %>%
  ggplot(aes(year,carbon_emissions)) +
  geom_line()

historic_co2 %>%
  filter(year<=2018 & year>=-3000) %>% 
  ggplot(aes(year,co2, color=source)) +
  geom_line()