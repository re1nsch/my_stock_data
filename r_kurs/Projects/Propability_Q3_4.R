library(gtools)
library(tidyverse)
data("esoph")

head(esoph)

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

#####################
high_al <- esoph %>% 
  filter(alcgp=="120+")

high_all_cases <- sum(high_al$ncases)
high_all_controls <- sum(high_al$ncontrols)

high_all_cases/(high_all_controls+high_all_cases)
high_all_controls/(high_all_controls+high_all_cases)

#####################
low_al <- esoph %>% 
  filter(alcgp=="0-39g/day")

low_all_cases <- sum(low_al$ncases)
low_all_controls <- sum(low_al$ncontrols)

low_all_cases/(low_all_controls+low_all_cases)

#####################
prob_10 <- esoph %>% 
  filter(tobgp != "0-9g/day")

all_cases_10 <- sum(prob_10$ncases)

all_cases_10/all_cases

#####################
all_controls_10 <- sum(prob_10$ncontrols)

all_controls_10/all_controls

#####################
high_all_cases/all_cases

#####################
high_tob <- esoph %>% 
  filter(tobgp=="30+")

high_all_cases_tob <- sum(high_tob$ncases)
high_all_controls_tob <- sum(high_tob$ncontrols)

high_all_cases_tob/all_cases

#####################
high_alc_tob <- esoph %>% 
  filter(tobgp=="30+" & alcgp=="120+")

high_all_cases_alc_tob <- sum(high_alc_tob$ncases)
high_all_controls_alc_tob <- sum(high_alc_tob$ncontrols)

high_all_cases_alc_tob/all_cases

#####################
high_alc_tob_or <- esoph %>% 
  filter(tobgp=="30+" | alcgp=="120+")

high_all_cases_alc_tob_or <- sum(high_alc_tob_or$ncases)
high_all_controls_alc_tob_or <- sum(high_alc_tob_or$ncontrols)

high_all_cases_alc_tob_or/all_cases

####### QUESTION 6a/b/c/d/e/f #######
# For controls, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

# For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)
