options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

head(titanic)
str(titanic)

a <- table(titanic$Sex)
a[names(a=="female")]

#p<- titanic %>%
#  ggplot(aes(Age,group=Sex, color=Sex, fill=Sex)) +
#  geom_histogram(binwidth = 0.5, alpha = 0.4, position="identity") +
#  geom_density(aes(y=..count..),alpha=0, size=1) +
#  geom_vline(data=titanic, aes(xintercept=40),linetype="dashed", size=1, colour="red")

#params <- titanic %>%
#  filter(!is.na(Age)) %>%
#  summarize(mean = mean(Age), sd = sd(Age))

#p<- titanic %>%
#  ggplot(aes(sample=Age)) +
#  geom_qq(dparams = params) +
#  geom_abline()

#Less than half of passengers survived?
#p<- titanic %>%
#  ggplot(aes(x=Survived)) +
#  geom_bar() +
#  geom_hline(data=titanic, 
#             aes(yintercept=(length(titanic$Survived)/2)),
#             linetype="dashed", size=1, colour="red")

#Most of survivors were female?
#p<- titanic %>%
#  filter(Survived==1) %>%
#  ggplot(aes(x=Sex)) +
#  geom_bar()

#Most of the males survided?
#p<- titanic %>%
#  filter(Sex=="male") %>%
#  ggplot(aes(x=Survived)) +
#  geom_bar()

#Most of the females survided?
#p<- titanic %>%
#  filter(Sex=="female") %>%
#  ggplot(aes(x=Survived)) +
#  geom_bar()

#p<- titanic %>%
#  ggplot(aes(Age,group=Survived, color=Survived, fill=Survived)) +
#  geom_density(aes(y=..count..),alpha=0.2, size=1)

#p<- titanic %>%
#  filter(Fare!=0) %>%
#  ggplot(aes(x=Survived,y=Fare)) +
#  geom_boxplot() +
#  scale_y_continuous(trans = 'log2') +
#  geom_jitter(width = 0.1, alpha = 0.2)

#p<- titanic %>%
#  ggplot(aes(x=Pclass)) +
#  geom_bar()

#p<- titanic %>%
#  filter(Survived==1) %>%
#  ggplot(aes(x=Pclass)) +
#  geom_bar()

p<- titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(position = "stack") +
  facet_grid(Sex ~ Pclass)

p
