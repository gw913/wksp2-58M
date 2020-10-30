#set working directory 

#load tidyverse package 
library(tidyverse)

#read in data
file <- "Human-development-index.csv"
hdi <- read_csv("Human-development-index.csv")
hdi

#tidy the data 
hdi2 <- hdi %>% 
  pivot_longer(names_to = "Years",
               values_to = "Index",
               cols = -c(Country, 'HDI Rank (2018)'))

#filter the dataset
is.na(hdi2)
hdi3 <- na.omit(hdi2)

#summarising data
hdi_summary <- hdi3 %>% 
  group_by(Country) %>%
  summarise(mean_index = mean(Index),
            n = length(Index),
            sd = sd(Index),
            se = sd/sqrt(n))
#test 
