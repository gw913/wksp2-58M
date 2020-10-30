#TASK 1
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

hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

#filter the data
#ten countries with lowest mean HDI
hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)

hdi_summary_low

#plot this
hdi_summary <- hdi3 %>% 
  group_by(Country) %>%
  summarise(mean_index = mean(Index),
            n = length (Index),
            sd = sd(Index),
            se = sd/sqrt(n)) %>%
  filter(rank(mean_index) < 11) %>%
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se,
                    ymax = mean_index + se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()
  

#TASK 2
#view data to decide how to read in
file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4)

#read in data
buoy44025 <- read_table(file,
                        col_names = FALSE,
                        skip = 2)

#scan data
scan(file = "buoy44025",)