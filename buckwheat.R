# Preliminary data from buckwheat variety trial during methods testing
# 23Jul

library(tidyverse)

read.csv("Organic Buckwheat Plot Plan - old_data_combined.csv") -> dat


# data availability -------------------------------------------------------
# some plots don't have sufficient low vigor areas to score so there is missing data

dat %>% 
  summary()

dat %>% 
  filter(sample=="no")
# these plots remove are 2 manor plots and a devyatka. 

# summary tables ----------------------------------------------------------

# Canopy
dat %>% 
  summarise(mean = round(mean(canopy)),
            max = round(max(canopy)),
            min = round(min(canopy)),
            sd = round(sd(canopy)))
# Canopy x variety
dat %>% 
  group_by(variety) %>% 
  summarise(mean = round(mean(canopy)),
            max = round(max(canopy)),
            min = round(min(canopy)),
            sd = round(sd(canopy)))

# canopy x vigor
dat %>% 
  group_by(vigor) %>% 
  summarise(mean = round(mean(canopy)),
            max = round(max(canopy)),
            min = round(min(canopy)),
            sd = round(sd(canopy)))

# canopy x vigor x variety
dat %>% 
  group_by(vigor,variety) %>% 
  summarise(mean = round(mean(canopy)),
            max = round(max(canopy)),
            min = round(min(canopy)),
            sd = round(sd(canopy)))

