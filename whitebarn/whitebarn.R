
library(tidyverse)
library(measurements)

# Whitebarn summer baseline data
# https://docs.google.com/spreadsheets/d/1CtGkiFazbxG_Xeg4kQVQFkLK9TAsaLhy8RNksqq9BNo/edit#gid=0

read.csv("White Barn Master - grain and straw.csv")->dat

# still needs threshed grain no bag

dat %>% 
  rename_all(tolower) %>% 
  select(Dry_straw_no_bag.g., dry)
  glimpse()
