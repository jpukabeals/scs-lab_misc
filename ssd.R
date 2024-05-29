# SSD is staggered start date for planting wheatgrass in spring

# drive https://drive.google.com/drive/folders/1pXd9bJmGZWBiIhhRn45jA5axwCFTosGK

# data collected using 12" long quadrat on rows experiencing 12" row spacing.
# Taken from random area and occaisonally shifted to more representaive area if
# there was obviosu planting error


library(tidyverse)

read.csv("SSD plot map - counts.csv") -> dat

read.csv("SSD plot map - treatments.csv") -> trt_codes

dat %>% 
  left_join(trt_codes) -> dat2
  

dat2 %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  ggplot(aes(treatment,plants.per.ft)) +
  stat_summary()
