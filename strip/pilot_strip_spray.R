# strip spray trial
# K8-Sack
# 2024
# data collected in July
library(tidyverse)

read.csv("STRIP_plot plan  - data_july_plantcount.csv") -> dat

# 3.8 lbs per gallon formulation
# applied 1 pint per acre is a 1X rate
# 1 pint = 16 fl oz
# 3.8 lbs per gallon = 128 fl oz
3.8/128*16

# applied aprox 0.5 lbs per acre of 24D for 1X
# 1.25 lbs ae per acre is max rate per application
# 1.75 lb ae per acre is max per year

library(measurements)

dat %>% 
  select(plot,X24D_rate_lb_ae_acre,plant_counts_perMeter) %>% 
  rename(rate = X24D_rate_lb_ae_acre,
         count = plant_counts_perMeter) -> dat2

dat2 %>% 
  group_by(rate) %>% 
  summarise(IWG_count_mean = mean(count),
            IWG_count_max = max(count),
            IWG_count_min = min(count),
            IWG_count_sd = sd(count))

dat2 %>% 
  lm(count~rate,.) %>% 
  anova()

dat2 %>% 
  ggplot(aes(rate,count)) +
  stat_summary()  +
  labs(y="IWG plants per meter",
       x="2,4-D (lbs ae per acre)",
       caption="Soil was field cultivated, IWG was seeded into a clean seedbed, soil was packed, \nsoil was sprayed before any crop or weeds emerged, \ncounts were recorded 18 days after spraying")
