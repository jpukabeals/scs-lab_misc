#DISH = Debalin Isidor Herbicide trial
# DISH 2024

# see DISH drive folder https://drive.google.com/drive/folders/1o-Uxzj_jdKeDWl_txSn17louCpe-P9ZO

# Data from DISH Master https://docs.google.com/spreadsheets/d/1HtKeSv93GQMWm8yx5QJRuqrUkAzYI577hPCDMTsKLhU/edit#gid=1999649668


# treatment codes ---------------------------------------------------------

read.csv("DISH master - plot matrix (1).csv") -> plt
read.csv("DISH master - treatment_codes.csv") -> trt_codes


# Forage data fall cut May 2024 ----------------------------------------------------

# Just fall sprayed plots

library(tidyverse)

read.csv("DISH master - yield.csv") -> dat

# select useful columns
dat %>% 
  select(experiment, timing, plot, 
         forage_quadrat_dry_grams_no_bag) -> dat2

# convert to grams per square meter
library(measurements)
dat2 %>% 
  filter(timing == "fall") %>% 
  mutate(dmyield_g_m2 = forage_quadrat_dry_grams_no_bag/.152258,
         dmyield_Mg_ha = conv_multiunit(dmyield_g_m2, 
                                        from = "g / m2",
                                        to = "Mg / hectare")) -> dat3

# add in treatments

plt %>% 
  select(-c(herbicide,rate,treatment,date)) %>% 
  left_join(trt_codes) %>% 
  left_join(dat3) %>% 
  filter(timing=="fall")-> dat4

# export
dat4 %>% 
  write.csv("dish_forageYieldFallSpray_8May2024.csv")

# summary
dat4 %>% 
  select(-c(dmyield_g_m2,forage_quadrat_dry_grams_no_bag)) -> dat5


dat5 %>% 
  summary()
# no missing data, yield averaged around 0.6 Mg ha
# spring hay yield is about 2.4 Mg ha (Hunter et al 2020)

dat5 %>% 
  ggplot(aes(dmyield_Mg_ha)) +
  stat_bin() +
  geom_vline(xintercept = 2.4) +
  labs(caption = "Spring forage yield is typically 2.4 Mg ha (Hunter et all 2020)")

dat5 %>% 
  ggplot(aes(dmyield_Mg_ha)) +
  geom_boxplot() +
  labs(caption = "some outliers, but not a lot")

# reweigh these samples to confirm the data is accurate
dat5 %>% 
  # filter(dmyield_Mg_ha < 2.2)
  filter(dmyield_Mg_ha > 3.6)

# Data was innacurately recorded for outliers. Reweighing. New data download and restarting script. 



# Spring forage May 2024 --------------------------------------------------


library(tidyverse)

read.csv("DISH master - yield (2).csv") -> dat6

# select useful columns
dat6 %>% 
  select(experiment, timing, plot, 
         forage_quadrat_dry_grams_no_bag) -> dat7

# convert to grams per square meter
library(measurements)
dat7 %>% 
  # filter(timing == "spring") %>% 
  mutate(dmyield_g_m2 = forage_quadrat_dry_grams_no_bag/.152258,
         dmyield_Mg_ha = conv_multiunit(dmyield_g_m2, 
                                        from = "g / m2",
                                        to = "Mg / hectare")) -> dat8

# add in treatments

plt %>% 
  select(-c(herbicide,rate,treatment,date)) %>% 
  left_join(trt_codes) %>% 
  left_join(dat8)  -> dat9

# export
dat9 %>% 
  write.csv("dish_forageYieldSpringSpray_28May2024.csv")

dat9 %>% 
  mutate(harvest_date = if_else(timing=="fall", "05-06-2024", "05-23-2024")) %>% 
  mutate(harvest_date = as.POSIXct(harvest_date,
                                   format = "%m-%d-%Y")) %>% 
  relocate(harvest_date, .before = experiment)-> dat10

# QAQC

dat9 %>% 
  # glimpse()
  filter(timing=="spring") %>% 
  ggplot(aes(dmyield_Mg_ha)) +
  geom_boxplot()

dat9 %>% 
  filter(timing=="spring") %>% 
  filter(dmyield_Mg_ha>4)
# bags were discarded

# looking at it all

dat9 %>% 
  ggplot(aes(dmyield_Mg_ha)) +
  geom_boxplot() 



# water content -----------------------------------------------------------

# fall
dat %>% 
  filter(timing=="fall") %>% 
  # mutate(water_content = (forage_quadrat_wet_grams_with_bag - forage_quadrat_dry_grams_with_bag) / (forage_quadrat_dry_grams_with_bag + (forage_quadrat_wet_grams_with_bag - forage_quadrat_dry_grams_with_bag)) * 100)
  mutate(water_content = (forage_quadrat_wet_grams_with_bag - 
                            forage_quadrat_dry_grams_with_bag) /  
           forage_quadrat_wet_grams_with_bag * 100) -> dat_fall_water
  
dat6 %>% 
  filter(timing=="spring") %>% 
  mutate(water_content = (forage_quadrat_wet_grams_no_bag - 
                            forage_quadrat_dry_grams_no_bag) /  
           forage_quadrat_wet_grams_no_bag * 100) -> dat_spring_water

bind_rows(dat_fall_water,dat_spring_water) %>% 
  select(1:8,water_content) -> dat_water

