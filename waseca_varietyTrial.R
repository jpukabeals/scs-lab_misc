# https://docs.google.com/spreadsheets/d/1OzEdmaXH6pbxAmr4vEpOwW3Rppu2h2_bIOsOnOUdcwY/edit#gid=184899642

# Waseca variety trial data
# 2019 is production year 1, 2022 is production year 4

read.delim(
  "clipboard"
) -> dat

library(tidyverse)

dat %>% 
  pivot_longer(
    cols = c(X2019, X2020,X2021,X2022)
  ) %>% 
  mutate(year = str_sub(name, 2)) %>% 
  rename(yield = value) %>% 
  dplyr::select(-name) -> dat2

dat2 %>% 
  # distinct(Plot) 
  distinct(Variety)

dat2 %>% 
  ggplot(aes(yield)) +
  stat_bin()

dat2 %>% 
  ggplot(aes(year,yield)) +
  geom_jitter(width = .2) +
  geom_boxplot(fill=NA)
  


# kg ha basis -------------------------------------------------------------


0.83612736 -> quadrat_m2

dat2 %>% 
  mutate(yield.kg.ha = yield/1000/quadrat_m2*10000) -> dat3

dat3 %>% 
  ggplot(aes(year,yield.kg.ha)) +
  geom_jitter(width = .2) +
  geom_boxplot(fill=NA)


  
  
