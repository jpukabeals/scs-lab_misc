read.csv("GEM scoring 17May2024 by Jesse - Sheet1.csv") -> dat

read.csv("GEM scoring 17May2024 by Jesse - Sheet2.csv") -> trt_matrix

library(tidyverse)

dat %>% 
  select(-c(1:3,5:7)) %>% 
  pivot_longer(cols = 2:4) %>% 
  select(-2) %>% 
  rename(node_height_inches = value) %>% 
  left_join(trt_matrix) %>% 
  ggplot(aes(variety,node_height_inches)) +
  stat_summary()


dat %>% 
  select(-c(1:3,5:7)) %>% 
  pivot_longer(cols = 2:4) %>% 
  select(-2) %>% 
  rename(node_height_inches = value) %>% 
  left_join(trt_matrix) %>% 
  ggplot(aes(spacing,node_height_inches)) +
  stat_summary()

dat %>% 
  select(-c(1:3,5:7)) %>% 
  pivot_longer(cols = 2:4) %>% 
  select(-2) %>% 
  rename(node_height_inches = value) %>% 
  left_join(trt_matrix) %>% 
  mutate(treatment = paste(variety,spacing)) %>% 
  mutate(treatment = fct_reorder(treatment,node_height_inches)) %>% 
  ggplot(aes(treatment,node_height_inches)) +
  stat_summary() +
  coord_flip()
  