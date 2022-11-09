library(tidyverse)

block1 <- 101:106
block2 <- 201:206
block3 <- 301:306
block4 <- 401:406
c(block1,block2,block3,block4) -> plots

subsample <- c(rep("a",length(plots)),
               rep("b",length(plots)))

data.frame(plots,subsample) %>% 
  arrange(plots) %>% 
  mutate(density = "") %>% 
  write.csv("ksera.datasheet.csv")


library(googlesheets4)
google_sheet <- "https://docs.google.com/spreadsheets/d/1wcejmrEWwbagmUeZ2OdPA8N3c7c5rOKcud__cIEsoYw/edit#gid=1765120216"
read_sheet(google_sheet,7,
           gs4_deauth()) -> data_count
read_sheet(google_sheet,8,
           gs4_deauth()) -> treatments

data_count %>% 
  left_join(treatments) -> dat

dat %>% 
  mutate(seed_rate = as.factor(seed_rate)) -> dat1

dat1 %>% 
  ggplot(aes(seed_rate,density)) +
  geom_boxplot()

dat1 %>% 
  ggplot(aes(seed_rate,density)) +
  stat_summary()

dat1 %>% 
  filter(seed_rate == "0") %>% 
  distinct(density)

dat1 %>% 
  filter(seed_rate == "35") %>% 
  arrange(desc(density))
