
# randomization of narrow-wide
set.seed(314)
sample(
  rep(c("narrow","wide"),4),
  8,F) -> spacing_rando

# this gets fixed across all plot numbers

c(101:108,201:208,301:308,401:408) -> plots

library(tidyverse)

plots %>% 
  as_tibble() %>% 
  rename(plot = value) %>% 
  mutate(spacing = rep(spacing_rando,4)) -> dat1


# randomization of nitrogen treatment.

# this occurs within the fixed row spacings and block. Therefore, in block 1 we
# need to randomize the wides and then randomize the narrow. Thus we want to
# create a list where the 4 n rates and randomized in batches of 4

set.seed(314)
n_rates <- c("NA","low","med","high")
iterations <- 16

sets_of_batches <- list()

for (i in 1:iterations) {
  sample(n_rates,4,replace = F) -> batch
  sets_of_batches[[i]] <- batch
  
}

# wide row spacing
dat1 %>% 
  filter(spacing=="wide") %>% 
  mutate(n_treatment = unlist(sets_of_batches)[1:16]) -> dat_wide

# narrow row spacing
dat1 %>% 
  filter(spacing=="narrow") %>% 
  mutate(n_treatment = unlist(sets_of_batches)[17:32]) -> dat_narrow

# combine two datasets

dat_wide %>% 
  bind_rows(dat_narrow) %>% 
  arrange(plot) -> dat2

dat2 %>% 
  mutate()

write.csv(dat2,
          "clipboard")

write_delim(dat2,
            "clipboard")

dat2%>% 
  View()


