# checking Katherine's label document at her request for errors
# RALLF

library(tidyverse)


# sheet = soil and RICS ---------------------------------------------------

# soil and RIC labels
readxl::read_xlsx("2022_rallf_labels_katherine.xlsx",
                  sheet = 1) -> dat.soil.ric

dat.soil.ric %>% 
  # glimpse()
  rename_all(tolower) %>%
  # colnames()
  # mutate_if(is.character,as.factor) %>% 
  # mutate(plot=as.factor(plot))
  # mutate(across(plot,as.factor)) %>% 
  mutate(across(
    .cols = where(is.character),
    .fns = as.factor
    # .names = tolower({.col})
  )) %>% 
  mutate(across(
    .cols = c(1,4:6),
    .fns = as.factor
  )) %>% 
  summary()


dat.soil.ric %>% 
  rename_with(tolower) %>%  #all column names lower case
  mutate(across(
    .cols = everything(),
    .fns = tolower
  )) %>%  # all text to lower case
  mutate(across(
    .cols = everything(),
    .fns = as.factor
  )) %>% #all data to factors
  # summary()
  mutate(cuts = fct_recode(cuts,
                           "5 cut" = "5 cuts")) -> dat.soil.ric.tidy


dat.soil.ric.tidy %>% 
  summary()
# uneven cut treatments, (5 cut; n=416), (4 cut; n=352)
# I wonder why?



# further investigating summary() output "(Other)" ------------------------

## plot
dat.soil.ric.tidy %>% 
  group_by(location,cuts) %>% 
  count(plot) %>% 
  # distinct(n)
  count(n)
# there are 32 unique plots
# 5 cut plots have 26 labels associated with them for 2022
# 4 cut plots have 22 labels associated with them for 2022
# there are 8 4cut and 8 5cut plots per site
# LOOKS GOOD

## sample timing
# just making sure everything looks logical
dat.soil.ric.tidy %>% 
  count(`sample timing`) %>% 
  arrange(n)
# LOOKS GOOD


##sample type
dat.soil.ric.tidy %>% 
  count(`sample type`) %>% 
  arrange(n)
# ric: stri 5 + ss5 only happen in 5 cut, where there is half as many
# the ss1:4 are the soil removed prior to inserting stri1:4
# ss0 should correspond to the ltri1 or 2
# ss0 n=32 but ltri n=64


# need to relevel sample timing
dat.soil.ric.tidy %>% 
  distinct(`sample timing`) %>% # I want these levels
  .$`sample timing`-> lvls

dat.soil.ric.tidy %>% 
  mutate(`sample timing` = factor(`sample timing`,
                                  levels = sample.timing.levels)) ->dat.soil.ric.tidy

dat.soil.ric.tidy %>% 
  # glimpse()
  group_by(`sample timing`,
           # cuts,
           `for`) %>% 
  count(`sample type`) %>% 
  print(n=nrow(.))

# We are installing both STRI-1 and LTRI-1 on april-1


# Katherine Notes - Soil+RIC sheet----------------------------------------------

# see https://docs.google.com/presentation/d/18SoCT98aZnyGcpgaiYP8gWXcOe17oKdz/edit#slide=id.p5
# ^schedule might be old/outdated
# sample timing for ltri 2 install is ~1april, but is 28June at above link

# no standing stock for LTRI's? confirming...
# no LTRI 3? confirming...


# datasheet for Katherine with consistent formatting if interested
dat.soil.ric.tidy %>% 
  write.csv("rallf_soil-ric.csv")


# sheet = quadrat samples -------------------------------------------------
readxl::read_xlsx("2022_rallf_labels_katherine.xlsx",
                  sheet = 2) -> dat.quad

dat.quad %>% 
  # glimpse()
  rename_with(tolower) %>% 
  mutate(across(.fns=tolower)) %>% 
  mutate(across(.cols = where(is.character),.fns = as.factor)) -> dat.quad.tidy

dat.quad.tidy %>% 
  distinct(`sample timing`) %>% 
  .$`sample timing`-> lvls

dat.quad.tidy %>% 
  mutate(`sample timing` = factor(`sample timing`,
                                      levels=lvls)) -> dat.quad.tidy

# checking summary
dat.quad.tidy %>% 
  summary()
# LOOKS GOOD

dat.quad.tidy %>% 
  View

## plot
dat.quad.tidy %>% 
  count(plot) %>% 
  count(n)
# GOOD - explained by different cutting treatments

## harvest point
dat.quad.tidy %>% 
  count(`harvest point`)

dat.quad.tidy %>% 
  group_by(`sample timing`) %>% 
  count(`harvest point`)

# comparing the two datasets
dat.soil.ric.tidy %>% 
  distinct(`sample timing`)

dat.quad.tidy %>% 
  distinct(`sample timing`)
#LOOKS GOOD


# Katherine Notes - quadrat sheet -----------------------------------------

# LOOKS GOOD

# datasheet for my bottle codes
dat.quad.tidy %>% 
  relocate(`bottle code`,
           .before=year) %>% 
  write.csv("rallf_bottle-codes_2022.csv",
            row.names = F)

