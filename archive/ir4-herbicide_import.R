# Jesse
# need to talk to Jake to clarify "stand" column in his datasheet
# see email that was sent to Jake on 2022-03-02


# 2March2022
# Objective: add r34 yield data into Jake's IR4 excel sheet

## Experiment data collection overview
# I am still unclear what stand column refers too, but likely a mistake was made
# need to get more clarity from jake
# production year 1 = new stand , production year 2 is old stand?
# r34 planted in 2019, sprayed 2019+2020+2021, harvested 2020+2021
# r34-2019 = new, r34-2020 = old
# r90 planted in 2020, sprayed 2020+2021+2022, harvested 2021+2022
# r90-2021 = new, r90-2022 = old

## Dataset status
# Data for (r34/r35) is complete for both years (2020-2021)
# Data for (r90) still needs spraying and scoring in spring 2022 and yield for 2022
# Data for r90 yield for 2021 is currently being threshed as of 2March2022

getwd()
setwd("C:/Users/pukab001/Downloads")
library(tidyverse)


# datasheet to fill
# downloaded from below url on 2March2022
# https://docs.google.com/spreadsheets/d/1mSXNLW3UX_hLpcwmNTCLwOEb6SwSJQU8/edit?usp=drive_web&ouid=106050995414111542297&rtpof=true
# weed_injury, weed_pressure and iwg_injury were already added to this datasheet, just needs yield
readxl::read_xlsx(path = "Data-2020-2021.JPB.xlsx",
                  sheet = 1, #note sheet number
                  skip = 14) %>%
  # glimpse()
  rowid_to_column() %>% 
  mutate(across(where(is.character),as.factor)) %>%
  mutate(across(yield_needed,as.factor)) %>%
  filter(sample_date=="2021-06-25") %>%
  # filter(is.na(weed_pressure))
  # filter(stand=="Old") %>%
  filter(stand=="New") %>%
  summary()
# there is no missing data for iwg_injury
# there is one missing datapoint for weed_pressure and weed_injury in 2021
  # r90 plot 328, double checked and no data was recorded
# there is missing data for weed_pressure and weed_injury in 2020 because it was unclear if we were collecting that data

# yield_needed column was added by Jake 
# yield_needed specifies actions taken based on a mistake in treatment application in 2021
# yield_needed only applies to data collected in 2021

# Based on Jake's calculations from yield_needed, for 2021 we need... 
# 108-37-12 = 59 yields from R90
# 108-31-16 = 61 yields from R35
# overall we want to thresh 59+61=120 samples


# empty datasheet for threshed grain from 2021
# this datasheet was created by Jesse based on the yield_needed column
readxl::read_xlsx("Data-2020-2021.JPB.xlsx",
                  sheet = 4) %>% #note sheet number
  # arrange(action) %>%
  # filter(action==3) 
  mutate(across(c(year,site,plot,action),as.factor)) %>% 
  summary()
# datasheet specified...
# 28 plots had action = 3, meaning do NOT thresh
# 61 plots had action = 2, meaning treatment name must be changed
# 59 plots had action = 1, meaning the treatment name is good
# 68 plots did not have an action, these are plots that we either are not spraying till sprin 2022, or were supposed to be sprayed in fall 2021 but weren't
# overall, our datasheet specified the threshing of 61+59 plots for 2021 = 120 samples
# if our datasheet was filled, it should meet yield_needed requirements for 2021


# cross checking with datasheet Jake made
readxl::read_xlsx("Data-2020-2021.JPB.xlsx",
                  sheet = 3, #note sheet number
                  skip=14) %>%
  # glimpse()
  mutate(across(yield_needed,as.factor)) %>% 
  summary()
# so Jake listed yield_needed for 59+61+28 plots
# we need 59+61 plots to have yield data
# confirmed: we are good


# filled datasheet for threshed grain 
# https://docs.google.com/spreadsheets/d/1fAq0c27S0JIGTh7hTj23kZnU9rb4YDzP166eS_N3idg/edit#gid=582434439

read.csv("Master Herbicide - Biomass_20220302.csv") %>%
  # distinct(Location)
  # head()
  # glimpse()
  rowid_to_column() %>%
  rename_all(tolower) %>%
  mutate(across(c(year,location,trt.,time,flag,plot),as.factor)) %>% 
  filter(year=="2021") %>% 
  # summarise(grain_counts=sum(!is.na(threshed_grain_no_bag.g.))) 
  # summary()
  # arrange(seedheads_no_bag.g.) %>%
  # head()
  drop_na(threshed_grain_no_bag.g.) %>%
  # glimpse()
  summary()
  
## outliers
# we have a single problematic datapoint in row 255 of excel sheet, but this datapoint is not threshed grain
# all threshed grain data values seem reasonable

## datapoints
# we have 61 threshed grain datapoints, not complete dataset
# this is because we have no R90 data

# checking we have all r34 data
readxl::read_xlsx("Data-2020-2021.JPB.xlsx",
                  sheet = 4) %>% #note sheet number
  # arrange(action) %>%
  # filter(action==3) 
  mutate(across(c(year,site,plot,action),as.factor)) %>%
  drop_na(action) %>% 
  filter(action!="3") %>% 
  # filter(action=="2") %>%
  summary()
# we need 61 yield datapoints from r34 and we have 61 yield datapoints


# correcting plot numbers in Katherine's dataset --------------------------
# if action = 2, then the herbicide treatment is changed
# Jake's datasheet has columns "new_herbicide" which will superceded the "herbicide column"








# joining datasets ------------------------------------------------------------------

# adding Katherine's yield dataset to Jakes

# Jakes dataset + Jesse's herbicide scoring
dat.jake <- readxl::read_xlsx(path = "Data-2020-2021.JPB.xlsx",
                  sheet = 1, #note sheet number
                  skip = 14) %>%
  mutate(across(where(is.character),as.factor)) %>%
  mutate(across(c(yield_needed,plot),as.factor))

summary(dat.jake)

# Katherine's dataset of yield
read.csv("Master Herbicide - Biomass_20220302.csv") %>%
  rename_all(tolower) %>%
  mutate(across(c(year, location, trt., time, flag, plot), as.factor)) %>%
  drop_na(threshed_grain_no_bag.g.) %>%
  # summary()
  # glimpse()
  mutate(stand = "Old") %>%
  # glimpse()
  summary()

dat.yield.2021 <- read.csv("Master Herbicide - Biomass_20220302.csv") %>%
  rename_all(tolower) %>%
  mutate(across(c(year, location, trt., time, flag, plot), as.factor)) %>%
  drop_na(threshed_grain_no_bag.g.) %>%
  # summary()
  # glimpse()
  mutate(stand = "Old") %>%
  filter(year == "2021") %>%
  mutate(stand=as.factor(stand)) %>%
  dplyr::rename(yield_2021="threshed_grain_no_bag.g.") %>%
  dplyr::select(stand,plot,yield_2021)

summary(dat.yield.2021)

# dat.jake is format we want to add data into
# joining yield data from 2021 to r34
summary(dat.jake)
summary(dat.yield.2021)

left_join(dat.jake,
          dat.yield.2021,
          by=c("stand","plot")) %>%
  write.csv("temp.csv",
            na = "",
            row.names = F)


# joining yield data from 2020 to r34
dat.yield.2020 <- read.csv("Master Herbicide - Biomass_20220302.csv") %>%
  rename_all(tolower) %>%
  mutate(across(c(year, location, trt., time, flag, plot), as.factor)) %>%
  drop_na(threshed_grain_no_bag.g.) %>%
  # summary()
  # glimpse()
  mutate(stand = "Old") %>%
  filter(year == "2020") %>%
  mutate(stand = as.factor(stand)) %>%
  dplyr::rename(yield_2020=threshed_grain_no_bag.g.) %>% 
  dplyr::select(stand,plot,yield_2020)

summary(dat.yield.2020)

left_join(dat.jake,
          dat.yield.2021,
          by=c("stand","plot")) %>%
  left_join(.,
            dat.yield.2020,
            by=c("stand","plot")) %>%
  write.csv("herbicide-ir4_r34_20220302.csv",
            na = "",
            row.names = F)

# now I'm going to manually add data from csv to the herbicide excel Jake needs for reporting r34


## Next steps
# add in 2021 yield data from R90
read.csv("Master Herbicide - Biomass_20220307.csv") %>%
  rename_all(tolower) %>% 
  filter(year=="2021" &
           location=="R90") %>% 
  drop_na(threshed_grain_no_bag.g.) %>% 
  glimpse()


# add in 2022 weed_pressure, iwg_injury, weed_injury and yield_2022 for R90 once collected

