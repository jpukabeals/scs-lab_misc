# 17Jan2023
# Just finished threshing summer harvest
# I selected a subset of data from master
# double checking things make sense and were entered correctly
# units are in grams for everything mass related

# we expect...


read.csv("Master PeWhe 2 - Copy of Yield.csv") -> dat

# we only want the 40 observations of summer harvest

library(tidyverse)

dat %>% 
  filter(harvest_timepoint == "summer harvest") -> dat2

# how are wet and dry seedheads correlated?

dat2 %>% 
  ggplot(aes(wet_seedheads,dry_seedheads)) +
  geom_point()
# that looks good!

dat2 %>% 
  mutate(moisture = ((wet_seedheads-dry_seedheads)/wet_seedheads)*100) %>% 
  select(moisture) %>% 
  summary()
# average of 50% water content in the seedheads
# it's possible this summer harvest was a bit too early and the seeds too wet

# how are dry seedheads correlated with threshed grain?
dat2 %>% 
  ggplot(aes(dry_seedheads,threshed_grain)) +
  geom_point()
# further support that dry seedhead data can be used to predict threshed grain

# how are seedhead count and threshed grain correlated?
dat2 %>% 
  ggplot(aes(seedhead_count,threshed_grain)) +
  geom_point()
# also very cool, shows that seedhead count doesn't have a strong effect

# Overall, how were the yields
# each quadrat was 20x20" 
# we expect around 1-3k kg ha yields

library(measurements)

conv_unit(20*20,"inch2","m2")

conv_unit(0.5,"m","inch")
# rows were 12 inch row spacing which means 1 row for 12" and 2 rows for 24".
# since we used a 20" x 20" quadrat, we got just 1 row
# so we really are 12" x 20", even though we used a 20"x20"

conv_unit(12*20,"inch2","m2")


dat2 %>% 
  mutate(
    yield_kgha = threshed_grain/conv_unit(20*12,"inch2","hectare")*
      conv_unit(1,"g","kg")) -> dat3

# distribution of yield in kg ha

dat3 %>% 
  ggplot(aes(variety.code,yield_kgha)) +
  geom_point()

