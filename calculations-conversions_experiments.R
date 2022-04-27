

# FGI water quality -------------------------------------------------------

## old school
# plot is 500 ft by 100 ft
500*100

# sq ft to ha
# 1sqft / 107639 = 1 ha
500*100/107639
# 0.46 ha 
# plot is 0.46 ha

# we want 60 kg ha
# 1 kg = 2.205 lb
60*2.205
# 132.3 lbs of N per ha

132.3*0.46
# 60.86 lbs of N per plot

# urea is 46% N
# 1 lbs of urea = 0.46 lbs of N

60.86/.46
# 132 lbs of urea per plot

# urea is in 40 lb bags
132/40
# we want 3.3 bags per plot


## new school
library(measurements)


# we want 60 kg N per hectare applied to plots that are 500'x100' with urea
# plot dims = 500*100ft
# get to hectare
conv_unit(500*100,"ft2","hectare")

# 60 kg N /0.46 = kg of urea
60/.46
# 130.435 kg of urea per hectare
conv_unit(60/.46,"kg","lbs")
# 287 lbs of urea per hectare
287*conv_unit(500*100,"ft2","hectare")
# 133 lbs urea per plot


# units package -----------------------------------------------------------

install.packages("units")
library(units)

x = 1:3
class(x)
units(x) <- as_units("m/s")
class(x)
y = 2:5
a <- set_units(1:3, m/s) #easily used with mutate
a
units(a)
class(a)

units(a) <- make_units(km/h) #convert the unit

yield <- seq(from=1,
             to=20,
             by=0.5)

rnorm(100,50,10) -> dummy.yield
dummy.yield <- set_units(dummy.yield,g/m2)
units(dummy.yield)
units(dummy.yield) <- make_units(kg/hectare)
units(dummy.yield)



# convert to a mixed_units object:
a <- set_units(1:3, m/s) #easily used with mutate
units(a) = c("m/s", "km/h", "km/s")
a
# The easiest way to assign units to a numeric vector is like this:
x <- y <- 1:4
units(x) <- "m/s"  # meters / second

library(tidyverse)
y %>% 
  set_units(m/s) %>% 
  ggplot(aes(y)) +
  geom_histogram()




data("npk")
npk %>% 
  # glimpse()
  mutate(yield_units = as_units(yield,"kg/ha")) %>% 
  # glimpse()
  # str()
  # mutate(yield_units = yield_units/1000) %>% 
  # dplyr::select(yield_units)
  ggplot(aes(yield_units)) +
  stat_bin()






# measurements pakcage ----------------------------------------------------

install.packages("measurements")
library(measurements)

# rate is 60 kg N per hectare, what is rate per plot?
# plot dims = 500ft*100ft
conv_unit(500*100,"ft2","hectare")
# plot is 0.464 hectare
60*conv_unit(500*100,"ft2","hectare")
# apply 27.9 kg N per plot
# how many pounds of N per plot
conv_unit(60,"kg","lbs")
# 132.28 lbs per plot









conv_unit(132.3,"lbs","kg")
conv_unit(.46,"hectare","acre")
