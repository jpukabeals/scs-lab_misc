# FERT soybean yields

install.packages("measurements")
library(measurements)

# 30 inch spacing
conv_unit(30,"inch","cm")

# 300 cm length

# measured 2 rows, rows were 76.2 cm apart and 300 cm long
# area is 76.2*2*300

76.2*2*300
# area on a square cm2 basis

conv_unit(76.2*2*300,"cm2","hectare")

?conv_unit

# https://docs.google.com/spreadsheets/d/1CEjByLdcfwR75t9rhGyG9KGahzjVLcRMQ5fiWQbjJTg/edit#gid=0

read.delim("clipboard",header=F) -> dat
read.delim("clipboard",header=F) -> names_plot

tibble(
  plot = names_plot$V1,
  yield_quadrat = dat$V1
) -> dat2

library(tidyverse)

# 

dat2 %>% 
  rename(
    yield_g_plot = yield_quadrat) %>% 
  mutate(yield_g_ha = yield_g_plot/conv_unit(76.2*2*300,"cm2","hectare"),
         yield_kg_ha = yield_g_ha*conv_unit(1,"g","kg"),
         yield_lbs_ac = yield_kg_ha*conv_unit(1,"kg","lbs")/
           conv_unit(1,"hectare","acre"),
         yield_bu_ac = yield_lbs_ac/60) %>% 
  summarise(mean = mean(yield_bu_ac),
            max = max(yield_bu_ac),
            min = min(yield_bu_ac))
