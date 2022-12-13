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



read.delim("clipboard",header=F) -> corn
read.delim("clipboard", header=F) -> corn_plots

tibble(
  plot = corn_plots$V1,
  yield_g_plot = corn$V1
) -> dat3

# sample area? SAME AS SOYBEANS!!

dat3 %>% 
  # rename(
  #   yield_g_plot = yield_quadrat) %>% 
  mutate(yield_g_ha = yield_g_plot/conv_unit(76.2*2*300,"cm2","hectare"),
         yield_kg_ha = yield_g_ha*conv_unit(1,"g","kg"),
         yield_lbs_ac = yield_kg_ha*conv_unit(1,"kg","lbs")/
           conv_unit(1,"hectare","acre"),
         yield_bu_ac = yield_lbs_ac/56) %>% 
  summarise(mean = mean(yield_bu_ac),
            max = max(yield_bu_ac),
            min = min(yield_bu_ac))



# kernza yields -----------------------------------------------------------

read.csv("fert_yields_kernza_2022.csv") -> dat_kernza

# harvested with 2 36"x36" quadrats
# area basis

conv_unit(36*36,"inch2","acre")
dat_kernza %>% 
  mutate(
    yield_g_quadrat = Threshed_grain_no_bag.g.,
    yield_g_hectare = yield_g_quadrat/conv_unit(36*36,"inch2","acre"),
    yield_kg_hectare = yield_g_hectare*conv_unit(1,"g","kg"),
    yield_lb_acre = yield_kg_hectare*conv_unit(1,"kg","lbs")/conv_unit(1,"hectare","acre")
    ) 

# 3 rows at 36" length
# 1 row at 36*3/12
36*3/12
# 9 ft of row is our yield_g

# 1 row that's 43560 ft long represents an acre
# plot_yield/9*43560 = grams per acre by rowfoot method?



str_split(
  dat_kernza$trt_name,
  "_",
  simplify = T
  )[,1] %>% 
  str_sub(.,
          end = -2) %>% 
  as_tibble() %>% 
  mutate(rate = if_else(
    value=="40-40",
    "80",
    value
  )) %>% 
  bind_cols(dat_kernza) %>% 
  # View()
  mutate(
    yield_g_quadrat = Threshed_grain_no_bag.g.,
    yield_g_hectare = yield_g_quadrat/conv_unit(36*36,"inch2","acre"),
    yield_kg_hectare = yield_g_hectare*conv_unit(1,"g","kg"),
    yield_lb_acre = yield_kg_hectare*conv_unit(1,"kg","lbs")/conv_unit(1,"hectare","acre"),
    yield_g_acre_rowft = yield_g_quadrat/9*43560,
    yield_lbs_acre_rowft = yield_g_acre_rowft*conv_unit(1,"g","lbs"),
    yield_kg_hectare_rowft = yield_lbs_acre_rowft*
      conv_unit(1,"lbs","kg")/conv_unit(1,"acre","hectare")
  ) %>% 
  # View()
  # group_by(rate) %>%
  group_by(value) %>% 
  summarise(yield_area = mean(yield_kg_hectare),
            yield_rowft = mean(yield_kg_hectare_rowft)) %>% 
  arrange(yield_area)

# how does the rowft conversion produce a 2.5x yield estimate? 