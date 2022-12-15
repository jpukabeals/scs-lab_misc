# FERT soybean yields

# install.packages("measurements")
library(measurements)
library(tidyverse)


# soybeans ----------------------------------------------------------------

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

read.delim("clipboard",header=F) -> soybeans
read.delim("clipboard",header=F) -> soybeans_plot

tibble(
  plot = soybeans_plot$V1,
  yield_quadrat = soybeans$V1
) -> dat2


# 

dat2 %>% 
  rename(
    yield_g_plot = yield_quadrat) %>% 
  mutate(yield_g_ha = yield_g_plot/conv_unit(76.2*2*300,"cm2","hectare"),
         yield_kg_ha = yield_g_ha*conv_unit(1,"g","kg"),
         yield_lbs_ac = yield_kg_ha*conv_unit(1,"kg","lbs")/
           conv_unit(1,"hectare","acre"),
         yield_bu_ac = yield_lbs_ac/60) %>% 
  # dplyr::select(plot,yield_kg_ha) %>% 
  # print()
  summarise(mean = mean(yield_bu_ac),
            kgha = mean(yield_kg_ha),
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
  # dplyr::select(plot,yield_kg_ha) %>% 
  # print()
  summarise(mean = mean(yield_bu_ac),
            kgha = mean(yield_kg_ha),
            max = max(yield_bu_ac),
            min = min(yield_bu_ac))



# kernza yields -----------------------------------------------------------

read.csv("fert_yields_kernza_2022.csv") -> dat_kernza

# harvested with 2 36"x36" quadrats
# area basis

conv_unit(36*36*2,"inch2","acre")
dat_kernza %>% 
  mutate(
    yield_g_quadrat = Threshed_grain_no_bag.g.,
    yield_g_hectare = yield_g_quadrat/conv_unit(36*36*2,"inch2","acre"),
    yield_kg_hectare = yield_g_hectare*conv_unit(1,"g","kg"),
    yield_lb_acre = yield_kg_hectare*conv_unit(1,"kg","lbs")/conv_unit(1,"hectare","acre")
    ) 

# 3 rows at 36" length and two quadrats
# 1 row at 36*3/12*2
36*3/12*2
# 18 ft of row is our yield_g

# 1 row that's 43560 ft long represents an acre
# plot_yield/18*43560 = grams per acre by rowfoot method?


# There is likely an error in this conversion causing yield estimation methods
# to be different, trying to find it. Think it has to do with hectare conversion


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
    yield_g_hectare = yield_g_quadrat/conv_unit(36*36*2,"inch2","hectare"),
    yield_kg_hectare = yield_g_hectare*conv_unit(1,"g","kg"),
    yield_lb_acre = yield_kg_hectare*conv_unit(1,"kg","lbs")/conv_unit(1,"hectare","acre"),
    yield_g_acre_rowft = yield_g_quadrat/18*43560,
    yield_lbs_acre_rowft = yield_g_acre_rowft*conv_unit(1,"g","lbs"),
    yield_kg_hectare_rowft = yield_lbs_acre_rowft*
      conv_unit(1,"lbs","kg")/conv_unit(1,"acre","hectare")
  ) %>% 
  # View()
  # group_by(rate) %>%
  # colnames
  # distinct(trt_name)
  # filter(trt_name == "80N_spring" |
  #          # trt_name == "80N_spring_noP" |
  #          # trt_name == "80N_spring_noPK" |
  #          trt_name == "0N_spring") %>% 
  # dplyr::select(plot_code,trt_name,yield_kg_hectare_rowft) %>% 
  # print(n=50)
  # group_by(value) %>% 
  summarise(yield_area = mean(yield_kg_hectare),
            yield_rowft = mean(yield_kg_hectare_rowft)) %>% 
  arrange(yield_area) %>% 
  mutate(yield_area = round(yield_area,0),
         yield_rowft = round(yield_rowft,0)) %>% 
  # write.csv("clipboard")
  print()

# how does the rowft conversion produce a 2.5x yield estimate? 



# nrate timing Staples was done using row feet
# plot yield from 1x 24" x 24" quadrat is 19 grams
# 6 inch row spacing
# what is kg ha?

# you have 4 rows, each are 2 feet long
# same as 1 row that 8 feet long
# divide the plot by weight by 8, and you have yield of a single row-foot
# acre is 43560 ft2, multiply that row foot by 43560 to get yield of an acre

# 19/8*43560 = grams per acre
19.4/8*43560/conv_unit(1,"acre","hectare")*
  conv_unit(1,"g","kg")
# I estimate 261


# dominics conversion
19.4*656*(3936/96)*0.001
# dominic estimates 521

# let's try to understand

# 1 ha is 107639 square feet. If square, that is 328 ft by 328 ft (or 3936" by
# 3936"). Samples from 24" by 24" quadrat: That's four rows, each 24" long.
# There are 656 rows in a square ha. We sampled 4 rows, each 24" totaling 96".
# Mulitply sample by 656 * (3936/96)*0.001


conv_unit(1,"hectare","ft2")
328*328 #is is not the same as a hectare, off by about 100 square feet
328*12/6
# THIS IS SUCH A CONFUSING WAY TO THINK ABOUT THIS CONVERSION

