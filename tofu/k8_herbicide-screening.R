library(agricolae)
library(tidyverse)

setwd("C:/Users/pukab001/Documents/R projects/scs-lab_misc\\tofu")






# plot dimensions calcs ---------------------------------------------------

# Herbicide IR4 experiment plot dimensions were 12' wide by 15' long. 

# We want this experiment to be smaller



# bicycle sprayer can do 80", 120" and 160"
# 80" is easiest

80/12
# Spray area is 6.6'
# round to plots at 6'

# kernza at 12" row spacing
# 6 rows of kernza
# sample middle 4 rows
6*12


# plots are 6' wide
# plots 10' long
# actual spray area is 6.6' x 10' or 80" x 120"
80/12*120/12
# 67 sq ft per experimental unit

# assumping 3 reps 
67*3
# 200 sq ft
 
200/43560
# 0.004591368 acres

# I typically put out 16.6 GPA with bicycle sprayer at current settings

# fluid oz per sq ft 
16.6/43560*128
# I need 0.5 fl oz per sq ft I spray

16.6/43560*128*67
# I need 3 fl oz per plot

# mL per sq ft
16.6/43560*3785.41
# I need 1.4 mL per sq ft I spray

16.6/43560*3785.41*67
# I need 97 mL per plot


# PRE treatments 
3*7
# 21 fl oz of spray volume per PRE treatment
97*7/1000
# 0.68 L of spray volume per PRE treatment

# POST
3*4
# 12 fl oz of spray volume per POST treatment

97*4/1000
# 0.4 L of spray volume per PRE treatment





# hopeful treatment list --------------------------------------------------

## PREs
# Dual (s-metolachor)
# Warrant/Harness (acetochlor)
# Prowl (pendamethalin)
# Valor 

## POST
# tacoma (fenoxaprop)
# axial XL (pinoxaden)
# axial bold (fenoxaprop + pinoxaden)

# bromac (bromoxynil)
# bison (bromoxynil + mcpa)

# wolverine (fenoxaprop + bromoxynil + pyrasulfotole)
# Zidua (pyroasulfone)

# banvel (dicamba)
# facet L (quinclorac)


# realistic treatment list ------------------------------------------------
## PREs
# Dual (s-metolachor)
# Warrant/Harness (acetochlor)
# Prowl (pendamethalin)
# **Don added Boundary herbicide

## POST
# wolverine (fenoxaprop + bromoxynil + pyrasulfotole) or Wolverine Advance
# axial bold (fenoxaprop + pinoxaden)
# bison (bromoxynil + mcpa)
# banvel or generic (dicamba)
# facet L (quinclorac)


# design ------------------------------------------------------------------

# 19Apr2022 adjustments....
# added boundary as a PRE treatment for Don V. He wanted to see some damage.
# changed axial bold to axial XL for Don V
# I think axial bold would've been better, but Don thinks fenoxoprop is well established as safe.



# reps = 3
# split plot design
# whole plot = PRE (5 levels)
# sub plot = POST (7 levels)

PRE <- c("dual","prowl",
         "acetochlor","weedy control",
         "boundary")
POST <- c("wolverine",
          "axial XL",
          "bison",
          "dicamba",
          "facet L",
          "weedy control",
          "weed-free control")


agricolae::design.split(
  trt1 = PRE,
  trt2 = POST,
  r=1,
  design = "crd",
  serie = 0,
  seed = 314,
  kinds = "Super-Duper",
  first = T,
  randomization = T
) %>% 
  .$book %>% 
  mutate(experimental_unit = paste(plots,splots,sep = "0"),
         .before=plots) %>% 
  write.csv("k8_herbicide-screening-randomization.csv",
            row.names = F)



# dummy data --------------------------------------------------------------

# PRE <- c("dual","prowl",
#          "acetochlor","weedy control",
#          "boundary")
# POST <- c("wolverine",
#           "axial XL",
#           "bison",
#           "dicamba",
#           "facet L",
#           "weedy control",
#           "weed-free control")


# agricolae::design.split(
#   trt1 = PRE,
#   trt2 = POST,
#   r=1,
#   design = "crd",
#   serie = 0,
#   seed = 314,
#   kinds = "Super-Duper",
#   first = T,
#   randomization = T
# ) %>% 
#   .$book %>% 
#   mutate(experimental_unit = paste(plots,splots,sep = "0"),
#          .before=plots) %>% 
#   dplyr::select(-c(plots,splots,r)) -> df 

# weed scores where 10 is a lot of weeds and 0 is no weeds
# weed scorse in may are a couple weeks after application
# weed_score_may <-
#   c(
#     # acetocholor
#     rnorm(7,2,.2),
#     # boundary
#     rnorm(7,4,.5),
#     # dual
#     rnorm(7,1,.1),
#     # prowl
#     rnorm(7,1.5,.7),
#     # weedy control
#     rnorm(7,9,.1)
#   )

# weed ground cover will be difficult to distinguish IWG vs weeds 
# will focus only on interrow area
# weed_ground.cover_may <-
#   c(
#     # acetocholor
#     rnorm(7,5,1),
#     # boundary
#     rnorm(7,15,3),
#     # dual
#     rnorm(7,2,.5),
#     # prowl
#     rnorm(7,12,2),
#     # weedy control
#     rnorm(7,80,8)
#   )

# weed counts on a meter square basis
# weed_counts_may <-
#   c(
#     # acetocholor
#     rnorm(7,2,1),
#     # boundary
#     rnorm(7,7,3),
#     # dual
#     rnorm(7,3,.5),
#     # prowl
#     rnorm(7,5,2),
#     # weedy control
#     rnorm(7,20,8)
#   )

# df %>% 
#   arrange(PRE,POST) %>% 
#   mutate(weed_score_may=weed_score_may,
#          weed_ground.cover_may = weed_ground.cover_may,
#          weed_counts_may = weed_counts_may) -> df_wide


# real data ---------------------------------------------------------------


read.csv("tofu_data-all.csv") -> dat_all

dat_all %>%
  filter(date==unique(dat_all$date)[1]) %>% 
  dplyr::select(-c(yield_kgha,shattering_vis,shattering_countm2)) -> dat_april

dat_all %>%
  filter(date==unique(dat_all$date)[2]) %>% 
  dplyr::select(-c(yield_kgha,shattering_vis,shattering_countm2)) ->  dat_june

dat_all %>%
  filter(date==unique(dat_all$date)[3]) %>% 
  dplyr::select(-c(weed_vis_score,iwg_injury,person, shattering_vis,shattering_countm2))-> dat_yield

dat_all %>%
  filter(date==unique(dat_all$date)[4]) %>% 
  dplyr::select(-c(weed_vis_score,iwg_injury,person, yield_kgha)) %>% 
  mutate(shattering_countm2 = round(shattering_countm2)) -> dat_shattering

# library(googlesheets4)
# url <- "https://docs.google.com/spreadsheets/d/1OhZ2Onva0yYqOb9LaS7dYVupmTNv4EJx-jcKqUueQ7w/edit#gid=123579174"

# read_sheet(url,
#            sheet=4,
#            gs4_deauth()) -> dat
# 
# 
# dat %>% 
#   slice(36:140) -> dat_june
# real data spring --------------------------------------------------------

# dat %>% 
#   slice(1:35) -> dat_april

# real data yield ---------------------------------------------------------

# read_sheet(
#   url,
#   sheet = 5,
#   gs4_deauth()) -> dat1


# quadrats were 24" x 24"

library(measurements)

# conv_unit(24*24,
#           "inch2",
#           "hectare")
# 
# dat1 %>% 
#   # glimpse()
#   mutate(yield_kgha = `threshed grain no bag(g)`/1000/conv_unit(24*24,
#                                                            "inch2",
#                                                            "hectare")) %>% 
#   dplyr::select(experimental_unit,
#                 date,
#                 PRE,
#                 POST,
#                 yield_kgha) -> dat_yield
  
# real data spring following year -----------------------------------------

# this is data collected in spring of 2023 after I noticed differences in
# germinating shattered IWG

# read_sheet(
#   url,
#   sheet = 6,
#   gs4_deauth()) -> dat2
# 
# dat2 %>% 
#   # glimpse()
#   mutate(shattering_countm2 = shattering_count/conv_unit(1.625*1.625,
#                                                                 "inch2",
#                                                                 "m2")) %>% 
#   dplyr::select(-shattering_count)-> dat_shattering 

# combined ----------------------------------------------------------------


# dat %>% 
#   bind_rows(dat_yield) %>% 
#   bind_rows(dat_shattering)-> dat_all

# write.csv(
#   dat_all,
#   "tofu_data-all.csv",
#   row.names = F
# )


# correlation plot
library(corrplot)

# dat_all %>%
#   filter(person == "jesse") %>% 
#   dplyr::select(iwg_injury,yield_kgha) %>% 
#   cor(method = "pearson") %>% 
#   # print
#   corrplot()

# person coefficient of -.2 or -20%. As injury increases, yield does decrease.
# 20% is on the border between a weak and moderate relationship

# this is what we expect. 

# anovas

library(multcomp)
library(emmeans)
library(stringr)

# PRE

dat_yield %>% 
  lm(yield_kgha~PRE,.) %>%
  # anova()
  emmeans::emmeans(~PRE) %>% 
  cld(Letters=letters,
      reverse=T) %>% 
  mutate(group = str_trim(.group)) %>% 
  dplyr::select(PRE,group) -> dum1

dat_yield %>% 
  group_by(PRE) %>% 
  summarise(yield = mean(yield_kgha),
            SD = sd(yield_kgha)) %>% 
  left_join(dum1) %>% 
  arrange(desc(yield)) %>% 
  mutate(`Yield (kg ha)` = paste(round(yield,0),
                        group,
                        sep = " ")) %>% 
  dplyr::select(PRE,`Yield (kg ha)`) %>% 
  knitr::kable()

#  POST 

dat_yield %>% 
  lm(yield_kgha~POST,.) %>%
  # anova()
  emmeans::emmeans(~POST) %>% 
  cld(Letters=letters,
      reverse=T) %>% 
  mutate(group = str_trim(.group)) %>% 
  dplyr::select(POST,group) -> dum2

dat_yield %>% 
  group_by(POST) %>% 
  summarise(yield = mean(yield_kgha),
            SD = sd(yield_kgha)) %>% 
  left_join(dum2) %>% 
  arrange(desc(yield)) %>% 
  mutate(`Yield (kg ha)` = paste(round(yield,0),
                                 group,
                                 sep = " ")) %>% 
  dplyr::select(POST,`Yield (kg ha)`) %>% 
  knitr::kable()

# shattering

dat_shattering %>% 
  lm(shattering_vis~PRE+POST,.) %>% 
  # anova()
  emmeans(~POST, level = .9) %>% 
  cld(alpha = 0.1,
      Letters =letters,
      reverse=T) %>% 
  mutate(group = str_trim(.group)) %>% 
  dplyr::select(POST,group) -> dum3

# shattering --------------------------------------------------------------

dat_shattering %>% 
  dplyr::select(
    shattering_vis,shattering_countm2
  ) %>% 
  cor() 

dat_shattering %>% 
  filter(shattering_vis<10) %>% 
  ggplot(aes(shattering_vis,
             shattering_countm2)) +
  geom_point() +
  geom_smooth(
    span = 1.5,
    se=F
  )

dat_shattering %>% 
  ggplot(aes(shattering_vis)) +
  stat_bin()


# aov() -------------------------------------------------------------------

# aov(weed_score_may~PRE,
#     df_wide) %>% 
#   summary()
#   # LSD.test(trt = "PRE") %>% 
#   # print()
# 
# aov(weed_ground.cover_may~PRE,
#     df_wide) %>% 
#   summary()
# 
# aov(weed_counts_may~PRE,
#     df_wide) %>% 
#   summary()




  
setwd("C:/Users/pukab001/Documents/R projects/scs-lab_misc")



