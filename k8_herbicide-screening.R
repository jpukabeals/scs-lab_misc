library(agricolae)


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

## POST
# wolverine (fenoxaprop + bromoxynil + pyrasulfotole) or Wolverine Advance
# axial bold (fenoxaprop + pinoxaden)
# bison (bromoxynil + mcpa)
# banvel or generic (dicamba)
# facet L (quinclorac)


# design ------------------------------------------------------------------

# reps = 3
# split plot design
# whole plot = PRE (4 levels)
# sub plot = POST (7 levels)

PRE <- c("dual","prowl",
         "warrant/harness","weedy control")
POST <- c("wolverine",
          "axial bold",
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
  seed = 123,
  kinds = "Super-Duper",
  first = T,
  randomization = T
) %>% 
  .$book %>% 
  mutate(experimental_unit = paste(plots,splots,sep = "0"),
         .before=plots) %>% 
  write.csv("k8_herbicide-screening-randomization.csv",
            row.names = F)



