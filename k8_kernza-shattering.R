
# treatments --------------------------------------------------------------

# Whole plot treatment = PRE's
# none, dual,prowl,acetochlor, atrazine, callisto, boundary

# Split plot treatment = harvest-method
# hand-harvest with rice knives
# mow kernza so no seed production

# additional treatments --------------------------------------------------------

# combine harvest
# bcs single-row harvester
# swathing

# specs of equipment ------------------------------------------------------

# sprayer minimum width is 80" or 6.6'
# make plots 6' on center and then spray 6.6'. 
# 6' plot reduced to 5.4' plot due to overlap of spray from adjacent plot
# we only sample middle 2 rows anyways though, so we're fine. 

# mower can fit any size plot
# we can mow access to plots along edges


# design ------------------------------------------------------------------

library(agricolae)
library(tidyverse)
whole_plot = c("control", "dual","prowl", "dual_2X",
               "acetochlor", "callisto", "atrazine")
sub_plot = c("mow", "harvest")

design.split(
  trt1 = whole_plot,
  trt2 = sub_plot,
  r=1,
  design = "crd",
  serie = 0,
  seed = 314,
  kinds = "Super-Duper",
  first = T,
  randomization = T) %>% 
  .$book %>% 
  mutate(experimental_unit = paste(plots,"0",splots,sep = ""),
         .before=plots) %>% 
  write.csv("k8_kesha.csv",
            row.names = F)

