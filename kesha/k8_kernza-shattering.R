
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
sub_plot = c("control", "dual","prowl", "dual_2X",
               "acetochlor", "callisto", "atrazine","boundary")
whole_plot = c("mow", "harvest","overseed")

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


# design 3 reps -----------------------------------------------------------

library(agricolae)
library(tidyverse)
sub_plot = c("control", "dual","prowl", "dual_2X",
             "acetochlor", "callisto", "atrazine","boundary")
whole_plot = c("mow", "harvest","overseed")

design.split(
  trt1 = whole_plot,
  trt2 = sub_plot,
  r=3,
  design = "crd",
  serie = 0,
  seed = 314,
  kinds = "Super-Duper",
  first = T,
  randomization = T) %>% 
  .$book %>% 
  mutate(plots=plots+9) %>% 
  mutate(experimental_unit = paste(plots,"_",splots,sep = ""),
         .before=plots) %>% 
  write.csv("k8_kesha_3-rep.csv",
            row.names = F)


# dataframe and dummy data ------------------------------------------------

design.split(
  trt1 = whole_plot,
  trt2 = sub_plot,
  r=4,
  design = "crd",
  serie = 0,
  seed = 314,
  kinds = "Super-Duper",
  first = T,
  randomization = T) %>% 
  .$book %>% 
  mutate(plots=plots+9) %>% 
  mutate(experimental_unit = paste(plots,"_",splots,sep = ""),
         .before=plots) -> df

df %>% 
  dplyr::select(-c(splots,r)) %>% 
  arrange(whole_plot,sub_plot) -> df


# dummy data - shattering score -------------------------------------------

# shattering score
# 1 is no shattering, 5 is a lot of shattering

set.seed(314)
c(
  # acetochlor harvest
  rnorm(4,2,1) %>% 
    round(.,0),
  # acetochlor mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # atrzaine harvest
  rnorm(4,2.5,1) %>% 
    round(.,0),
  # atrazine mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # callisto harvest
  rnorm(4,1.5,1) %>% 
    round(.,0),
  # callisto mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # control harvest
  rnorm(4,5,1) %>% 
    round(.,0),
  # control mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # dual harvest
  rnorm(4,2,1) %>% 
    round(.,0),
  # dual mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # dual 2x harvest
  rnorm(4,1.3,1) %>% 
    round(.,0),
  # dual 2x mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # prowl harvest
  rnorm(4,1.6,1) %>% 
    round(.,0),
  # prowl mow
  rnorm(4,1,0) %>% 
    round(.,0)
) %>% 
  pmax(1) %>% #all values below 1 are rounded up to 1
  pmin(5) -> shat_score # all values above 5 are rounded to 5


# dummy data - grid counts of shattered kernza plants --------------------------
# range of 0 to 25
set.seed(314)
c(
  # acetochlor harvest
  rnorm(4,8,5) %>% 
    round(.,0),
  # acetochlor mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # atrzaine harvest
  rnorm(4,6,5) %>% 
    round(.,0),
  # atrazine mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # callisto harvest
  rnorm(4,10,8) %>% 
    round(.,0),
  # callisto mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # control harvest
  rnorm(4,22,1) %>% 
    round(.,0),
  # control mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # dual harvest
  rnorm(4,12,6) %>% 
    round(.,0),
  # dual mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # dual 2x harvest
  rnorm(4,4,1) %>% 
    round(.,0),
  # dual 2x mow
  rnorm(4,1,0) %>% 
    round(.,0),
  # prowl harvest
  rnorm(4,10,4) %>% 
    round(.,0),
  # prowl mow
  rnorm(4,1,0) %>% 
    round(.,0)
) %>% 
  pmax(0) %>% #all values below 1 are rounded up to 1
  pmin(25) -> grid_counts



# dummy data - yield ------------------------------------------------------

# kg ha
# year 2 yields should be near 400 kg ha


set.seed(314)
c(
  # acetochlor harvest
  rnorm(4,400,50) %>% 
    round(.,0),
  # acetochlor mow
  rnorm(4,420,50) %>% 
    round(.,0),
  # atrzaine harvest
  rnorm(4,400,50) %>% 
    round(.,0),
  # atrazine mow
  rnorm(4,420,50) %>% 
    round(.,0),
  # callisto harvest
  rnorm(4,400,50) %>% 
    round(.,0),
  # callisto mow
  rnorm(4,420,50) %>% 
    round(.,0),
  # control harvest
  rnorm(4,350,50) %>% 
    round(.,0),
  # control mow
  rnorm(4,420,50) %>% 
    round(.,0),
  # dual harvest
  rnorm(4,400,60) %>% 
    round(.,0),
  # dual mow
  rnorm(4,420,50) %>% 
    round(.,0),
  # dual 2x harvest
  rnorm(4,400,50) %>% 
    round(.,0),
  # dual 2x mow
  rnorm(4,400,50) %>% 
    round(.,0),
  # prowl harvest
  rnorm(4,400,40) %>% 
    round(.,0),
  # prowl mow
  rnorm(4,420,50) %>% 
    round(.,0)
)  -> yields_yr2

# kesha dataframe ---------------------------------------------------------

df %>% 
  mutate(shat_score = shat_score,
         grid_counts = grid_counts,
         yields_yr2 = yields_yr2) -> ke

# adding units to yield - crashed R on home laptop
# install.packages("units")
# library(units)


# visualize ---------------------------------------------------------------

## shat_score
ke %>% 
  ggplot(aes(shat_score)) +
  # stat_bin()
  # stat_bin(aes(fill=sub_plot)) 
  stat_bin(aes(fill=whole_plot))


## grid counts
ke %>% 
  # filter(sub_plot!="mow" &
           # whole_plot!="control") %>% 
  ggplot(aes(grid_counts)) +
  stat_bin()
  # stat_bin(aes(fill=sub_plot))
  # stat_bin(aes(fill = whole_plot))
  # geom_density(aes(col=whole_plot))

## yields_yr2
ke %>% 
  # filter(sub_plot != "mow") %>% 
  # mutate(whole_plot = fct_reorder(whole_plot,
  #                                 yields_yr2)) %>% 
  ggplot(aes(yields_yr2)) +
  stat_bin()
  # stat_bin(aes(fill=whole_plot))
  # stat_bin(aes(fill=sub_plot))
  # geom_boxplot(aes(fill=whole_plot)) +
  # theme(axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank())
  # geom_density(aes(color=whole_plot,
  #                  # fill=whole_plot
  #                  ),
  #              bw=40,
  #              alpha=.1) 



# aov () ------------------------------------------------------------------

# are grid counts different between whole plots that were harvested

# grid_counts is discrete response that likely does not meet assumptions of
# anova but we'll do the simple easy way first

# looking only at grid_counts in plots that were harvested
ke %>% 
  filter(sub_plot == "harvest") %>% 
  aov(grid_counts~whole_plot,.) %>% 
  # summary()
  LSD.test(trt = "whole_plot",
           # p.adj = "bonferroni"
           ) %>% 
  # print()
  .$groups %>% 
  mutate(herbicide = attributes(.)$row.names,
         .before = grid_counts) %>% 
  mutate(herbicide = fct_reorder(herbicide,desc(grid_counts))) %>% 
  ggplot(aes(herbicide,grid_counts,
             label = groups)) +
  stat_summary(geom="bar") +
  geom_text(vjust=-1) +
  scale_y_continuous(limits = c(0,25)) 

# grid counts by full factorial of herbicide and harvest
ke %>% 
  aov(grid_counts~whole_plot*sub_plot,.) %>% 
  # summary() #three way interaction
  LSD.test(trt = c("whole_plot","sub_plot"),
           p.adj = "bonferroni"
  ) %>% 
  # print()
  .$groups %>% 
  mutate(herbicide = attributes(.)$row.names,
         .before = grid_counts) %>% 
  mutate(herbicide = fct_reorder(herbicide,desc(grid_counts))) %>% 
  ggplot(aes(herbicide,grid_counts,
             label = groups)) +
  stat_summary(geom="bar") +
  geom_text(hjust=-1) +
  scale_y_continuous(limits = c(0,25)) +
  coord_flip()


ke %>% 
  group_by(whole_plot) %>% 
  aov(grid_counts~sub_plot,.) %>% 
  summary()



## yields_yr2
ke %>% 
  filter(sub_plot == "harvest") %>% 
  aov(grid_counts~whole_plot,.) %>% 
  # summary()
  LSD.test(trt = "whole_plot",
           # p.adj = "bonferroni"
  ) %>% 
  # print()
  .$groups %>% 
  mutate(herbicide = attributes(.)$row.names,
         .before = grid_counts) %>% 
  mutate(herbicide = fct_reorder(herbicide,desc(grid_counts))) %>% 
  ggplot(aes(herbicide,grid_counts,
             label = groups)) +
  stat_summary(geom="bar") +
  geom_text(vjust=-1) +
  scale_y_continuous(limits = c(0,25))

