library(tidyverse)

getwd()
setwd("C:/Users/pukab001/Downloads")

dat <- readxl::read_excel("sack.data.xlsx")

library(lattice)

bwplot(dm.crop.Mg.ha~trt,dat)
bwplot(dm.weed.Mg.ha~trt,dat)
bwplot(den.crop.ct.m2~trt,dat)
bwplot(den.weed.ct.m2~trt,dat)
bwplot(den.weed.ct.m2.2~trt,dat)
bwplot(height.weed.m~trt,dat)




a<-bwplot(dm.crop.Mg.ha~trt,dat,ylab = expression("Crop dry matter" ~ (Mg ~ ha^{-1})))
b<-bwplot(dm.weed.Mg.ha~trt,dat,ylab = expression("Weed dry matter" ~ (Mg ~ ha^{-1})))
c<-bwplot(den.crop.ct.m2~trt,dat,ylab = expression("Crop stand density" ~ (plants ~ m^{-2})))
d<-bwplot(den.weed.ct.m2~trt,dat,ylab = expression("Weed density" ~ (plants ~ m^{-2})))

library(ggplot2)

install.packages("cowplot")
library(cowplot)

plot_grid(a,b,c,d)
ggsave("sack.summer2021.png",
       width=7,
       height = 5.5,
       units = "in",
       dpi=400)
# png("sack.summer2021.png",width = 7,height = 6.5,units = "in", res = 400)
# plot_grid(a,b,c,d)
# dev.off()


dat %>%
  ggplot(aes(trt,dm.crop.Mg.ha)) +
  geom_point() 

dat %>%
  ggplot(aes(trt,dm.weed.Mg.ha)) +
  geom_boxplot()
