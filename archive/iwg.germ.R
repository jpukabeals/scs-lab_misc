library(tidyverse)
getwd()
setwd("C:/Users/pukab001/Downloads")
dat <- readxl::read_excel("iwg.gtest.xlsx")

library(lattice)
xyplot(germ~date|rep,dat)

library(lubridate)


dat <- dat %>%
  group_by(rep) %>%
  mutate(cumgerm=cumsum(germ),
         gperc=cumgerm/total*100,
         day=1,
         cumday=cumsum(day)) %>%
  ungroup()

xyplot(gperc~date|rep,dat)
xyplot(gperc~cumday|rep,dat)

max(subset(dat,rep==1)$gperc)
max(subset(dat,rep==2)$gperc)

library(ggplot2)

dat$rep <- as.factor(dat$rep)

dat %>%
  ggplot(aes(cumday,gperc,
             group=rep,
             color=rep,
             shape=rep)) +
  geom_point()+
  geom_line()# +
  # geom_hline(yintercept = max(subset(dat,rep==1)$gperc)) +
  # geom_hline(yintercept = max(subset(dat,rep==2)$gperc)) +
  # geom_label(aes(0,95.8,label="95.8"))

dat %>%
  ggplot(aes(cumday,gperc,
             group=rep,
             color=rep,
             shape=rep)) +
  geom_point(size=3)+
  geom_line(size=1) +
  geom_hline(yintercept = max(subset(dat,rep==1)$gperc),linetype=3) +
  geom_hline(yintercept = max(subset(dat,rep==2)$gperc),linetype=3) +
  geom_label(aes(2,95.8,label="96%"),inherit.aes = F,label.size=NA) +
  geom_label(aes(2,86,label="86%"),inherit.aes = F,label.size=NA) +
  labs(x="Days",
       y="Germination (%)") +
  theme_bw() +
  scale_color_manual(values = c("#7fbf7b","#af8dc3"),name="Replicate") +
  scale_shape_discrete(name="Replicate")

ggsave("iwg.germ.png")
             
    