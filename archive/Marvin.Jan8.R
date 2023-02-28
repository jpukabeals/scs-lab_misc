

# Overview ----------------------------------------------------------------



# Install packages --------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)


# Import data ---------------------------------------------------------------
library(readxl)
dat <- read_excel("~/R data/Marvin.101-106.tidy.xls")


dat1 <- dat %>% 
  # str()
  rename_all(tolower) %>% 
  drop_na(plot) %>% 
  mutate(across(c(plot,id,`main seed`,
                  date,time),as.factor))

dat1 %>% 
  # distinct(plot)
  summary()
  # distinct(time) %>% 
  # View()
# 90 minutes for 6 plots
90/6
# 15 min per plot


# we take the sums of these multiple observations
dat2 <- dat1 %>%
  group_by(plot) %>%
  summarise(seeds.count=sum(`main seeds`),
            Weight.grams=sum(`weight(g)`))

dat2 %>%
  # summary()
  glimpse()




ggplot(dat2) +
  geom_density(aes(x=Seeds.count))

ggplot(dat1) +
  geom_jitter(aes(x=Plot,
                  y=`Main Seeds`),
              width = .01)


