
# Looking at plant count data
# collected 4Nov 2022
# entered into google drive

url <- "https://docs.google.com/spreadsheets/d/1KWJQ7qcqVZyjV-tf0MPXlvSAWSjIIMZHnlr1qYm6dKY/edit#gid=0"

library(googlesheets4)

read_sheet(url) -> dat

library(tidyverse)

dat %>% 
  glimpse()

dat %>% 
  mutate(plants_per_meter = 2*`iwg_plants-per-0.5-meter-row`) %>% 
  glimpse()

dat %>% 
  mutate(plants_per_meter = 2*`iwg_plants-per-0.5-meter-row`) %>% 
  mutate(treatment=as_factor(treatment)) %>% 
  mutate(treatment=fct_recode(treatment,
                              "No-till" = "fall_nt",
                              "Convention till" = "fall_ct"))  -> dat1

dat1 %>% 
  group_by(treatment) %>% 
  summarise(plants_density = round(mean(plants_per_meter),1),
            sd = sd(plants_per_meter),
            n=n()) -> dat2;dat2

set.seed(314)
dat1 %>% 
  left_join(.,dat2) %>% 
  ggplot(aes(treatment, plants_per_meter)) +
  geom_jitter(width = .1) +
  geom_boxplot(fill=NA,
               width=.1) +
  geom_label(aes(label=plants_density,
                y=20),
            stat = "summary",
            # label.size = NA
            ) + 
  theme_bw() +
  labs(y="IWG stand counts\n(plants per meter)",
       x="",
       caption = "treatment means displayed in figure\ndata collected 4Nov2022\nData collected from row sections with highest plant density\nIWG stands in conventional till appeared worse than in no-till on a plot level")
ggsave("atat_plant-counts.png",
       width=5,
       height=4,
       units = "in")  
