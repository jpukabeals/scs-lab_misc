# 11Feb2022
# Jesse

# Find likely dates for cold snap after thaw using historical data in Rosemount
# This helps predict rainout shelter install dates

getwd()
setwd("C:/Users/pukab001/Downloads")
read.csv("DataDowloadCSV (3).csv")

# historical data for rosemount downloaded from 
# https://www.dnr.state.mn.us/climate/historical/daily-data.html?sid=217107&sname=ROSEMOUNT%20RESEARCH%20AND%20OUTREACH%20CENTER&sdate=por&edate=por
# downloaded today 11Feb2022

dat <- read.csv("DataDowloadCSV (3).csv")

library(tidyverse)
theme_set(theme_bw())

dat1 <- dat %>%
  # str()
  mutate(date=as.POSIXct(Date,format="%Y-%m-%d"),
         year=format(date,format="%Y"),
         month=format(date,format="%b")) %>%
  rename(temp.max.F="Maximum.Temperature.degrees..F.",
         temp.min.F="Minimum.Temperature.degrees..F.",
         snow.depth.inches="Snow.Depth..inches.") %>%
  dplyr::select(date,year,month,temp.max.F,
                temp.min.F,snow.depth.inches) 

dat1 %>%
  # str()
  filter(month=="Mar" | month=="Apr") %>%
  filter(date<=as.POSIXct("2020-12-25") &
           date>=as.POSIXct("2009-01-01")) %>%
  pivot_longer(.,
               cols=c(temp.max.F,temp.min.F,snow.depth.inches)) %>%
  # filter(name!="snow.depth") %>%
  mutate(value=as.numeric(value)) %>%
  drop_na %>%
  ggplot(aes(date,value,
             group=name,
             color=name)) +
  geom_line() +
  geom_hline(yintercept = 32) +
  # geom_vline(xintercept = as.POSIXct("2021-04-01")) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b") +
  theme(axis.text.x = element_text(angle=45,
                                   hjust = 1),
        legend.title = element_blank(),
        legend.position = "top") +
  facet_wrap(~year,
             scales = "free_x") +
  labs(x="",
       y="",
       title = "Rosemount Historical Spring Weather")

ggsave("parce_historical-weather-spring.png",
       width = 7,
       height = 6.5,
       units = "in",
       dpi=400)


# for every year, find first day in march when there is no snow
# for every year, find the first day in March/April where there is no snow and 
# the minimum temp below 25

dat1 %>%
  # str()
  mutate(year=format(date,format="%Y"))
