getwd()
setwd("~/ibutton")

temp = list.files(pattern="*.csv")
temp[1]
#all file names are 16 characters

library(stringr)
# str_sub(temp[1], start = 1L, end = -12L)


# dat1 <- read.csv("~/ibutton/0400000051660F41_110821.csv",
#                  skip=20,header = F)
# 
# paste("~/ibutton/",temp[1], sep = "")
# 
# read.csv(paste("~/ibutton/",temp[1], sep = ""), skip=20,header = F)

# temp1 <- str_sub(temp[1], start = 1L, end = -12L)
temp1 <- read.csv(paste("~/ibutton/",temp[1], sep = ""), skip=20,header = F)
temp1$id <- str_sub(temp[1], start = 1L, end = -12L)


# temp2 <- str_sub(temp[2], start = 1L, end = -12L)
temp2 <- read.csv(paste("~/ibutton/",temp[2], sep = ""), skip=20,header = F)
temp2$id <- str_sub(temp[2], start = 1L, end = -12L)


temp1.2 <- dplyr::full_join(temp1,temp2)

# for (i in 1:length(temp)){
#   t[i] <- read.csv(paste("~/ibutton/",temp[i], sep = ""), skip=20,header = F)
#   t[i]$id <- str_sub(temp[i], start = 1L, end = -12L)
# }



# for loop ----------------------------------------------------------------
# 
# temp = list.files(pattern="*.csv")
# 
# for (i in 1:length(temp)){
#   t <- read.csv(paste("~/ibutton/",temp[i], sep = ""), skip=20,header = F)
#   t$id <- str_sub(temp[i], start = 1L, end = -12L)
#   t[[i]] <- t
# }
# View(t)

datalist = list(temp)

for (i in 1:length(temp)) {
  # dat <- data.frame(x = 16), y = 5)
  t <- read.csv(paste("~/ibutton/",temp[i], sep = ""), skip=20,header = F)
  t$id <- str_sub(temp[i], start = 1L, end = -12L)
  datalist[[i]] <- t # add it to your list
}

dat.all = do.call(rbind, datalist)


# visualize ---------------------------------------------------------------

library(lubridate)
dat.all$V2
parse_date_time(dat.all$V2)
parse_date_time(dat.all$V2, '%I:%M:%S %p')

hms(dat.all$V2)
hms::as_hms(dat.all$V2)
as.numeric(hms::as_hms(dat.all$V2))
as.POSIXct(hms::as_hms(dat.all$V2))

hms::as_hms(85)

library(tidyverse)

dat.all %>%
  mutate(time=parse_date_time(dat.all$V2, '%I:%M:%S %p')) %>%
  ggplot(aes(x=time)) +
  geom_point(aes(y=V4)) +
  facet_wrap(~id)

dat.all %>%
  mutate(time=parse_date_time(dat.all$V2, '%I:%M:%S %p')) %>%
  filter(id=="F400000051647A41") %>%
  ggplot(aes(x=time)) +
  geom_point(aes(y=V4)) +
  facet_wrap(~id)

dat.all %>%
  mutate(time=parse_date_time(dat.all$V2, '%I:%M:%S %p')) %>%
  filter(id!="F400000051647A41") %>%
  ggplot(aes(x=time)) +
  geom_point(aes(y=V4)) +
  facet_wrap(~id)


dat.all %>%
  mutate(time=parse_date_time(dat.all$V2, '%I:%M:%S %p')) %>%
  filter(id!="F400000051647A41") %>%
  ggplot(aes(x=time)) +
  geom_line(aes(y=V4)) +
  facet_wrap(~id)


dat.all %>%
  mutate(time=parse_date_time(dat.all$V2, '%I:%M:%S %p')) %>%
  filter(id!="F400000051647A41") %>%
  ggplot(aes(x=time,
             group=id,
             color=id)) +
  geom_line(aes(y=V4)) +
  labs(y="Temperature (C)",
       x="Time of Day on 8Nov2021")
