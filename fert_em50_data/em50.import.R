
# downloaded data from em50 datalogger should be saved using the default
# name suggested by the program in the folder on the blue tablet specified
# on line 9 , i.e. "C:/Users/pukab001/Documents/fer.em50.data/"
# this script will take care of the rest --Jesse

# import from blue tablet -------------------------------------------------

setwd("C:/Users/pukab001/Documents/fer.em50.data/")
temp = list.files(pattern="*.txt")
datalist = list(temp)
library(stringr)
for (i in 1:length(temp)) {
  t <- read.delim(paste("~/fer.em50.data/",temp[i], sep = ""), header = T)
  t$plot <- str_sub(temp[i],1,7)
  t$date <- str_sub(temp[i],8,15)
  datalist[[i]] <- t # add it to your list
}
dat.all = do.call(rbind, datalist)

library(lubridate)
library(tidyverse)
dat.tidy <- dat.all %>%
  mutate(timepoint=parse_date_time(Measurement.Time, orders = '%m/%d/%y %I:%M %p'),
         time=parse_time(str_sub(Measurement.Time,10,17)),
         date=dmy(date),
         plot=factor(plot)) %>%
  mutate(vwc.20=as.numeric(`Port.1.5TM.Moisture.Temp.m³.m³.VWC`),
         vwc.40=as.numeric(`Port.2.5TM.Moisture.Temp.m³.m³.VWC`),
         vwc.60=as.numeric(`Port.3.5TM.Moisture.Temp.m³.m³.VWC`),
         temp.20=as.numeric(Port.1.5TM.Moisture.Temp..C.Temp),
         temp.40=as.numeric(Port.2.5TM.Moisture.Temp..C.Temp),
         temp.60=as.numeric(Port.3.5TM.Moisture.Temp..C.Temp)) %>%
  select(timepoint,time,plot,vwc.20,vwc.40,vwc.60,temp.20,temp.40,temp.60)
#NA's introduced are due to missing values when sensors malfunctioned

dat.tidy.vwc <- dat.tidy %>%
  select(-temp.20,-temp.40,-temp.60) %>%
  rename(`20`=vwc.20,
         `40`=vwc.40,
         `60`=vwc.60) %>%
  pivot_longer(
    cols = c(`20`,`40`,`60`),
    names_to = "depth",
    values_to = "vwc") 
dat.tidy.vwc <- dat.tidy.vwc %>%
  mutate(id=paste(dat.tidy.vwc$timepoint,dat.tidy.vwc$plot,dat.tidy.vwc$depth))

dat.tidy.temp <- dat.tidy %>%
  select(-vwc.20,-vwc.40,-vwc.60) %>%
  rename(`20`=temp.20,
         `40`=temp.40,
         `60`=temp.60) %>%
  pivot_longer(
    cols = c(`20`,`40`,`60`),
    names_to = "depth",
    values_to = "temp") 
dat.tidy.temp <- dat.tidy.temp %>%
  mutate(id=paste(dat.tidy.temp$timepoint,dat.tidy.temp$plot,dat.tidy.temp$depth))

big.dat <- left_join(dat.tidy.vwc,dat.tidy.temp, by="id")

big.dat.tidy <- big.dat %>%
  select(-timepoint.x,-time.x,-plot.x,-depth.x) %>%
  rename(timepoint=timepoint.y,
         time=time.y,
         plot=plot.y,
         depth=depth.y) 
big.dat.tidy <- big.dat.tidy %>%
  mutate(date=parse_date(str_sub(big.dat.tidy$timepoint,1,10)))


# visualize ---------------------------------------------------------------

dat <- big.dat.tidy %>%
  filter(date>"2021-11-07") #remove outlier timepoints during testing

dat %>%
  ggplot(aes(x=timepoint,
             group=plot,
             color=plot)) +
  geom_point(aes(y=vwc))


dat %>%
  ggplot(aes(x=timepoint,
             group=depth,
             color=depth)) +
  geom_line(aes(y=vwc)) +
  facet_wrap(~plot)

dat %>%
  ggplot(aes(x=timepoint,
             group=depth,
             color=depth)) +
  geom_line(aes(y=temp)) +
  facet_wrap(~plot)


