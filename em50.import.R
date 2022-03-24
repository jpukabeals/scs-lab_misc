
# downloaded data from em50 datalogger should be saved using the default
# name suggested by the program in the folder on the blue tablet specified
# on line 9 , i.e. "C:/Users/pukab001/Documents/fer.em50.data/"
# this script will take care of the rest --Jesse

theme_set(theme_bw())
options(digits=2)

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

# dat %>%
#   ggplot(aes(x=timepoint,
#              group=plot,
#              color=plot)) +
#   geom_point(aes(y=vwc))


dat %>%
  ggplot(aes(x=timepoint,
             group=depth,
             color=depth)) +
  geom_line(aes(y=vwc)) +
  facet_wrap(~plot)+
  labs(y="Volumetric water content",
       x="")

ggsave("vwc.cap.16dec2021.png",dpi=300)

dat %>%
  ggplot(aes(x=timepoint,
             group=depth,
             color=depth)) +
  geom_line(aes(y=temp)) +
  facet_wrap(~plot)+
  labs(y="Temperature (C)",
       x="")

ggsave("temp.cap.16dec2021.png",dpi=300)

dat %>%
  dplyr::select(timepoint) %>%
  arrange(timepoint) %>%
  head()


# nasa power data ---------------------------------------------------------

weather <- read.csv("POWER_Point_Daily_20211108_20211215_044d7004N_093d0824W_LST (1).csv",
                    skip = 14)

weather$date <- paste(weather$YEAR,weather$MO, weather$DY,sep = "/")
weather$date <- as.POSIXct(weather$date)

weather<- weather %>%
  rename(precip.mm=PRECTOTCORR,
         temp.C=TS)

weather %>%
  filter(precip.mm>=0) %>%
  ggplot(aes(x=date)) +
  geom_point(aes(y=precip.mm),
             color="blue") +
  geom_line(aes(y=temp.C),
             color="red") 

weather %>%
  arrange(desc(precip.mm)) %>%
  dplyr::select(date,precip.mm) %>%
  head()


# annotating with weather events ------------------------------------------

dat %>%
  ggplot(aes(x=timepoint,
             group=depth,
             color=depth)) +
  geom_line(aes(y=vwc)) +
  facet_wrap(~plot)+
  labs(y="Volumetric water content",
       x="") +
  geom_vline(xintercept = as.POSIXct("2021-11-10 4:00:00 UTC"),
             linetype=3) +
  annotate(geom = "text",
           x=as.POSIXct("2021-11-10 14:00:00 UTC"),
           y=.12,
           label="rain",
           size=3)
ggsave("rain.cap.png",dpi=300)


# Outliers ----------------------------------------------------------------

dat %>%
  ggplot(aes(x=timepoint,
             group=plot,
             color=plot)) +
  geom_line(aes(y=vwc)) +
  facet_wrap(~depth)+
  labs(y="Volumetric water content",
       x="",
       subtitle = "FER2306 appears as outlier at 60cm depth")

ggsave("fer2306.alldepths.png",dpi=300)
dat %>%
  filter(depth=="60") %>%
  ggplot(aes(x=timepoint,
             group=plot,
             color=plot)) +
  geom_line(aes(y=vwc)) +
  facet_wrap(~depth)+
  labs(y="Volumetric water content",
       x="",
       subtitle = "FER2306 appears as outlier at 60cm depth")
ggsave("fer2306.60cm.png",dpi=300)

# can we determine if outlier

dat %>%
  filter(depth=="60") %>%
  ggplot(aes(y=vwc)) +
  geom_boxplot()

# library(rstatix)

dat %>%
  filter(depth=="60") %>%
  mutate(outlier=is_outlier(.$vwc),
         ext.outlier=is_extreme(.$vwc)) %>%
  arrange(desc(outlier)) %>%
  head()

# not an extreme outlier