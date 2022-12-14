library(tidyverse)




# make plot plan ----------------------------------------------------------

# range = rep(1:7,6)
# entry = rep(1:6,7)

# tibble(range,entry) %>% 
#   mutate(plot = paste0(range,entry)) %>% 
#   arrange(plot) -> plots


# pull in plot plan -------------------------------------------------------

list.files()[1]
filename <- "2022 winterkill + yield notes - Sheet1.csv"

read.csv(filename) -> dat

library(tidyverse)

dat %>% 
  # glimpse()
  # filter(harvested18Aug=="y") %>% 
  filter(harvested27Jul=="n") %>% 
  mutate(variety.code = str_pad(variety.code,
                                2,
                                "left",
                                pad = 0)) %>% 
  arrange(plot_id) -> dat2


# find image file ---------------------------------------------------------


# "C:\Users\pukab001\Downloads\pewhe_photos_18aug\iCloud Photos"

location <- "C:\\Users\\pukab001\\Downloads\\pewhe_photos_18aug\\iCloud Photos"
list.files(location) %>% 
  length()



# for loop ----------------------------------------------------------------

setwd(location)


# for (i in 1:length(list.files(location))) {
#   file.rename(
#     list.files(location)[i],
#     paste0(plots$plot[i],".JPG"))
# }

for (i in 1:length(list.files(location))) {
  file.rename(
    list.files(location)[i],
    paste0(
      dat2$variety.code[i],
      "_plot", 
      dat2$plot_id[i],
           ".JPG"))
}
