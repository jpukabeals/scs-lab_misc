library(tidyverse)


# make plot plan
range = rep(1:7,6)
entry = rep(1:6,7)

tibble(range,entry) %>% 
  mutate(plot = paste0(range,entry)) %>% 
  arrange(plot) -> plots

# find image files

# C:\Users\pukab001\Downloads\iCloud Photos (16)\iCloud Photos

location <- "C:\\Users\\pukab001\\Downloads\\iCloud Photos (16)\\iCloud Photos"
list.files(location)

list.files(location)[1]

# setwd(location)

for (i in 1:length(list.files(location))) {
  file.rename(
    list.files(location)[i],
    paste0(plots$plot[i],".JPG"))
}
