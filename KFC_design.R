
# Spring forage trial (SPF)

# Led by TLI, Spencer/Brandon

# Planted in fall 2023 in KS, MO
# MN didn't plant in 2023, but may plant spring 2024

# plot dimensions: 7.5 ft x 20 ft. 

library(agricolae)
trtIWG <- c("TLI C5", "Manifest", "TLI 3471", "Ohae", "TLI C3", "TLI 703", 
            "TLI 701", "MN Clearwater", "Rush", "TLI 704"," TLI 801" ,"TLI C4")

library(tidyverse)
trtIWG %>% 
  as_tibble() %>% 
  rowid_to_column() %>% 
  rename(entry = rowid,
         trtIWG = value) ->  entry_codes

designIWG <- design.rcbd(trtIWG, r=4, 2,seed=314,
                         first = T)
bookIWG <- designIWG$book

bookIWG %>%
  left_join(entry_codes)->bookIWG
  


write.csv(
  bookIWG,
  "SPF_book.csv",
  row.names = F
)
