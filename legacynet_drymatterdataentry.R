# creating data entry sheet for legacynet dry matter composition

# column 1 should be plot number, repeating
# column 2 should be the specie
# column 3 should be dry matter weight


getwd()
# moving excel doc to working directory

library(tidyverse)

readxl::read_xlsx("lnet.test.xlsx") -> dat

dat %>% 
  pivot_longer(.,
               cols = c("s1",
                        "s2",
                        "s3",
                        "s4",
                        "s5",
                        "s6")) %>% 
  drop_na(value) %>% 
  rename(species=value) %>% 
  # View() # looks good!
  mutate(dry_matter_paper_bag=".") %>% 
  write.csv(row.names = F,
            "legacynet_cut4_composition-weight.csv")
               


