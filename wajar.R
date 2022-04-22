
# IWG performs best after a 3-yr alfalfa stand vs. intercropped

# specs -------------------------------------------------------------------

# wintersteiger and grain drill is 5'
# thor has 4-row planter which is 10' width

# let's make all plots 10' by 10'


# treatments --------------------------------------------------------------

# legacy cropping systems: corn-soy-oat, alfalfa-alfalfa-alfalfa
# cropping system: kernza-mono, kernza-intercropped

legacy_cropping_system <- c("corn-soy-oat", "alfalfa-alfalfa-alfalfa")
crop <- c("kernza-monoculture", "kernza-alfalfa-biculture")



# design ------------------------------------------------------------------
library(tidyverse)
library(agricolae)

print(design.split)

agricolae::design.split(
  trt1 = legacy_cropping_system,
  trt2 = crop,
  r = 4,
  design = "rcbd",
  serie = 0,
  seed = 314,
  kinds = "Super-Duper",
  first = T,
  randomization = T
) %>%
  .$book %>% 
  mutate(rep = rep(seq(1:4),4)) %>% 
  mutate(experimental_unit = paste(block,rep,sep = "0"),
         .before = plots) %>% 
  dplyr::select(-rep) %>% 
  mutate(crop_2022 = ifelse(legacy_cropping_system == "alfalfa-alfalfa-alfalfa",
                            "alfalfa",
                            "corn")) %>% 
  mutate(crop_2023 = ifelse(legacy_cropping_system == "alfalfa-alfalfa-alfalfa",
                            "alfalfa",
                            "soy")) %>% 
  mutate(crop_2024 = ifelse(legacy_cropping_system == "alfalfa-alfalfa-alfalfa" &
                              crop == "kernza-monoculture",
                            "alfalfa-kernza",
                            ifelse(legacy_cropping_system == "alfalfa-alfalfa-alfalfa" &
                                     crop == "kernza-alfalfa-biculture",
                            "alfalfa-kernza+alfalfa",
                            ifelse(legacy_cropping_system == "corn-soy-oat" &
                                     crop == "kernza-monoculture",
                            "oat-kernza",
                            "oat-kernza+alfalfa")))) %>% 
  mutate(crop_2025 = ifelse(crop=="kernza-monoculture",
                            "kernza",
                            "kernza+alfalfa"),
         crop_2026 = ifelse(crop=="kernza-monoculture",
                            "kernza",
                            "kernza+alfalfa")) %>% 
  write.csv(
    file="wajar_plot-assignments.csv",
    row.names = F
  )




