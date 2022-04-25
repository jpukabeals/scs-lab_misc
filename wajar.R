
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



# generate dummy data -----------------------------------------------------

# in 2022-2023-2024 we will impose cropping systems where we will monitor
# performance, but it is not of primary interest. The yields of corn/soy/alfalfa
# will mostly function to let us know the approximate productivity of the land
# and whether productivity seems to vary within the experiment


# in 2025-2026 we will harvest kernza grain in August
# in 2026 we may also harvest fall regrowth

# here we generate expected responses


# create dataframe from experimental design
legacy_cropping_system <- c("corn-soy-oat", "alfalfa-alfalfa-alfalfa")
crop <- c("kernza-monoculture", "kernza-alfalfa-biculture")

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
  dplyr::select(-c(rep,plots,splots,block)) -> df

# arrange by treatments
df %>% 
  arrange(legacy_cropping_system,crop)

# create yield vector based on dataframe
# each unique treatment combination has 4 replicates
set.seed(314) #consistent outputs for rnorm
c(
  #kernza-alfalfa-biculture on alfalfa ground
  rnorm(4,480,20),
  # kernza-mono on alfalfa ground
  rnorm(4,520,20),
  # kernza-alfalfa-biculture on bau ground
  rnorm(4,450,20),
  # kernza-mono on bau ground
  rnorm(4,510,20)
) -> yield_2024


c(
  #kernza-alfalfa-biculture on alfalfa ground
  rnorm(4,480*.8,20),
  # kernza-mono on alfalfa ground
  rnorm(4,520*.7,20),
  # kernza-alfalfa-biculture on bau ground
  rnorm(4,450*.8,20),
  # kernza-mono on bau ground
  rnorm(4,510*.6,20)
) -> yield_2025


df %>% 
  arrange(legacy_cropping_system, crop) %>% 
  mutate(yield_2024 = yield_2024,
         yield_2025 = yield_2025) -> df_wide


df1 %>% 
  pivot_longer(
    cols = c(yield_2024,yield_2025),
    names_to = "yield_year",
    values_to = "yield"
  ) %>% 
  mutate(year = fct_recode(yield_year,
                           "2024" = "yield_2024",
                           "2025" = "yield_2025")) -> df_long

df_long %>% 
  ggplot(aes(crop,yield,
             col=year,
             fill=year)) +
  stat_summary(geom = "errorbar",
               position = position_dodge(.5),
               width = .3,
               col = 1) +
  stat_summary(geom = "bar",
               position = position_dodge(.5),
               width = .5,
               col=1) 


# aov() with df_wide separate by year -------------------------------------------------------------------

shapiro.test(df_wide$yield_2024)
bartlett.test(df_wide$yield_2024,
              g=df_wide$crop)

aov(yield_2024~crop,
    data = df_wide) %>% 
  # summary()
  LSD.test(.,
           trt = "crop") %>% 
  print()

shapiro.test(df_wide$yield_2025)
# reject Ho that yield_2024 is from normal distribution (even though it is!)
shapiro.test(
  log(df_wide$yield_2025)
)
# log transformation does not fix this
shapiro.test(
  sqrt(df_wide$yield_2025)
)
# square root transformation does not fix this
shapiro.test(
  1/(df_wide$yield_2025)
)
# reciprocal did not fix this
library(MASS)
boxcox(
  aov(yield_2025~crop,
      data = df_wide)
) -> bc
lambda <- bc$x[which.max(bc$y)] # got this code from online

shapiro.test(
  ((df_wide$yield_2025^lambda-1)/lambda)
)
# box-cox transformation succeeded in making response normally distributed

bartlett.test(df_wide$yield_2025,
              g=df_wide$crop)
# reject Ho that variance around yield_2025 is homogeneous

# use oneway.test due to unequal variance (found this test online)
oneway.test(
  ((df_wide$yield_2025^lambda-1)/lambda)~df_wide$crop
)
# we reject Ho, yield in 2025 differs between intercrop and mono

# let's do same analysis with aov()
# no transformation of response variable or adjustment for unequal variance
aov(yield_2025~crop,
    data = df_wide) %>% 
  summary()
# same conclustion, reject Ho

aov(yield_2025~crop,
    data = df_wide) %>%
  LSD.test(.,
           trt = "crop") %>% 
  print()

# aov() with df_long, year as factor --------------------------------------

df_long %>% 
  dplyr::select(yield) %>% 
  .$yield %>% 
  shapiro.test()
# cannot reject Ho that yield samples are from normal distribution

df_long %>% 
  bartlett.test(
    x=.$yield,
    g=c(.$year)
  )
# cannot reject Ho that variance was similar among years

df_long %>% 
  bartlett.test(
    x=.$yield,
    g=c(.$crop)
  )
# reject Ho variance was similar among crops

df_long %>% 
  group_by(crop) %>% 
  summarise(st.dev = sd(yield))
# variance was higher in kernza monocultures

# run an anova anyways
# full model
aov(
  formula = yield~crop*year*legacy_cropping_system,
  data = df_long
) %>% 
  summary()
# crop*year interaction
# main effect of legacy_cropping_system and year
# no effect of crop

# testing crop*year interaction
aov(
  formula = yield~crop*year*legacy_cropping_system,
  data = df_long
) %>% 
  LSD.test(
    trt = c("crop","year")
  ) %>% 
  print()
  
# testing legacy_cropping_system main effect
aov(
  formula = yield~crop*year*legacy_cropping_system,
  data = df_long
) %>% 
  LSD.test(
    trt = c("legacy_cropping_system")
  ) %>% 
  print()

# testing year main effect
aov(
  formula = yield~crop*year*legacy_cropping_system,
  data = df_long
) %>% 
  LSD.test(
    trt = c("year")
  ) %>% 
  print()


# figures -----------------------------------------------------------------

source("ggplot_custom_theme.R")
theme_set(theme_jpb())
# crop*year interaction
aov(
  formula = yield~crop*year*legacy_cropping_system,
  data = df_long
) %>% 
  LSD.test(
    trt = c("crop","year")
  ) %>% 
  # print()
  # str()
  .[["groups"]] %>% 
  mutate(crop = c("kernza-mono",
                  "kernza-alfalfa-intercrop",
                  "kernza-alfalfa-intercrop",
                  "kernza-mono"),
         year = c("2024",
                  "2024", 
                  "2025",
                  "2025")) %>% 
  ggplot(aes(crop,yield,
             label = groups,
             fill = year)) +
  geom_col(position = position_dodge(.7),
           width = .7,
           col=1) +
  geom_text(position = position_dodge(.7),
            vjust=-1,
            size=5) +
  scale_y_continuous(limits = c(0,600)) +
  labs(y="Yield (kg ha)",
       x="") +
  scale_fill_brewer(type = "qual")

# legacy_cropping_system main effect
aov(
  formula = yield~crop*year*legacy_cropping_system,
  data = df_long
) %>% 
  LSD.test(
    trt = c("legacy_cropping_system")
  ) %>% 
  # print()
  .[["groups"]] %>% 
  mutate(cropping_system = c("alfalfa",
                             "corn-soy")) %>% 
  ggplot(aes(cropping_system,yield,
             label=groups)) +
  geom_col(width = .7,
           col=1,
           size=.8) +
  geom_text(vjust=-1,
            size=6) +
  scale_y_continuous(limits = c(0,500)) +
  labs(y="Kernza yield (kg ha)",
       x="Cropping system prior to establishment")
