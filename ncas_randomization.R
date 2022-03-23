# Creating NCAS plot plan randomization
# Jesse, 28Feb2022

# design specs

# 3 factor RCBD
# planting_year (3 levels: 2022,2023,2024)
# N_rate (3 levels: 0, 80, 160)
# blocks (4 levels: 1,2,3,4)

planting_year <- c("2022",
                   "2023",
                   "2024") %>%
  as.factor()

n_rate <- c("0",
            "80",
            "160") %>%
  as.factor()

block <- c("1",
           "2",
           "3",
           "4") %>%
  as.factor()

ncas_df <- data.frame(
  rep(planting_year,1,
      each=3),
  rep(n_rate,3)
) %>%
  # glimpse()
  # colnames()
  rename(planting_year="rep.planting_year..1..each...3.",
         n_rate = "rep.n_rate..3.") %>%
  mutate(trt_num=1:9,
         n_rate=str_pad(n_rate,3,"left","0"),
         trt_name=paste(planting_year,n_rate,sep = "_"));ncas_df


library(agricolae)
?agricolae::design.rcbd()
# this function uses set.seed 

design.rcbd(
  trt = ncas_df$trt_name,
  r=4,
  seed = 1052154225,
  kinds = "Super-Duper"
) %>%
  .$book %>%
  rename(trt_name="ncas_df$trt_name") %>%
  write.csv(row.names = F,
            "ncas_randomization.csv")
