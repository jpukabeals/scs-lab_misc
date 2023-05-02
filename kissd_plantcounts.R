getwd()

## bringing in data from...
# https://docs.google.com/spreadsheets/d/1pJgJMa4qHDk_reHJgjUmOihmJb-7BWKUbMS7RBcOCO8/edit#gid=0

# read.delim(
#   "clipboard"
# ) -> dat
# 
# # plant counts recorded by jesse and katherine
# 
# dat %>% 
#   mutate(person = if_else(plot>412, "katherine", "jesse")) %>% 
#   dplyr::select(Date,Variety, plot, person, IWG_row_1,IWG_row_2) %>% 
#   mutate(plants = (IWG_row_1 + IWG_row_2)/2) -> dat2
# 
# dat2 %>% 
#   group_by(person) %>% 
#   summarise(
#     mean=mean(plants),
#     sd=sd(plants))
# 
# 
# 
# read.delim(
#   "clipboard"
# ) -> dat3
# 
# dat3 %>% 
#   mutate(person = "undergrad") %>% 
#   dplyr::select(Date,Variety, plot, person, IWG_row_1,IWG_row_2) %>% 
#   mutate(plants = (IWG_row_1 + IWG_row_2)/2) -> dat4
# 
# bind_rows(dat2,dat4) -> dat5
# 
# dat5 %>% 
#   group_by(Date) %>% 
#   summarise(
#     mean=mean(plants),
#     sd=sd(plants))
# 
# dat5 %>% 
#   mutate(plants_per_meter = plants*2) %>% 
#   write.csv(
#     "kissd_plantcounts.csv",
#     row.names = F
#   )

read.csv(
  "kissd_plantcounts.csv"
) -> dat6


# summary statistics ------------------------------------------------------

# Did plant counts differ by date?

dat6 %>%
  group_by(Date) %>%
  summarise(
    mean=mean(plants),
    sd=sd(plants))

# Plant counts were estimated to be greater in fall than in following spring. 
# Fall counts were taken by undergrad workers, Spring counts by Jesse and Katherine

# This may indicate there was winterkill or we underestimated when counting in
# spring. Katherine calls this the "bunch bias", because when plants are growing
# super close together we just count it as one plant because it is one bunch
# when in reality it could be two different plants


# Did plant counts differ between Jesse and Katherine?

dat6 %>%
  # distinct(Date)
  filter(Date == "5/2/2023") %>% 
  group_by(person) %>%
  summarise(
    mean=mean(plants),
    sd=sd(plants))
# Jesse and Katherine have very similar plant counts! This is good!


# Did plant counts differ by Variety?

dat6 %>% 
  group_by(Date,Variety) %>%
  summarise(
    mean=mean(plants),
    sd=sd(plants)) %>% 
  arrange(mean)
# no obvious trends, but October had more plants than May

dat6 %>% 
  mutate(plants = plants*2) %>% 
  ggplot(aes(Date,plants,
             group = Variety,
             color = Variety,
             shape = Variety)) +
  # geom_point() +
  stat_summary(
    geom = "point",
    size=3
  ) +
  stat_summary(geom = "line",
               size=1) +
  labs(
    y="IWG plants per meter of row",
    x=""
  ) +
  theme(
    legend.title = element_blank()
  )
ggsave(
  "kissd_plantcountfig.png",
  width = 5.5,
  height = 4.5,
  unit = "in"
)




