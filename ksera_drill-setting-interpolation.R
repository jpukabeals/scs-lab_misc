
# Just copied from a google sheet. When I ctrl+C, that information is on the clipboard
dat <- read.delim("clipboard")

dat %>% 
  glimpse()

dat %>% 
  rename(setting=1,
         output=2) -> dat1

dat1 %>% 
  ggplot(aes(setting,output)) +
  geom_point()
# super linear

lm(output~setting,dat1)

# output = -15.54setting +70

# 35 = -15.54x + 70
(35-70)/-15.54


(25-70)/-15.54
(62-70)/-15.54

dat1 %>% 
  ggplot(aes(setting,output)) +
  geom_point() +
  geom_text(aes(label="y = -15.54x + 70",
                x=3,
                y=50)) +
  theme_bw() +
  labs(x="Flute distance from right of seed cup (cm)",
       y="Bulk seeding rate (lbs/A)",
       title = "Truax 8ft wide 8 inch row spacing calibration",
       subtitle = "Performed by Jesse 31Aug2022",
       caption = "*seeding rates below 12 lbs A may have \nunequal seed output among seed tubes") 

ggsave("truax-8ft_calibration_iwg.png",
       width = 4.5,
       height = 4,
       dpi=300)

(12-70)/-15.54
