# want to calculate the cost-of-production
# what is the profit per acre?
# how do changes to yield and price change the projected profit?


# data is from Griggs farm in TN
# https://www.youtube.com/watch?v=vmt4aYelzBk

# reading in data
read.csv("cost-of-production.csv") -> dat

# dat represents the raw data he has from 2021 and predicted expenses for 2022

# 2022 yields are based on 5 year averages

# some 2021 expenses are outliers, like insecticides for 1 crop that had an
# usual pest outbreak

# note the "overhead" subcategory includes equipment, utilities, insurance,
# taxes and other necessary costs of doing business

library(tidyverse)

dat %>% 
  glimpse()

# note that dollars per acre are all positive values. If category is expense,
# they should be subtracted in order to calculate profit

# yield is in lbs per acre for cotton and bushels per acre for other crops

# land rent is lower in TN than in midwest


# calculate profit --------------------------------------------------------

# Option 1
# dplyr filter and join
dat %>% 
  filter(category=="income") %>% 
  mutate(dollars_per_acre = yield*price) -> calc.return

dat %>% 
  drop_na(dollars_per_acre) %>% 
  bind_rows(.,calc.return) %>% 
  glimpse() -> dat1

dat1 %>% 
  arrange(crop,year,desc(category),subcategory) -> dat1

# Option 2
# for loop
dat2 <- dat
for(i in 1:length(dat$dollars_per_acre)){
  ifelse(is.na(dat2[i,7]),
         dat2[i,7] <- dat2[i,5]*dat2[i,6],
         dat2[i,7] <- dat2[i,7]
         )
}

# Profit of each system
# start with one system
# sum the expenses, then sum the income



# Change expenses to negative values ---------------------------------------

# with filter and join
dat1 %>% 
  filter(category=="expense") %>% 
  mutate(dollars_per_acre=-1*dollars_per_acre) -> neg.values

dat1 %>% 
  filter(category=="income") %>% 
  bind_rows(.,neg.values) -> dat3
rm(neg.values)

# with for loop
dat4 <- dat1
for (i in 1:length(dat1$dollars_per_acre)) {
  ifelse(
    dat4[i,3]=="expense",
    dat4[i,7] <- dat4[i,7]*-1,
    dat4[i,7] <- dat4[i,7]
  )
}
dat4


dat4 %>% 
  # filter(crop=="corn" &
  #          year=="2021") %>% 
  group_by(crop,year) %>%
  summarise(net_profit = sum(dollars_per_acre)) ->dat_profit

dat4 %>% 
  filter(category=="expense") %>% 
  group_by(crop,year) %>% 
  summarise(cost_of_production = sum(dollars_per_acre)*-1) -> dat_expense

dat4 %>% 
  filter(category=="income") %>% 
  group_by(crop,year) %>% 
  summarise(gross_profit = sum(dollars_per_acre)) -> dat_gross

dat_profit %>% 
  bind_cols(.,dat_expense)%>% 
  bind_cols(.,dat_gross) %>% 
  rename(crop = crop...1,
         year = year...2) %>% 
  dplyr::select(crop,year,net_profit,cost_of_production,gross_profit) -> sum.tab.1
rm(dat_profit,dat_expense,dat_gross)


sum.tab.1

sum.tab.1 %>% 
  pivot_longer(cols = c(net_profit,cost_of_production,gross_profit)) %>% 
  ggplot(aes(name, value,
             # col=1,
             fill=crop)) +
  stat_summary(geom = "errorbar",
               position = position_dodge(.6),
               width=0.3,
               col=1) +
  stat_summary(geom = "bar",
               position = position_dodge(.6),
               width=0.6,
               col=1) +
  labs(x="",
       y="Dollars per acre",
       caption = "Data combined across 2 years (2021 and 2022)
       cotton and soybean were not profitable in 2021") -> gg.sum.tab.1
gg.sum.tab.1

# breaking down cost of production

dat2 %>% 
  dplyr::select(-c(yield,price)) %>% 
  filter(category=="expense") %>% 
  arrange(subcategory,crop,year) %>% 
  group_by(subcategory) %>% 
  summarise(cost_avg = mean(dollars_per_acre),
            cost_range = paste(min(dollars_per_acre),"--",max(dollars_per_acre))) -> sum.tab.2

sum.tab.2


dat2 %>% 
  dplyr::select(-c(yield,price)) %>% 
  filter(category=="expense") %>%
  mutate(subcategory=as.factor(subcategory),
         subcategory=fct_relevel(subcategory,"seed")) %>% 
  ggplot(aes(subcategory,dollars_per_acre,
             fill=crop,
             col=crop)) +
  stat_summary(position = position_dodge(.5)) +
  labs(x="",
       y="Dollars per acre")-> gg.sum.tab.2
gg.sum.tab.2


# sensitivity analysis ----------------------------------------------------

# what price is required in 2022 to break even?

# we need gross profit = cost of production


# c = y*p
# c = cost of production
# y = yield
# p = price

breakeven <- function(cost,yield){
  cost/yield
}



# what is cost of production?
# adding a row so I can bind with income table
sum.tab.1 %>% 
  filter(year=="2022") %>% 
  dplyr::select(crop,cost_of_production) %>% 
  add_row()-> sum.tab.cost

dat1 %>% 
  filter(category=="income" & 
           year == "2022") %>% 
  bind_cols(.,sum.tab.cost) %>% 
  rename(crop = crop...1) %>% 
  dplyr::select(-c(category, subcategory)) -> tab.breakeven

# given current prices, what yields are needed to breakeven for each crop?

tab.breakeven %>% 
  dplyr::select(-crop...8) %>% 
  mutate(breakeven_yield = cost_of_production/price,
         breakeven_price = cost_of_production/yield)

# now can we make a graph that shows price and yield combinations that produce
# cost of production

tibble(
  crop = rep("corn",length(seq(173*.5,173*2,5))),
  yield = seq(173*.5,173*2,5),
  price = tab.breakeven$cost_of_production[1]/yield
)  -> df.corn.sensitivity

tibble(
  crop = rep("soybean",length(seq(62*.5,62*2,5))),
  yield = seq(62*.5,62*2,5),
  price = tab.breakeven$cost_of_production[3]/yield
)  -> df.soybean.sensitivity

df.corn.sensitivity %>% 
  bind_rows(df.soybean.sensitivity) %>% 
  ggplot(aes(price,yield)) +
  # geom_point() +
  geom_path() +
  facet_wrap(~crop,
             scales = "free") +
  labs(x="Price ($ per bushel)",
       y="Yield (bushels per acre",
       title = "Row crop breakeven",
       caption = "Need $4 corn and 200 bushel yields to breakeven
       Need $12 soybeans + 50 bushel yields to breakeven")

breakeven(596,50)

tibble(
  crop = rep("cotton",length(seq(1070*.5,1070*2,5))),
  yield = seq(1070*.5,1070*2,5),
  price = tab.breakeven$cost_of_production[2]/yield
)  -> df.cotton.sensitivity

df.cotton.sensitivity %>% 
  ggplot(aes(price,yield)) +
  geom_path() +
  labs(x="Price ($ per lb)",
       y="Yield (lbs per acre)",
       title = "Cotton breakeven",
       caption = "Need $1 cotton and 750 lbs A to breakeven")

breakeven(tab.breakeven$cost_of_production[2],1)

