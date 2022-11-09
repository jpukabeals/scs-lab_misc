library(measurements)

30*15*20
conv_unit(30*15*20,"ft2","acre")


128*8*.03

40*20*3*4*2
conv_unit(19200,"ft2","acre")

conv_unit(19200/3*2,"ft2","acre")


(50*100)+(40*30)
conv_unit(6200,"ft2","acre")


50-7.5
42.5/15



# K8 germ
c("8/10",
  "8/16",
  "8/19",
  "8/23",
  "8/24")
# 29 no germ
# 
data.frame(timepoint=1:3,
           germ=c(24,103,16),
           nogerm = c(172-24,172-103-24,172-16-24-103)) -> k8_germ.test

library(tidyverse)

k8_germ.test %>% 
  print()

k8_germ.test %>% 
  mutate(sum=cumsum(germ)) %>% 
  mutate(perc = sum/172*100)






