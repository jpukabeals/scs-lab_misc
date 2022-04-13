data("npk")

library(tidyverse)

library(lme4)
# install.packages("gvlma")
library(gvlma)
npk %>%
  # str()
  lm(yield~N*P*K,
       .) %>%
  # gvlma()
  # anova()
  emmeans::emmeans(~N) %>%
  multcomp::cld(Letters=letters,
                reverse=T) %>%
  mutate(group=str_trim(.group))

global_model <- function(x,y){
  x %>%
    lm(.$y~N*P*K,
       .) %>%
    anova() 
  
  aov <- x %>%
    lm(.$y~N*P*K,
       .) %>%
    anova() ;aov
  

  lets <- x %>%
    lm(.$y~N*P*K,
       .) %>%
    # anova()
    emmeans::emmeans(~N) %>%
    multcomp::cld(Letters=letters,
                  reverse=T) ; lets
  
}

global_model(npk,yield)
