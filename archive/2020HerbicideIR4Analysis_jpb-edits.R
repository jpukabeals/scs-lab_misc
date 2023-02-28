library(Rmisc)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(stringr)
library(nlme)
library(ggplot2)
library(lattice)
library(multcompView)
library(emmeans)

# idat<-read.xlsx("IR4-All-2020-data.xlsx", sheet=8, startRow=15, na.strings="-", cols=1:12)

# Downloading the excel file from drive into my Downloads folder
# https://drive.google.com/drive/folders/1s-IcJDcgUsDythhYMhPrwUSvza2y6zrP
# accomplishing same task of line 14 using tidyverse

library(tidyverse)
library(multcomp)
idat <- readxl::read_xlsx("IR4-All-2020-data (1).xlsx",
                  sheet = 8,
                  skip = 12,
                  na = "-") %>% 
  dplyr::select(1:12) 



idat<-idat%>%
  mutate_if(is.character,as.factor)
str(idat)
idat$fherb<-factor(idat$herbicide, levels(idat$herbicide)[c(10,3,4,6,8,7,9,1,2,5,11)])
idat2<-subset(idat, fherb!="clethodim"&fherb!="sethoxydim"&timing!="3"&timing!="4")


# iwg_injury
mod1<-lm(iwg_injury~fherb*sample_season, na.action=na.omit, 
          data=subset(idat2, stand=="New"&
                        state=="WI"&
                        spray_date=="Spring 2020"))
cld(emmeans(mod1, ~fherb|sample_season), sort=F, Letters=letters)
# write.xlsx(cld(emmeans(mod1, ~fherb|sample_season), sort=F, Letters=letters), 
           # "/Users/junge037/Google Drive/Intermediate_wheatgrass_research/FGI_Experiments/HerbicideScreening/AnalysisOutputs/T7.xlsx")

write.csv(cld(emmeans(mod1, ~fherb|sample_season), sort=F, Letters=letters),
          "test.csv")

#Old stands
mod2<-lm(iwg_injury~fherb*sample_season, na.action=na.omit, 
         data=subset(idat2, stand=="Old"&
                       state=="MN"&
                       spray_date=="Fall 2019"))
cld(emmeans(mod2, ~fherb|sample_season), sort=F, Letters=letters)
write.xlsx(cld(emmeans(mod2, ~fherb|sample_season), sort=F, Letters=letters), 
           "/Users/junge037/Google Drive/Intermediate_wheatgrass_research/FGI_Experiments/HerbicideScreening/AnalysisOutputs/T9.xlsx")

#Weed injury
mod3<-lm(weed_injury~fherb*sample_season, na.action=na.omit, 
#mod3<-lm(weed_injury~fherb, na.action=na.omit, 
         data=subset(idat2, stand=="Old"&
                       state=="WI"&
                       spray_date=="Spring 2020"&
                       fherb!="Nontreated check (NTC)"))
cld(emmeans(mod3, ~fherb|sample_season), sort=F, Letters=letters)
#cld(emmeans(mod3, ~fherb), sort=F, Letters=letters)
write.xlsx(cld(emmeans(mod3, ~fherb|sample_season), sort=F, Letters=letters), 
           "/Users/junge037/Google Drive/Intermediate_wheatgrass_research/FGI_Experiments/HerbicideScreening/AnalysisOutputs/W6.xlsx")


#yield
ydat<-read.xlsx("IR4-All-2020-data (1).xlsx", sheet=9, startRow=11, na.strings="NA", cols=1:10)
str(ydat)
ydat<-ydat%>%
  mutate_if(is.character,as.factor)
ydat$fherb<-factor(ydat$herbicide, levels(ydat$herbicide))#[c(10,3,4,6,8,7,9,1,2,5,11)])
ydat2<-subset(ydat, fherb!="clethodim"&fherb!="sethoxydim")

library(clipr)
library(Rmisc)
write_clip(summarySE(subset(ydat2, stand=="New"&state=="NY"), "yield", c("state","spray_date", "fherb"), na.rm=T))
mod3<-lm(yield~fherb, na.action=na.omit, 
         data=subset(ydat2, stand=="Old"&
                       state=="WI"))
cld(emmeans(mod3, ~fherb), sort=F, Letters=letters)
write.xlsx(cld(emmeans(mod3, ~fherb), sort=F, Letters=letters), 
           "/Users/junge037/Google Drive/Intermediate_wheatgrass_research/FGI_Experiments/HerbicideScreening/AnalysisOutputs/Y4.xlsx")

anova(mod1)

dummy <- subset(idat, stand=="New"&spray_date=="Spring 2020") %>% 
  dplyr::select(iwg_injury,state,fherb,sample_season) %>% 
  # distinct(iwg_injury) %>% 
  drop_na(iwg_injury)


idatnsum<-summarySE(data = dummy,
                    measurevar = "iwg_injury", 
                    groupvars = c("state", "fherb", "sample_season"),
                    na.rm=T)
write.csv(idatnsum, "/Users/junge037/Google Drive/Intermediate_wheatgrass_research/FGI_Experiments/HerbicideScreening/AnalysisOutputs/T2.csv")
write.xlsx(idatnsum, "/Users/junge037/Google Drive/Intermediate_wheatgrass_research/FGI_Experiments/HerbicideScreening/AnalysisOutputs/T1.xlsx")

idatnsum %>% 
  glimpse()

ggplot(idatnsum, aes(x=fherb, y=iwg_injury))+
  facet_grid(state~sample_season)+
  geom_bar(stat="identity", position=position_dodge(0.9))+
  geom_errorbar(aes(ymax=iwg_injury+se, ymin=iwg_injury-se), width=0.2)
ggsave("Ave.png",width=11, height=9, units="in")
