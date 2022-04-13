# demonstrating the classical analysis of a randomized complete block design
# experiment. This analysis approach should be useful for analysis of most
# experiments in the lab. This analysis approach will not be useful for
# unbalanced datasets with repeated measures



# practicing aov and lsd.test with agricolae

data(npk)
library(tidyverse)
source("ggplot_custom_theme.R")
theme_set(theme_jpb())

npk %>% 
  # glimpse()
  summary()


# describing pea yield ----------------------------------------------------------

# standardizing yield to pounds per acre
npk %>% 
  mutate(yield=yield*70) -> npk

# distribution of the samples of yield

npk %>% 
  ggplot(aes(yield)) +
  stat_bin(bins = 15)
# fairly normally distributed

npk %>% 
   ggplot(aes(yield)) +
   geom_density(bw=200) +
   stat_bin(aes(y=..density..),
            alpha=.3,
            bins = 15)
# fairly normally distributed

shapiro.test(npk$yield)
# cannot reject Ho that yield is normally distributed

# ?npk 

# Narrative: we sampled a population of peas for their yield 

# our samples of this population of peas were normally distributed

# Therefore, we felt confident that our sampling was robust enough to describe
# the entire pea population in the experiment, as we expect yield to be normally
# distributed

# Due to normal distribution, we can indicate centrality and variance using
# mean and standard deviation

npk %>% 
  summarise(population_mean= mean(yield),
            population_sd = sd(yield))
# our pea population that we sampled from was a 3841 lb acre yielding population
# we feel confident in making that inference of the population

# we expect that if we went through the work of measuring every single plant in
# the entire experiment, we would still have an average yield of 3841 lb acre

# we also expect variations from the 3841 lb acre yield. We can infer that 68%
# of the plants we sample would yield (when converted to a lb acre basis)
# between 1 standard deviation of the mean, meaning they'd be in the range of
# 3841-432 and 3841+432 (3409-4273 lb acre respectively). We'd also expect 95%
# of the plants to be between 2 standard deviations of the mean. These are the
# properties of the normal distribution.

# From our samples, we feel confident in our ability to describe the entire pea
# population in our experiment, which we can graphically present below
ggplot() +
  stat_function(
    fun = dnorm,
    args = list(mean=mean(npk$yield),
                sd=sd(npk$yield)),
    size=1.1
  ) +
  scale_x_continuous(limits = c(mean(npk$yield)-3*sd(npk$yield),
                                mean(npk$yield)+3*sd(npk$yield)))+
  annotate(geom = "rect",
           col=1,
           linetype=3,
           fill=NA,
           xmin=(mean(npk$yield)-1*sd(npk$yield)),
           xmax=(mean(npk$yield)+1*sd(npk$yield)),
           ymax = .001,
           ymin = 0,
           alpha=.5) +
  annotate(geom = "text",
           x=mean(npk$yield),
           y=.0011,
           label = "dotted lines = 1 standard devation or\n68% of expected population")+
  annotate(geom = "text",
           x=mean(npk$yield-1.1*sd(npk$yield)),
           y=.0008,
           label = "3409 lb acre",
           angle=90) +
  annotate(geom = "text",
           x=mean(npk$yield+1.1*sd(npk$yield)),
           y=.0008,
           label = "4273 lb acre",
           angle=-90) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  stat_bin(
    data=npk,
    aes(y=..density..*.5,
        x=yield),
    alpha=.2,
    bins=15,
    col=1
  ) +
  labs(y="Frequency",
       x="Pea yield (lb acre)",
       title = "Pea popluation of npk experiment")
#^ This is accomplishment in itself. Achieving good enough sampling to make an
#inference of a population. This can be worth reporting, though typically just
#in text vs. a ggplot

# testing by N rate -------------------------------------------------------

# do populations within the pea population that recieved different N rates
# differ in their yield?

# because we are able to confidently infer what the properties of the entire pea
# population, we can test if differences in sub-populations within the pea
# population can be explained by random chance. If differences cannot be
# explained by random chance, and each sub-population was treated identically
# except for an applied treatment then we can attribute the differences in the
# populations to the treatment

# looking at sub-populations of peas that either recieved N fertilizer or did
# NOT recieve N fertilizer

# population distribution by N rate
npk %>% 
  ggplot(aes(yield,
              col=N)) +
  geom_density(bw=250) +
  stat_bin(aes(y=..density..*.4,
               fill=N),
           alpha=.3,
           col=NA,
           position = position_dodge(),
           bins=10)
# both N populations seem normally distributed, populations seem different

npk %>%
  # filter(N=="0") %>% 
  # filter(N=="1") %>% 
  .$yield %>% 
  shapiro.test(.)
# cannot reject Ho for both N-rate populations that they come from a normal
# distribution

 
# Because our samples appear normally distributed and we cannot reject Ho that
# they are not normally distributed, we will assume these samples are coming
# from two populations that are both normally distributed


# summary statistics for reporting a normal distribution
npk %>% 
  group_by(N) %>% 
  summarise(yield.mean = mean(yield),
            yield.n = n(),
            yield.sd = sd(yield),
            yield.se = sd(yield)/sqrt(yield.n))
# why do we use standard error? we can describe each plot as a population. Our
# yield computed for that plot can be considered as a mean of the population of
# that plot. When comparing the standard deviations of means, we use the
# standard error of the mean. Since we have 12 plots for each N rate, we are
# saying that each plot is a population and that the plants that make up the
# sample are an average. When we get the yield of those plants, we are taking an
# average of a population, so our datapoint is already a mean. When we take the
# mean of our datapoints, we are taking a mean of a mean, and we describe that
# variance using standard error rather than standard deviation


##ANOVA time!! 

#Based on what we can infer about the entire pea population and of the
#sub-populations that received different N rates, we can ask the question if it
#is likely these populations are different in their yields. We expect that if we
#went back in time and redid the sampling of the populations we would get
#different numbers for yield, but would these still indicate these populations
#are different? This is where the properties of normal distributions again help
#us answer these questions

library(agricolae)
# Ho: yield does not differ between N rates

# Ho (rephrased): the yield we observed in the sub-populations came from the
# same population and can be explained by the inherent variance in this
# population

# alpha level is the threshold we use for what is considered allowable random
# chance. Alpha of 0.05 means that only 5% of the time could we observe the
# differences in these sub-populations by random chance when they came from the
# overall pea population


## t-test
# the appropriate test for this situation is a t-test
t.test(
  x=subset(npk,N=="1")$yield,
  y=subset(npk,N=="0")$yield
)
# reject Ho that these sub-populations are from the same population 

## ANOVA
# ANOVA with aov()
npk %>% 
  aov(yield~N,
      data = .) %>% 
  summary()
# reject Ho that yield does not differ between N rates
# note same p-value, it's same math


## LSD
# compare groups using fishers least significant difference
aov(yield~N,npk) %>% 
  LSD.test(y=.,
           trt="N") %>% 
  print()


# Experiment summary and findings -----------------------------------------------------------

aov(yield~N,npk) %>% 
  summary() -> yield.N.aov.summary


aov(yield~N,npk) %>% 
  LSD.test(y=.,
           trt="N") -> yield.N.LSD.summary


yield.N.LSD.summary$statistics
# The pea population mean yield was 3841 lb acre

# Nitrogen rates were imposed on the pea population
# a subset of the population received N and a subset did NOT receive N
# these samples of these populations were also normally distributed

# we felt confident our samples captured the properties of these populations and
# we can describe them statistically
yield.N.LSD.summary$means

# while these populations appear different, we do not know if these differences
# can be explained by random chance and the error inherent in our sampling
# methods

# we can test a null hypothesis that the populations are not different using an
# analysis of variance with an alpha level of 0.05, meaning that based on the
# mean and variances of the two populations, we would only observe samples that
# were this different from a population that was the same 5% or less of the time

aov(yield~N,npk) %>% 
  summary()
# since the p-value is below the threshold of 0.05, we reject the null
# hypothesis that the pea yields from the two populations that received
# different N rates are from the same population

# Now we determine which populations differ from eachother. This is a post-hoc
# test, only done IF and AFTER an anova determines we can reject the Ho
yield.N.LSD.summary$groups

# since there are only two populations, this is obvious, but sometimes there are
# multiple populations and some differ from eachother and others do not


# aov() also generates descriptive statistics about the anova itself
yield.N.LSD.summary$statistics

## Coefficient of variation
# The coefficient of variaiton (CV) is 10.2 - this is a relative measure of
# variability in the data. It takes experience with this type of experiment to
# know if the CV (proxy for variability) is high or low. Usually quantitative
# data (i.e. yields) has lower CV values and qualitative data (i.e. lodging
# scores)

# CV values can be used to identify and exclude certain levels of a treatment CV
# of different N populations...calculating below. equation from field design
# notes
npk %>% 
  group_by(N) %>%
  summarise(cv=sd(yield)/mean(npk$yield)*100)
# CV values are similar between 2 populations, this is good. 
# CV seems similar to standard deviation, it seems to be convention to report

## Mean-square error
#need to review how to interpret
##LSD 
# need to review notes
# Df
# review notes

yield.N.aov.summary

## Sum sq
# review notes

##F value
# review notes

