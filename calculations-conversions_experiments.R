# How do we convert between units?

# One option is simply googling conversions. Use the conversions in a google
# sheet equation or in R for logging the changes.

# Another option is using measurements::conv_unit(). Very simple and robust way
# within R that leaves a log of conversions

# Another option is using units package. units::set_unit. This is really nice
# within a dataframe that will be used for analysis. It will attach a unit class
# to the variables


# calculating fertilizer rates for experiment by googlings conversion -----------------------------

# plot is 500 ft by 100 ft
500*100

# sq ft to ha
# 1sqft / 107639 = 1 ha
500*100/107639
# 0.46 ha 
# plot is 0.46 ha

# we want 60 kg ha
# 1 kg = 2.205 lb
60*2.205
# 132.3 lbs of N per ha

132.3*0.46
# 60.86 lbs of N per plot

# urea is 46% N
# 1 lbs of urea = 0.46 lbs of N

60.86/.46
# 132 lbs of urea per plot

# urea is in 40 lb bags
132/40
# we want 3.3 bags per plot


# calculating fertilizer rates for experiment using measurements::conv_unit -------------------------------------------------------
library(measurements)


# we want 60 kg N per hectare applied to plots that are 500'x100' with urea
# plot dims = 500*100ft
# get to hectare
conv_unit(500*100,"ft2","hectare")

# 60 kg N /0.46 = kg of urea
60/.46
# 130.435 kg of urea per hectare
conv_unit(60/.46,"kg","lbs")
# 287 lbs of urea per hectare
287*conv_unit(500*100,"ft2","hectare")
# 133 lbs urea per plot

# note that measurements does not change the class or name of the vector

# units package with npk dataset to convert between rates -----------------------------------------------------------

# install.packages("units")
library(units)

data(npk)
npk %>% 
  mutate(yield = set_units(yield,"kg/ha"), #the original value
         yield.Mg = set_units(yield,"Mg/ha",
                              force_single_symbol = T
                              )) -> npk.data

# let's say yield data was grams per 0.5 meter square

npk %>% 
  mutate(yield=yield*2, #grams per square meter
         yield = set_units(yield,g/m2), #tell it the units
         yield.kgha = set_units(yield,kg/ha), #now it recognizes units and can convert to anything
         yield.tona = set_units(yield,tons/acre)
         )
