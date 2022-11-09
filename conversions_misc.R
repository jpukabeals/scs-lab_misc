0.2/.06
1*.2
3*.4


library(measurements)

conv_unit(0.5,pints,acre)
conv_unit(0.5,"pt","floz")
?conv_unit


# rate recommendation
# 0.5 pints per acre
conv_unit(0.5,"us_pint","us_oz")
# 8 fl oz per acre

# how many lbs ae per acre is that
# 8 fl oz per acre
# 3.8 lbs ae per gallon


conv_unit(8,"us_oz","us_gal")*3.8
# 0.2375 lbs of 24D acid 

# half pint rate is about 0.25 lb ae per acre
# 1 pint is about 0.5 lb ae per acre


# Prowl = 3 pints A
# 24D = 1 pint acre
# 0.2 acres kernza mono

# Prowl rate
conv_unit(3*.2,"us_pint","us_oz")
# need 10 fl oz of prowl for kernza monos

# 24D rate
conv_unit(1*.2,"us_pint","us_oz")
# 4 fl oz of 24D amine

# 16.6 GPA
16.6*.2

# need 3.5 gallons of water



# barley planting legacynet -----------------------------------------------

# we want to put down 120 lbs per acre of barley
# planting area is 0.5 acres
# we want to put down 60 lbs of barley


# Jakes math is 46 grams per 10 turns, double checking calcs

# 120 lbs acre
# 10 rotations is 74 feet
74*5
# 370 sq ft
library(measurements)

conv_unit(370,"ft2", "acre")


conv_unit(370,"ft2", "acre")*120/8

# .0637 lbs of seed per opener per 10 revolutions

conv_unit(0.127,"lbs","g")

# 57.6 grams per opener per 10 revolutions

# checking math....

# 57.6 grams over 74 ft
# each gram is 25 seeds
57.6*25
# 1440 seeds over 74 feet
74*12
# 888 inches
888/1440
# that's a seed every .62 inches


# you want 1 seed every 0.67 inches
# PERFECT

# confirm the rate is 58 grams per opener


# https://www.sprowtlabs.com/blog/barley-growing-guide

# 25 seeds/gram



# Tofu POST spraying ------------------------------------------------------

library(measurements)
# 5 seconds for 13 feet
# convert to miles per hour

conv_unit(1,"mile","ft")
13/5280
# ^ fraction of a mile I traveled

5280/13


406.15*5
# it would've taken 2031 seconds to travel a mile

conv_unit(2031,"sec","hr")
# It would've taken 0.56 hours to travel a mile

1/.56

# I would travel 1.8 miles in one hour
# travel time is 2 mile per hour

# https://www.rogerssprayers.com/teejet-tip-8002vs-yellow
# 30 psi
# 0.17 gallons per minute
conv_unit(0.17,"us_gal","us_oz")
# 22 fl oz per minute per nozzle

conv_unit(80,"inch","ft")


6.6*10*5
# 1000 sq ft amount
conv_unit(1000,"ft2","acre")

# .02 acres
# facetL
# 32 fl oz per acre is rate
32*conv_unit(1000,"ft2","acre")

# we want 1 fl oz of facet L for experiment
# put 2 fl oz in mixture, apply all plots using half



# axial
# 16 fl oz per acre
# we want 0.5 fl oz of axial for experiment
# put 1 fl oz of axial in a 2 L bottle

# wolverine
# 1.7 pint per acre
conv_unit(1.7,"us_pint","us_oz")
# 27 fl oz per acre
# we want 0.75 fl oz of wolverine
# put 1.5 fl oz of wolverine in a 2L bottle

# dicamba 4lb formulation
# 1 pint per acre
conv_unit(1,"us_pint","us_oz")
# 16 fl oz per acre
# put 1 fl oz of dicamba in a 2L bottle

# bison
# 2 pints per acre
# put 2 fl oz of bison in a 2L bottle




# 5.8
# 100/5.8


58*5
library(measurements)
conv_unit(58*5,"ft2","acre")


# 100 lbs acre
100*conv_unit(58*5,"ft2","acre")

conv_unit(0.6657484,"lbs","g")



# grams per foot 
# want lbs per acre

conv_unit(50,"g","lbs")

