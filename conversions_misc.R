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






conv_unit()