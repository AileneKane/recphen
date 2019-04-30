#Make fake data for testing stan model for WDFW rec fishing data
## Started 8 April 2019 ##
## By Ailene ##

## This file builds fake data for testing Stan models for wa rec data ##

##set.seed(73)

## CODE IS NOT DONE!
##################################################################
## Start with a simple linear model with no random effects and only doy as a predictor:
## fishabund ~ doy|fa + 1|year
## Plan is to fit separate models for each species, include year as a random effect and fishing area as a random effect

##################################################################


nfa = 10 # number of fishing areas
nyr = 20 # number of years

ntot = 50 # numbers of obs per year per fishing area  

#  with species  (note to self: This is not the best, better to draw from a distribution)
baseinter <- 60 # baseline intercept (days to BB) across all species
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

# now start building ...
testdat <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) no interactions, linear effects of force + photo + chill only
# (d) each obs is a different set of treatments

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nsp){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  force = rnorm(ntot, 0, 2)
  photo = rnorm(ntot, 0, 2)
  chill = rnorm(ntot, 0, 5)
  
  # set up effect sizes
  chillcoef = -3 # steep slope for earlier day with higher chilling
  forcecoef = -2 # less steep for forcing
  photocoef = -1
  
  # SD for each treatment
  chillcoef.sd = 1
  forcecoef.sd = 0.5 
  photocoef.sd = 0.1
  
  # build model matrix 
  mm <- model.matrix(~chill+force+photo, data.frame(chill, force, photo))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(spint[i], 
             rnorm(1, chillcoef, chillcoef.sd),
             rnorm(1, forcecoef, forcecoef.sd),
             rnorm(1, photocoef, photocoef.sd)
  )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx <- data.frame(bb, sp = i, 
                         chill, force, photo)
  
  testdat <- rbind(testdat, testdatx)  
}

