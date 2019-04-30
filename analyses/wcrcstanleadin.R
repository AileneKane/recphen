#housekeeping

#rm(list=ls()) 
#options(stringsAsFactors = FALSE)


# Set working directory: 
#setwd("~/Documents/GitHub/recphen")

# Load libraries

source("analyses/source/savestan.R") # Dan Flynn code
source("analyses/source/stan_utility.R") # From Mike Betancourt

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## First steps to cleaning: Get the data, subset down to exact data columns that we want 
## Be sure to keep an eye on this part of the code and the files it sources, they will need updating!

## (1) slim down the data to correct response and no NAs ...

source("analyses/wcrcdataplease.R")
dim(fishsum)
dim(fishsum.yr)
##################################
## Prep the data for Stan model ##
##################################


# PRE-STAN STUFF
# knot.loc is a vector of the locations of knots, 
# pred.loc is a vector of prediction locations

# # location of knots
# nk<-round((max(dat$day)-min(dat$day)+1)/4)
# nknots<-ifelse(nk<35,nk,35)
# knots<-quantile(unique(covariate),seq(0,1,length=(nknots+2))[-c(1,(nknots+2))])
# 
# knot.loc<-
# pred.loc<- 
# d_knot_knot <- as.matrix(dist(knot.loc,upper=T)) 
# d_pred_knot <- as.matrix(dist(c(pred.loc,knot.loc),upper=T))
# d_pred_knot <- d_pred_knot[(N_pred_loc+1):nrow(d_pred_knot),1:N_pred_loc]
# 
# d_knot_knot2 <- d_knot_knot^2 # square matrix of the distance among knots.
# d_pred_knot2 <- d_pred_knot^2 #  matrix of the distance for the knot locations (rows) to the predicted locations (columns)
# 
# 


# making a list out of the processed data. It will be input for the model ...

#  source("source/bb_zscorepreds.R")
    datalist.chin <- with(fishsum, 
                    list(y = log(chin), 
                         w = week, 
                         effort = anglers,
                         N = nrow(fishsum)
                         )
                    )
    datalist.coho <- with(fishsum, 
                      list(y = log(coho), 
                           week = week, 
                           effort = anglers,
                           N = nrow(fishsum)
                          )
                    )
    
    datalist.chinyr <- with(fishsum.yr, 
                          list(y = log(chin), 
                               w = week, 
                               effort = anglers,
                               N = nrow(fishsum.yr)
                          )
    )
    datalist.cohoyr <- with(fishsum.yr, 
                          list(y = log(coho), 
                               week = week, 
                               effort = anglers,
                               N = nrow(fishsum.yr)
                          )
    )

#Now you're ready to fit the model in stan!


    
## AT THE END ...
str(datalist.chin)
str(datalist.coho)

#print("Unique forcing types are ...")


#First try fitting the model as a gam, across all areas
# for(y in 1987:1993) {
#   if(y==1988){next}
#   yr<-fishsum.yr[fishsum.yr$year==y,]
#   #fa<-unique(yr$area)
#   #for (f in 1:length(unique(fa)))
#   #fayr<-yr[yr$area==fa[f],]
#   w=as.integer(yr$week)
#   c = yr$chin
#   effort = yr$anglers
#   plot(w,c, pch=21,bg="gray",xlab = "Week",
#        ylab = "Expected recreational catch", main = paste("Year: ",y))
#   g = gam(log(c+1) ~ s(w) + offset(log(effort)))
#   lines(w,exp(g$fitted.values),lwd=3)
#   gam.check(g)
#   plot.gam(g)
# }
# 
# #Now, try to fit in stan
# dim(fishsum)
# head(fishsum)
# table(fishsum$area[fishsum$chin>0],fishsum$year[fishsum$chin>0])
# log(c+1)
