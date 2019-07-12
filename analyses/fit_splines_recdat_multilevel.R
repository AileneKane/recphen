#simulate some salmon catch data and fit splits to it
#super useful markdown file on splines in stan!
#https://github.com/milkha/Splines_in_Stan/blob/master/splines_in_stan.Rmd
#https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html

#housekeeping
#rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set working directory: 
setwd("~/Documents/GitHub/recphen")

# Load libraries

library("splines")
library("rstan")
library(mgcv)

#prepare for stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#Read in WA rec data (just the early years for now)
d<-read.csv("analyses/output/wacrdat_1984_1993.csv", header=TRUE)#for now just use the old data

#prep the data for the model
source("analyses/wcrcstanleadin.R")

years<-unique(fishsum.yr$year)

quartz(height=8, width=20)

dat=fishsum
  yr<-as.integer(as.factor(dat$year))
  n_yr<-length(unique(yr))
    #for (a in 1:length(regions)){
  n_weeks<-length(unique(dat$week))
  w<-as.integer(unique(dat$week))
  X <- as.integer(dat$week) #weeks
  spline_degree <- 3
  #could add a smoothing prior (to help with overfitting): https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html 

  #Currently choosing knot location and fitting the B-spline before fitting the stan model
  #Setting knots every 6 weeks- why? I don't know!; this yields 11 knots plus the two boundary knots
  B <- t(bs(w, knots=seq(1,52,6), degree=spline_degree, intercept = TRUE)) # creating the B-splines
  N <- length(X); 
  num_basis <- nrow(B)
   
  Y <- log(dat$chin+.001)
  OFFSET<-log(dat$anglers)
  Y<-Y.offset<- log(dat$chin+.001)/log(dat$anglers+.001)
  reg<-as.integer(dat$region)
  smml<-stan_model("analyses/recmodml_AOS.stan")
  
  fitml<-sampling(smml,iter=1000,control = list(adapt_delta=0.99,max_treedepth=15))
  quartz()
  pairs(fitml)
  #plot(fitml)
  ffml<-extract(fitml)
  ffml.sum<-summary(fitml)$summary
  ffml.sum[grep("a0",rownames(ffml.sum)),]#0.123,0.016,0.019,-0.23,0.054,0.122
  ffml.sum[grep("mu",rownames(ffml.sum)),]#0.0179
  #much lower than a0 in the non-multilevel model- is this ok?
  #ff.sum[grep("a0",rownames(ff.sum)),1]#0.6458231
  ffml.sum[grep("sigma",rownames(ffml.sum)),]#sigma:758; sigma_a_yr: 1.514
  #why is sigma for y so much bigger than in the nonml models? I don't think it should be this much bigger even though it includes all years
  #ff.sum[grep("sigma",rownames(ff.sum)),1]#sigma:0.05880219
  
  range(ffml.sum[grep("a_raw",rownames(ffml.sum)),1])#-1.70304100  0.01866892
  #range(ff.sum[grep("a_raw",rownames(ff.sum)),1])#-1.236066  1.201761
  #Questions
  #Why are 8 identical Y_hat_logs?
  #compare the ranges of the ypreds in multilevel and nonmultilevel model:
  
 range(ffml$Y_hat_log)#-596.0600  283.7359
 range(ffml$Y_hat_log)#0.4616113 0.8527236; 
 median(ffml$Y_hat_log)
 quantile(ffml$Y_hat_log,probs = 0.25) 
 quantile(ffml$Y_hat_log,probs = 0.75) 
 #nonmultilevel range is much lower.....what is going on here? ewhy 
 
 