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
library(shinystan)

#prepare for stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#Read in WA rec data (just the early years for now)
d<-read.csv("analyses/output/wacrdat_1984_1993.csv", header=TRUE)#for now just use the old data

#prep the data for the model
source("analyses/wcrcstanleadin.R")


#quartz(height=8, width=20)
dat=fishsum[fishsum$year!="1991",]
#remove 1991 for now because so many 0s
years<-unique(dat$year)

yr<-as.integer(as.factor(dat$year))
n_yr<-length(unique(yr))
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
  
fitml<-sampling(smml,iter=1000,warmup=, control = list(adapt_delta=0.8,max_treedepth=15))
#look at sampl params to look at raw chains or 
#check that Yhat and Y indiexing are correct
#probably missing weeks?
#check which weeks are missing frmo particular years
#plot observed vs pred 


traceplot(fitml)
launch_shinystan(fitml)
#quartz()

  #plot(fitml)
  ffml<-extract(fitml)
  ffml.sum<-summary(fitml)$summary
  ffml.sum[grep("a0",rownames(ffml.sum)),]#0.4412101,0.4393600,0.2837746,0.2885874,1.0057101
  ffml.sum[grep("mu",rownames(ffml.sum)),]# 0.15131234
  #ff.sum[grep("a0",rownames(ff.sum)),1]#0.6458231
  ffml.sum[grep("sigma",rownames(ffml.sum)),]#sigma:3.665146e-08; sigma_a_yr: 1.9
  #ff.sum[grep("sigma",rownames(ff.sum)),1]#sigma:0.05880219
  
  range(ffml.sum[grep("a_raw",rownames(ffml.sum)),1])#-0.5684867  0.3963934
  #range(ff.sum[grep("a_raw",rownames(ff.sum)),1])#-1.236066  1.201761
 range(ffml$Y_hat_log)#0.2838755 1.0058113
 median(ffml$Y_hat_log)#0.439461
 quantile(ffml$Y_hat_log,probs = 0.25) 
 quantile(ffml$Y_hat_log,probs = 0.75) 

 