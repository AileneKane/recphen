#simulate some salmon catch data and fit splits to it
#super useful markdown file on splines in stan!
#https://github.com/milkha/Splines_in_Stan/blob/master/splines_in_stan.Rmd
#https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html

#housekeeping
rm(list=ls()) 
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

#try one year at a time for now
years<-unique(fishsum.yr$year)

quartz(height=8, width=20)
par(mfrow=c(2,3))

#fit a separate model for each year (for all sites in puget sound)
for (y in 1:length(years)){
  dat=fishsum.yr[fishsum.yr$year==years[y],]
  X <- as.integer(dat$week) #weeks
  #num_knots <- 13 #11 interior knots and 2 boundary knots
  spline_degree <- 3
  #num_basis <- num_knots + spline_degree - 1
  
  #could add a smoothing prior (to help with overfitting): https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html 
  
  #Currently choosing knot location and fitting the B-spline before fitting the stan model
  #Setting knots every 6 weeks- why? I don't know!; this yields 11 knots plus the two boundary knots
  
  B <- t(bs(X, knots=seq(1,52,4), degree=spline_degree, intercept = TRUE)) # creating the B-splines
  N <- length(X); 
  num_basis <- nrow(B)
  Y <- log(dat$chin+.001)
  OFFSET<-log(dat$anglers)
  Y<-Y.offset<- log(dat$chin+.001)/log(dat$anglers+.001)
  
  sm<-stan_model("analyses/recmod.stan")
  
  fit<-sampling(sm,iter=500,control = list(adapt_delta=0.95))
  ff<-extract(fit)
  ff.sum<-summary(fit)$summary
  ff.sum[grep("a0",rownames(ff.sum)),1]#0.6458231
  range(ff.sum[grep("a_raw",rownames(ff.sum)),1])#-1.236066  1.201761
  ff.sum[grep("sigma",rownames(ff.sum)),1]#sigma:0.05880219
  #plot(fit)
  ff<-extract(fit)
  Y_hat_med <- array(NA, length(Y))
  Y_hat_ub <- array(NA, length(Y))
  Y_hat_lb <- array(NA, length(Y))
  for (i in 1:length(Y)) {
    Y_hat_med[i] <- median(ff$Y_hat_log[,i])
    Y_hat_lb[i] <- quantile(ff$Y_hat_log[,i],probs = 0.25)
    Y_hat_ub[i] <- quantile(ff$Y_hat_log[,i],probs = 0.75)
  }
  plot(X,Y, col="azure4", type="p",pch=21,xlab="week", ylab="log(chincatch)",bty="l", main=paste(years[y]), ylim=c(0.4,1.2))
  polygon(c(rev(X), X), c(rev(Y_hat_lb), Y_hat_ub), col = 'grey80', border = NA)
  lines(X, Y_hat_med, col="Red", lw=2)
  
  abline(v=which(Y_hat_med==max(Y_hat_med)), col="blue", lty=2, lwd=2)
  text(which(Y_hat_med==max(Y_hat_med))+1,1,labels=as.character(paste(which(Y_hat_med==max(Y_hat_med)))), col="blue", cex=1.5)
  abline(v=which(Y_hat_med==max(Y_hat_med[10:20], na.rm=TRUE)), col="gray", lty=2, lwd=1)
  text(which(Y_hat_med==max(Y_hat_med[10:20], na.rm=TRUE))+1,1,labels=as.character(paste(which(Y_hat_med==max(Y_hat_med[10:20], na.rm=TRUE)))), cex=1.2)
  
  abline(v=which(Y_hat_med==max(Y_hat_med[20:40], na.rm=TRUE)), col="gray", lty=2, lwd=1)
  text(which(Y_hat_med==max(Y_hat_med[20:40], na.rm=TRUE))+1,1,labels=as.character(paste(which(Y_hat_med==max(Y_hat_med[20:40], na.rm=TRUE)))), cex=1.2)
  
  abline(v=which(Y_hat_med==max(Y_hat_med[40:53], na.rm=TRUE)), col="gray", lty=2, lwd=1)
  text(which(Y_hat_med==max(Y_hat_med[20:53], na.rm=TRUE))+1,1,labels=as.character(paste(which(Y_hat_med==max(Y_hat_med[20:53], na.rm=TRUE)))), cex=1.2)
  
  print(paste("peak abundance week:",which(Y_hat_med==max(Y_hat_med)), sep=""))
  print(paste("peak abundance week, between weeks 10-20:",which(Y_hat_med==max(Y_hat_med[10:20], na.rm=TRUE)), sep=""))
  print(paste("peak abundance week, between weeks 20-40:",which(Y_hat_med==max(Y_hat_med[20:40], na.rm=TRUE)), sep=""))
  print(paste("peak abundance week, between weeks 40-53:",which(Y_hat_med==max(Y_hat_med[40:53], na.rm=TRUE)), sep=""))
}

#do same thing but for a few areas that have lots of catch

#crcs<-unique(d$CRCArea)
#table(d$CRCArea,d$year)#5, 6, 7 9 10,11,12,13 have good numbers
table(d$ChinCaught,d$year)


#Need to add to recmod.stan:
#1. Fix offset of effort- does not seem to be working the way i added it to the model (need to get logging right)
#2. Also: why is spline for 1991 fitting so poorly?
#3. random effect of year
#First do above with a few areas that have a lot of data (7, 4, 9)- somewhere in south sound. compare phenological pattern.

#4. random effect of site
#5. account for regulations

#Data cleaning/imputing questions:
#1) #There are some rows (70) for which coho were cuahgt but anglers=NA. replace with 1? or how to deal with this?
#perhaps fill with mean anglers per coho...range of coho caught on these dates is 1-8.range of chinook is 1-3 (fewer rows- 34- of chinook with anglers=na- )


