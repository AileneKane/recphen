#simulate some salmon catch data and fit splits to it
#super useful markdown file on splines in stan!
#https://github.com/milkha/Splines_in_Stan/blob/master/splines_in_stan.Rmd
#https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html
library("splines")
library("rstan")
#try for just one year
dat=fishsum.yr[fishsum.yr$year==1987,]
X <- as.integer(dat$week) #weeks
#num_knots <- 13 #11 interior knots and 2 boundary knots
spline_degree <- 3
#num_basis <- num_knots + spline_degree - 1
#this helps avoid overfitting

#could add a smoothing prior: https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html

#Currently choosing knot location and fitting the B-spline before fitting the stan model
#determine where knots are and how many...currently setting knots every 5 weeks; this yields 11 knots plus the two boundary knots
B <- t(bs(X, knots=seq(min(X),max(X),5), degree=spline_degree, intercept = TRUE)) # creating the B-splines
num_data <- length(X); num_basis <- nrow(B)
Y <- log(dat$chin+1)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("analyses/recanalysis/recmod.stan")

fit<-sampling(sm,iter=500,control = list(adapt_delta=0.95))
plot(fit)
ff<-extract(fit)
Y_hat_med <- array(NA, length(Y))
Y_hat_ub <- array(NA, length(Y))
Y_hat_lb <- array(NA, length(Y))
for (i in 1:length(Y)) {
  Y_hat_med[i] <- median(ff$Y_hat[,i]);
  Y_hat_lb[i] <- quantile(ff$Y_hat[,i],probs = 0.25)
  Y_hat_ub[i] <- quantile(ff$Y_hat[,i],probs = 0.75)
}
plot(X,Y, col="azure4", xlab="week", ylab="log(chincatch)",bty="l")
polygon(c(rev(X), X), c(rev(Y_hat_lb), Y_hat_ub), col = 'grey80', border = NA)
lines(X, Y_hat_med, col="Red", lw=2)
head(summary(fit)$summary)
#NEed to add to recmod.stan:
#1. offset of effort
#2. random effect of year
#3. random effect of site
