#simulate some salmon catch data and fit splits to it
#super useful markdown file on splines in stan!
#https://github.com/milkha/Splines_in_Stan/blob/master/splines_in_stan.Rmd
#https://mc-stan.org/users/documentation/case-studies/splines_in_stan.html
library("splines")
library("rstan")
X <- seq(from=1, to=53, by=1) # generating inputs, which is weeks
B <- t(bs(X, knots=seq(1,53,4), degree=3, intercept = TRUE)) # creating the B-splines
num_data <- length(X); num_basis <- nrow(B)
c=4#mean log(weeklycatch)  all year- not sure if this is the right way to add this to get the data how i want them
a0 <- 0 # intercept
a <- rnorm(num_basis, 0, 1) # coefficients of B-splines
Y_true <- as.vector(a0*X + a%*%B+ c) # generating the output
Y <- Y_true + rnorm(length(X),0,1) # adding noise
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm<-stan_model("analyses/recmod.stan")
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
plot(X,Y, col="azure4")
polygon(c(rev(X), X), c(rev(Y_hat_lb), Y_hat_ub), col = 'grey80', border = NA)
lines(X, Y_hat_med, col="Red", lw=2)
lines(X, Y_true, col="blue",lw=2)
head(summary(fit)$summary)
