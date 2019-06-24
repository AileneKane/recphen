// Model of salmon phenology in Puget Sound using WDFW rec data

data { 
  int<lower=1> N;
  int num_basis; 
  vector[N] Y; //log(number of fish caught)
  vector[N] X; //week
  //vector[num_data] OFFSET ; //effort= number of anglers
  matrix[num_basis, N] B; 
	//int<lower=1> n_reg
	//int<lower=1, upper=n_reg> reg[N];
	int<lower=1> n_yr;
	int<lower=1, upper=n_yr> yr[N];
} 
 
parameters { 
  row_vector[num_basis] a_raw; 
  vector[n_yr] a0; //offset for year
  real<lower=0> sigma; 
  real<lower=0> tau; 
  real mu_a_yr; 
	real <lower=1> sigma_a_yr; 
} 
 
transformed parameters { 
  row_vector[num_basis] a; 
  vector[N] Y_hat_log; 
  a = a_raw*tau;  
  Y_hat_log = a0[yr] + to_vector(a*B);

} 
 
 
model { 
  mu_a_yr ~ normal(0, 1);
  sigma_a_yr ~ normal(0, 1);
  a0 ~ normal(mu_a_yr, sigma_a_yr); 
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 1); 
  sigma ~ cauchy(0, 1); 
  Y ~ normal(Y_hat_log, sigma); 
} 



