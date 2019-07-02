// Model of salmon phenology in Puget Sound using WDFW rec data

data { 
  int<lower=1> N;
  int num_basis;
  int N_weeks ; /// THIS IS NEW
  vector[N] Y; //log(number of fish caught)
  vector[N] X; //week
  //vector[num_data] OFFSET ; //effort= number of anglers
  matrix[num_basis, N_weeks] B;  /// THIS IS DIFFERENT
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
  vector[N_week] Y_hat_log[N_yr]; /// THIS IS NEW
  a = a_raw*tau;  
  
  /// ADD A LOOP HERE
  // version 1
  for( i in 1:N_yr)
  Y_hat_log[i] = a0[i] + to_vector(a*B) + to_vector(WIGGLES[i]*B) ; /// THIS IS NEW
  }
  
  // .* means element by element multiplication.
  //version 2 - should be the same answer as v1
  Y_hat_log = a0 + to_vector(a*B) ; /// THIS IS NEW
} 
 
 
model { 
  mu_a_yr ~ normal(0, 1);
  sigma_a_yr ~ normal(0, 1);
  a0 ~ normal(mu_a_yr, sigma_a_yr); 
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 1); 
  sigma ~ cauchy(0, 1); 
  for(i in  1:n_yr){ /// THIS IS NEW 
    Y[i] ~ normal(Y_hat_log[i], sigma); 
    
    Y[i] ~ normal(a0[i] + to_vector(a*B),sigma);
    
  }
} 



