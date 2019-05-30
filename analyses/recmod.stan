// Model of salmon phenology in Puget Sound using WDFW rec data

data { 
  int num_data; 
  int num_basis; 
  vector[num_data] Y; //log(number of fish caught)
  vector[num_data] X; //week
  //vector[num_data] OFFSET ; //effort= number of anglers
  matrix[num_basis, num_data] B; 
  //int<lower=1> N;
	//int<lower=1> n_yr;
	//int<lower=1, upper=n_r> yr[N];
} 
 
parameters { 
  row_vector[num_basis] a_raw; 
  real a0; 
  real<lower=0> sigma; 
  real<lower=0> tau; 
} 
 
transformed parameters { 
  row_vector[num_basis] a; 
  vector[num_data] Y_hat_log; 
  a = a_raw*tau;  
  Y_hat_log = a0 + to_vector(a*B) ;
} 
 
model { 
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 1); 
  sigma ~ cauchy(0, 1); 
  Y ~ normal(Y_hat_log, sigma); 
} 

