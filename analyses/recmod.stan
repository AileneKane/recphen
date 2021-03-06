// Model of salmon phenology in Puget Sound using WDFW rec data

data { 
  int N; 
  int num_basis; 
  vector[N] Y; //log(number of fish caught)
  vector[N] X; //week
  //vector[num_data] OFFSET ; //effort= number of anglers
  matrix[num_basis, N] B; 
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
  vector[N] Y_hat_log; 
  a = a_raw*tau;  
  Y_hat_log = a0 + to_vector(a*B) ;
} 
 
model { 
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 1); 
  sigma ~ cauchy(0, 1); 
  Y ~ normal(Y_hat_log, sigma); 
} 

