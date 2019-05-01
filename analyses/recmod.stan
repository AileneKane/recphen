// Model of salmon phenology in Puget Sound using WDFW rec data

data { 
  int num_data; 
  int num_basis; 
  real OFFSET;//effort= //log(number of anglers)
  vector[num_data] Y; //log(number of fish caught)
  vector[num_data] X; //week
  matrix[num_basis, num_data] B; 
} 
 
parameters { 
  row_vector[num_basis] a_raw; 
  real a0; 
  real<lower=0> sigma; 
  real<lower=0> tau; 
} 
 
transformed parameters { 
  row_vector[num_basis] a; 
  vector[num_data] Y_hat; 
  a = a_raw*tau;  
  Y_hat = a0*X + to_vector(a*B) + OFFSET;
} 
 
model { 
  a_raw ~ normal(0, 1); 
  tau ~ cauchy(0, 1); 
  sigma ~ cauchy(0, 1); 
  Y ~ normal(Y_hat, sigma); 
} 
