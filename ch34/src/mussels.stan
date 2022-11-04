data {
 int<lower=0> n_data;
 int<lower=0> y_i[n_data];
 vector[n_data] km;
}
parameters{
  real beta0; // intercept
  real beta1; // slope for km
}
transformed parameters {
 vector[n_data] mu; // prediction in normal space

 for(i in 1:n_data){
   mu[i] = exp(beta0 + beta1*km[i]);
 }
}
model {
  // priors 
  beta0 ~ normal(0, 5); 
  beta1 ~ normal(0, 1); 
  
  // likelihood
  y_i ~ poisson(mu); 
}
