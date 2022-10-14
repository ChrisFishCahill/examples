data {
  int<lower=0> n_data;                     // bounding to be > 0 
  array[n_data] int<lower=0, upper=1> y;   // y is an integer, either 0 or 1
}
parameters {
  real<lower=0, upper=1> theta;            // stuff we are estimating 
}
model {                
  theta ~ beta(1, 1);   // beta prior 
  y ~ bernoulli(theta); // vectorized likelihood
  // NOTE: 
  // stan is setting lp = 
  // log(prior) + log(likelihood) in the background (misery for old timers)
}
