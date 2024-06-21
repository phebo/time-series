data {
  int<lower=0> n;
  vector[n] x;
}

parameters {
  real<lower=0> theta;
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=0,upper=1> lambda;
  lambda = exp(-theta);
}

model {
  x[1] ~ normal(0, sigma / sqrt(1 - lambda*lambda));
  for(i in 2:n) {
    x[i] - x[i-1] * lambda ~ normal(0, sigma);
  }
}