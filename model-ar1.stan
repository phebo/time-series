data {
  int<lower=0> n;
  vector[n] x;
}

parameters {
  real<lower=0> lambda;
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=0,upper=1> theta;
  theta = -log(lambda);
}

model {
  for(i in 2:n) {
    x[i] - x[i-1] * lambda ~ normal(0, sigma);
  }
  x[1] ~ normal(0, sigma / (1-lambda^2)); // Initial = stationary standard deviation
}
