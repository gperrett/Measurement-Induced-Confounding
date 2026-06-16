data {
  int<lower=1> I; // # questions
  int<lower=1> J; // # persons
  int<lower=1> N; // # observations
  int<lower=1, upper=I> ii[N]; // question for n
  int<lower=1, upper=J> jj[N]; // person for n
  int<lower=0, upper=1> correct[N]; // correctness for n
  vector[J] y; //outcome variable
  vector[J] z;
  int<lower=0,upper=1> z_pi[J]; //treatment

}
parameters {
  vector<lower=0>[I] alpha; // discrimination for item i
  vector[I] beta; // difficulty for item i
  vector[J] theta; // ability for person j
  real beta_z; // ate
  real beta_theta; // prognostic
  real beta0; // intercept
  real<lower=0> sigma; // residual standard error
  real beta0_pi;
  real beta_pi;
}
model {
  vector[N] eta;
  alpha ~ exponential(1);
  beta ~ normal(0,10);
  theta ~ normal(0,1);
  beta_z ~ normal(0, 5);
  beta_theta ~ normal(0, 5);
  beta0 ~ normal(0, 5);
  sigma ~ exponential(1);
  beta_pi ~ normal(0, 5);
  beta0_pi ~ normal(0, 5);
  z_pi ~ bernoulli_logit(beta0_pi + theta*beta_pi);
  for (n in 1:N)
    eta[n] = alpha[ii[n]] * (theta[jj[n]] - beta[ii[n]]);
  correct ~ bernoulli_logit(eta);
  y ~ normal(beta0 + theta*beta_theta + z*beta_z, sigma);
}
