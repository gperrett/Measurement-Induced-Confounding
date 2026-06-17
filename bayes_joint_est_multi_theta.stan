data {
  int<lower=1> I1; // # n items for latent trait 1
  int<lower=1> I2; // # n items for latent trait 2
  int<lower=1> J; // # persons
  int<lower=1> N1; // # person x item (1)
  int<lower=1> N2; // # person x item (2)
  int<lower=1, upper=I1> ii1[N1]; // item id for latent trait 1
  int<lower=1, upper=I2> ii2[N2]; // item id for latent trait 2
  int<lower=1, upper=J> jj1[N1]; // person item id for latent trait 1
  int<lower=1, upper=J> jj2[N2]; // person item id for latent trait 2
  int<lower=0, upper=1> correct1[N1]; // correctness for latent triat 1
  int<lower=0, upper=1> correct2[N2]; // correctness for latent triat 2
  vector[J] y; //outcome variable
  vector[J] z; // treatment variable
  int<lower=0,upper=1> z_pi[J]; //treatment variable for assignment model

}
parameters {
  vector<lower=0>[I1] alpha1; // discrimination (1)
  vector<lower=0>[I2] alpha2; // discrimination (2)
  vector[I1] b1; // difficulty (1)
  vector[I2] b2; // difficulty (1)
  vector[J] theta1; // ability (1)
  vector[J] theta2; // ability (2)

  real gamma0; // intercept
  real gamma_z; // ate
  vector[2] gamma; // prognostic
  real<lower=0> sigma; // residual standard error
  
  real beta0;
  vector[2] beta; // assignment theta

}
model {
  vector[N1] eta1;
  vector[N2] eta2;

  alpha1 ~ exponential(1);
  alpha2 ~ exponential(1);
  b1 ~ normal(0,10);
  b2 ~ normal(0,10);
  theta1 ~ normal(0,1);
  theta2 ~ normal(0,1);
  
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
  
  gamma0 ~ normal(0, 5);
  gamma_z ~ normal(0, 5);
  gamma ~ normal(0, 5);

  z_pi ~ bernoulli_logit(beta0 + theta1*beta[1] + theta2*beta[2]);
  
  for (n1 in 1:N1)
    eta1[n1] = alpha1[ii1[n1]] * (theta1[jj1[n1]] - b1[ii1[n1]]);
  correct1 ~ bernoulli_logit(eta1);
  
  for (n2 in 1:N2)
    eta2[n2] = alpha2[ii2[n2]] * (theta2[jj2[n2]] - b2[ii2[n2]]);
  correct2 ~ bernoulli_logit(eta2);
  
  y ~ normal(gamma0 + z*gamma_z + theta1*gamma[1] + theta2*gamma[2], sigma);
}
