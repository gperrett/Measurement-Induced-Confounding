library(tidyverse)
library(mirt)

source('functions.R')


set.seed(2)
n <- nrow(theta_dep)
asn_beta <- runif(6, .3, .9)
probit <- sim_anxiety[,c(14,2,10,3, 7, 12)] %*% asn_beta + sim_depression[, 13]*.8 + sim_stress[, 1]*.4
probit <- probit*.2
pscores <- pnorm(-2 + probit)


beta_anx <- runif(6, .2, .7)
beta_dep <- runif(ncol(sim_depression), .1, .4)
beta_stress <- runif(ncol(sim_stress), .1, .2)
 


y0hat <- sim_anxiety[,c(14,2,10,3, 7, 12)] %*% beta_anx + sim_depression %*% beta_dep + sim_stress %*%beta_stress
y1hat  <- y0hat + -.25*sd(y0hat)

y0 <- y0hat + rnorm(n, 0, 1)
y1 <- y1hat + rnorm(n, 0, 1)

item_item <- parallel::mclapply(1:1000, function(i){
  causal_sim(y1 = y1, 
             y0 = y0, 
             pscores = pscores, 
             est_theta_anxiety = est_theta_anxiety, 
             est_theta_depression = est_theta_depression, 
             est_theta_stress = est_theta_stress, 
             depression_dat = sim_depression, 
             anxiety_dat = sim_anxiety, 
             stress_dat = sim_stress
  )
}, mc.cores = 8)
