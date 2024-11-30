library(tidyverse)
library(mirt)

source('functions.R')
#source('altered_dass.R')


set.seed(2)
n <- nrow(theta_dep)
asn_beta <- runif(6, .3, .9)
probit <- sim_anxiety[,c(14,2,10,3, 7, 12)] %*% asn_beta + sim_depression[, 13]*.8 + sim_stress[, 4]*.4
probit <- probit*.2
pscores <- pnorm(-2 + probit)

y0hat <- theta_dep*.6 + theta_anx*1.2 + theta_stress*.5
y1hat  <- theta_dep*.6 + theta_anx*1.2 + theta_stress*.5 + -.25*sd(y0hat)
y0 <- y0hat + rnorm(n, 0, 1)
y1 <- y1hat + rnorm(n, 0, 1)

bad_latent_item <- parallel::mclapply(1:1000, function(i){
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

