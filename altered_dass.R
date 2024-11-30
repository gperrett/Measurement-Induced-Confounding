library(mirt)
library(tidyverse)
set.seed(2)
anxiety_irt <- readr::read_rds('anxiety_irt.rds')
stress_irt <- readr::read_rds('stress_irt.rds')
depression_irt <- readr::read_rds('depression_irt.rds')

params <- coef(anxiety_irt)
params <- do.call('rbind', params[1:14])

params[c(order(params[, 1])[1:5]),1] <- runif(5, 0, .5)
params[c(order(params[, 1])[11:14]), 1] <- runif(4, 2.5, 3.5)


sim_anxiety <- simdata(N = 10000, a = params[,1], d = params[, 6:9], itemtype = 'gpcm',returnList = T)

est_irt_anxiety <- mirt(sim_anxiety$data)

est_theta_anxiety <- fscores(est_irt_anxiety)



params <- coef(stress_irt)
params <- do.call('rbind', params[1:14])

params[c(order(params[, 1])[1:5]),1] <- runif(5, 0, .5)
params[c(order(params[, 1])[11:14]), 1] <- runif(4, 2.5, 3.5)

sim_stress <- simdata(N = 10000, a = params[,1], d = params[, 6:9], itemtype = 'gpcm',returnList = T)

est_irt_stress <- mirt(sim_stress$data)

est_theta_stress <- fscores(est_irt_stress)




params <- coef(depression_irt)
params <- do.call('rbind', params[1:14])

params[c(order(params[, 1])[1:5]),1] <- runif(5, 0, .5)
params[c(order(params[, 1])[11:14]), 1] <- runif(4, 2.5, 3.5)

sim_depression <- simdata(N = 10000, a = params[,1], d = params[, 6:9], itemtype = 'gpcm',returnList = T)

est_irt_depression <- mirt(sim_depression$data)

est_theta_depression <- fscores(est_irt_depression)



theta_dep <- sim_depression$Theta
theta_anx <- sim_anxiety$Theta
theta_stress <- sim_stress$Theta

sim_anxiety <- sim_anxiety$data
sim_depression <- sim_depression$data
sim_stress <- sim_stress$data

