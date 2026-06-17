library(dplyr)
library(tidyr)
library(rstan)

set.seed(2)
N_persons <- 1000
N_items <- 100
theta1 <- rnorm(N_persons, 0, 1)
b1 <- rnorm(N_items)
a1 <- rnorm(N_items, mean = 1, sd = 0.2)
th_mat1 <- matrix(theta1, nrow = N_persons, ncol = N_items, byrow = FALSE)
b_mat1 <- matrix(b1, nrow = N_persons, ncol = N_items, byrow = TRUE)
a_mat1 <- matrix(a1, nrow = N_persons, ncol = N_items, byrow = TRUE)

p_mat1 <- plogis(a_mat1*(th_mat1 - b_mat1))


theta2 <- rnorm(N_persons, 0, 1)
b2 <- runif(N_items, -1, 1)
a2 <- rnorm(N_items, mean = 1, sd = 0.3)
th_mat2 <- matrix(theta2, nrow = N_persons, ncol = N_items, byrow = FALSE)
b_mat2 <- matrix(b2, nrow = N_persons, ncol = N_items, byrow = TRUE)
a_mat2 <- matrix(a2, nrow = N_persons, ncol = N_items, byrow = TRUE)

p_mat2 <- plogis(a_mat2*(th_mat2 - b_mat2))

pi <- pnorm(theta1*.8 + theta2*.5)
#y1hat <- ifelse(theta > 0, theta*.5, theta*1)
y1hat <- theta1*1.2 + theta2*.8 + .2
y0hat <- theta1*1.2 + theta2*.8
cate <- mean(y1hat - y0hat)

# unique iteration
set.seed(i)
z <- rbinom(N_persons, 1, pi)
y1 <- y1hat + rnorm(N_persons)
y0 <- y0hat + rnorm(N_persons)
y <- ifelse(z == 1, y1, y0)

resp1 <- rbinom(N_persons * N_items, 1, p_mat1) |>
  matrix(nrow = N_persons, ncol = N_items) |>
  data.frame()

resp2 <- rbinom(N_persons * N_items, 1, p_mat2) |>
  matrix(nrow = N_persons, ncol = N_items) |>
  data.frame()

# Bayesian Joint Estimation
## item responses for latent trait 1
dat1 <- resp1 |>
  mutate(id = 1:1000) |> 
  pivot_longer(1:100) |>
  mutate(item = as.numeric(stringr::str_sub(name, start = 2)))

## item responses for latent trait 2
dat2 <- resp2 |>
  mutate(id = 1:1000) |> 
  pivot_longer(1:100) |>
  mutate(item = as.numeric(stringr::str_sub(name, start = 2)))

stan_dat <- list(
  jj1 = dat1$id,
  jj2 = dat2$id,
  ii1 = dat1$item, 
  ii2 = dat2$item, 
  correct1 = dat1$value,
  correct2 = dat2$value, 
  J = 1000, 
  I1 = 100, 
  I2 = 100, 
  N1 = nrow(dat1), 
  N2 = nrow(dat2), 
  y = y, 
  z = z, 
  z_pi = z
)

fit <- rstan::stan(file = 'bayes_joint_est_multi_theta.stan', 
                    data = stan_dat,
                    cores = 4, 
                    chains = 4, 
                    control=list(adapt_delta=0.95))
post <- as.matrix(fit)


est <- mean(post[, 'gamma_z'])

lci <- quantile(post[, 'gamma_z'], .025, names = F)
uci <- quantile(post[, 'gamma_z'], .975, names = F)

est
lci;uci
