library(dplyr)
library(tidyr)
library(mirt)
library(survey)
library(rstan)

set.seed(2)
N_persons <- 1e3
N_items <- 1e2
# for items, we generate two parameters: difficulty and discrimination

# difficulty is often just simulated normal or uniform. for more info, see:
# https://files.osf.io/v1/resources/jbhxy_v2/providers/osfstorage/67d0bc61c56041fdac00eb4d?action=download&direct&version=1
# measurement error will increase (and be more heterogeneous across respondents)
# if the distribution of theta and b are poorly aligned
b <- rnorm(N_items)

# discrimination - rescales difference between ability and difficulty
# values less than one provide less information about theta,
# values greater than one provide more information about theta
# this should probably be between [0.5, 2] or so unless you're trying to
# simulate something specific
# for some variability:
a <- rnorm(N_items, mean = 1, sd = 0.2)

# value of the latent trait (motivation) 
theta <- rnorm(N_persons) # assume standard normal population ability

# construct response probabilities according to a 2PL logistic IRT model:
th_mat <- matrix(theta, nrow = N_persons, ncol = N_items, byrow = FALSE)
b_mat <- matrix(b, nrow = N_persons, ncol = N_items, byrow = TRUE)
a_mat <- matrix(a, nrow = N_persons, ncol = N_items, byrow = TRUE)

p_mat <- plogis(a_mat*(th_mat - b_mat))

# the assignment mechanism
pi <- pnorm(theta)

# mean strucutre of potential outcomes 
y1hat <- theta*1 + .2
y0hat <- theta*1
cate <- mean(y1hat - y0hat) # definition of CATE

# In causal inference evaluations of CATE population parameters are fixed
# thus theta values, a, and b parameters are fixed acorss iterations
# in each run of our simulation study we re-draw treatment assignment (z), potential outcomes y1 and y0 and item responses
# basically everything with an error term gets redrawn and we re-draw realizations of z

# unique iteration (1-1000)
set.seed(i)
z <- rbinom(N_persons, 1, pi)
y1 <- y1hat + rnorm(N_persons)
y0 <- y0hat + rnorm(N_persons)
y <- ifelse(z == 1, y1, y0)

resp <- rbinom(N_persons * N_items, 1, p_mat) |>
  matrix(nrow = N_persons, ncol = N_items) |>
  data.frame()

# sum scores
theta_hat_sum <- rowSums(as.matrix(resp))

# 2PL IRT model
irt_fit <- mirt(data = resp, itemtype = '2PL')
theta_hat_2PL <- as.vector(fscores(irt_fit))

fit1 <- summary(lm(y ~ z + theta_hat_sum))
fit2 <- summary(lm(y ~ z + theta_hat_2PL))
fit3 <- summary(lm(y ~ z + as.matrix(resp)))

# estimate propensity scores for IRT
pi1 <- predict(glm(z ~ theta_hat_sum, family = binomial()), type = 'response')
pi2 <- predict(glm(z ~ theta_hat_2PL, family = binomial()), type = 'response')
pi3 <- predict(glm(z ~ as.matrix(resp), family = binomial()), type = 'response')

# IPTW estimates
dat <- data.frame(y, z)
w1 <- 1/(ifelse(z == 1, pi1, 1 - pi1))
dsn1 <- svydesign(~1, weights = w1, data = dat)
fit4 <- summary(svyglm(y ~ z, design = dsn1, data = dat))

w2 <- 1/(ifelse(z == 1, pi2, 1 - pi2))
dsn2 <- svydesign(~1, weights = w2, data = dat)
fit5 <- summary(svyglm(y ~ z, design = dsn2, data = dat))

w3 <- 1/(ifelse(z == 1, pi3, 1 - pi3))
dsn3 <- svydesign(~1, weights = w3, data = dat)
fit6 <- summary(svyglm(y ~ z, design = dsn3, data = dat))

# Bayesian Joint Estimation
dat <- resp |>
  mutate(id = 1:1000) |> 
  pivot_longer(1:100) |>
  mutate(item = as.numeric(stringr::str_sub(name, start = 2)))

stan_dat <- list(
  jj = dat$id,
  ii = dat$item, 
  correct = dat$value, 
  J = 1000, 
  I = 100, 
  N = nrow(dat), 
  y = y, 
  z = z, 
  z_pi = z
)

fit7 <- rstan::stan(file = 'bayes_joint_est.stan', 
                   data = stan_dat,
                   cores = 4, 
                   chains = 4, 
                   control=list(adapt_delta=0.95))
post <- as.matrix(fit7)


est <- c(fit1$coef[2, 1], 
         fit2$coef[2, 1], 
         fit3$coef[2, 1], 
         fit4$coef[2, 1], 
         fit5$coef[2, 1], 
         fit6$coef[2, 1], 
         mean(post[, 'beta_z'])
         )

lci <- c(fit1$coef[2, 1] - 1.96*fit1$coef[2, 2], 
         fit2$coef[2, 1] - 1.96*fit2$coef[2, 2], 
         fit3$coef[2, 1] - 1.96*fit3$coef[2, 2], 
         fit4$coef[2, 1] - 1.96*fit4$coef[2, 2], 
         fit5$coef[2, 1] - 1.96*fit5$coef[2, 2], 
         fit6$coef[2, 1] - 1.96*fit6$coef[2, 2], 
         quantile(post[, 'beta_z'], .025, names = F)
)

uci <- c(fit1$coef[2, 1] + 1.96*fit1$coef[2, 2], 
         fit2$coef[2, 1] + 1.96*fit2$coef[2, 2], 
         fit3$coef[2, 1] + 1.96*fit3$coef[2, 2], 
         fit4$coef[2, 1] + 1.96*fit4$coef[2, 2], 
         fit5$coef[2, 1] + 1.96*fit5$coef[2, 2], 
         fit6$coef[2, 1] + 1.96*fit6$coef[2, 2], 
         quantile(post[, 'beta_z'], .975, names = F)
)

out <- data.frame(est = est, 
           lci = lci, 
           uci = uci,
           sd_y = sd(y), 
           truth = cate, 
           model = c('response surface: sum score', 
                     'response surface: IRT', 
                     'response surface: X', 
                     'IPTW: sum score', 
                     'IPTW: IRT', 
                     'IPTW: X',
                     'Bayesian Joint Estimation'
                     ))
path <- paste0('output/output', i, '.csv')
readr::write_csv(out, path)
