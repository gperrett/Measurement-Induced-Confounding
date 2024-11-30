library(mirt)
library(tidyverse)
sim_anxiety <- read_rds('sim_anxiety.rds')
sim_stress <- read_rds('sim_stress.rds')
sim_depression <- read_rds('sim_depression.rds')

anxiety_irt <- read_rds('anxiety_irt.rds')
theta_anx <- fscores(anxiety_irt)

depression_irt <- read_rds('depression_irt.rds')
theta_dep <- fscores(depression_irt)


stress_irt <- read_rds('stress_irt.rds')
theta_stress <- fscores(stress_irt)

sim_depression <- read_rds('sim_depression.rds')
est_irt_depresson <- mirt(sim_depression, itemtype = 'gpcm')
est_theta_depression <- fscores(est_irt_depresson)


sim_anxiety <- read_rds('sim_anxiety.rds')
est_irt_anxiety <- mirt(sim_anxiety, itemtype = 'gpcm')
est_theta_anxiety <- fscores(est_irt_anxiety)


sim_stress <- read_rds('sim_stress.rds')
est_irt_stress <- mirt(sim_stress, itemtype = 'gpcm')
est_theta_stress <- fscores(est_irt_stress)

