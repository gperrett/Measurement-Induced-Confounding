dass <- read_csv('depression_anxiety_stress.csv')

dass <- dass |>
  tidyr::pivot_wider(id_cols = id,
                     names_from = item, 
                     values_from = resp, 
                     names_prefix = 'item'
  )

dass <- dass[, 18:59]


depression <- dass[, c(3, 5, 10, 13, 16, 17, 21, 24, 26, 31, 34, 37, 38, 42)]
depression_irt <- mirt(depression, itemtype = 'gpcm')
readr::write_rds(depression_irt, 'depression_irt.rds')

anxiety <- dass[, c(2, 4, 7, 9, 15, 19, 20, 23, 25, 28, 30, 36, 40, 41)]
anxiety_irt <- mirt(anxiety, itemtype = 'gpcm')
readr::write_rds(anxiety_irt, 'anxiety_irt.rds')

stress <- dass[, c(1, 6, 8, 11, 12, 14, 18, 22, 27, 29, 32, 33, 35, 39)]
stress_irt <- mirt(stress, itemtype = 'gpcm')
readr::write_rds(stress_irt, 'stress_irt.rds')
theta_anx <- fscores(anxiety_irt)
theta_dep <- fscores(depression_irt)
theta_stress <- fscores(stress_irt)


sim_depression <- simdata(model = depression_irt,Theta = theta_dep)
readr::write_rds(sim_depression, 'sim_depression.rds')

est_irt_depresson <- mirt(sim_depression, itemtype = 'gpcm')
est_theta_depression <- fscores(est_irt_depresson)

sim_anxiety <- simdata(model = anxiety_irt,Theta = theta_anx)
readr::write_rds(sim_anxiety, 'sim_anxiety.rds')

est_irt_anxiety <- mirt(sim_anxiety, itemtype = 'gpcm')
est_theta_anxiety <- fscores(est_irt_anxiety)

sim_stress <- simdata(model = stress_irt,Theta = theta_stress)
readr::write_rds(sim_stress, 'sim_stress.rds')

est_irt_stress <- mirt(sim_stress, itemtype = 'gpcm')
est_theta_stress <- fscores(est_irt_stress)


