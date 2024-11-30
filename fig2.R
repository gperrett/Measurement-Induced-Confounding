library(latex2exp)
library(tidyverse)
source('load_dass.R')


dass <- tibble(
      `$\\hat{\\theta}_{anx.}$` = as.vector(est_theta_anxiety), 
       `$\\hat{\\theta}_{dep.}$`= as.vector(est_theta_depression), 
       `$\\hat{\\theta}_{stress}$` = as.vector(est_theta_stress), 
       `$\\theta_{anx.}$` = as.vector(theta_anx), 
       `$\\theta_{dep.}$` = as.vector(theta_dep), 
       `$\\theta_{stress}$` = as.vector(theta_stress), 
       `$X_{stress}$` = rowSums(sim_stress), 
       `$X_{anx.}$` = rowSums(sim_anxiety), 
       `$X_{dep.}$` = rowSums(sim_depression)
       )

ncol(dass)
dass$`$X_{total}$` <- rowSums(dass[, 7:9])
dass <- dass |> cor()


source('altered_dass.R')
bad_dass <- tibble(
  `$\\hat{\\theta}_{anx.}$` = as.vector(est_theta_anxiety), 
  `$\\hat{\\theta}_{dep.}$`= as.vector(est_theta_depression), 
  `$\\hat{\\theta}_{stress}$` = as.vector(est_theta_stress), 
  `$\\theta_{anx.}$` = as.vector(theta_anx), 
  `$\\theta_{dep.}$` = as.vector(theta_dep), 
  `$\\theta_{stress}$` = as.vector(theta_stress), 
  `$X_{stress}$` = rowSums(sim_stress), 
  `$X_{anx.}$` = rowSums(sim_anxiety), 
  `$X_{dep.}$` = rowSums(sim_depression)
)

bad_dass$`$X_{total}$` <- rowSums(bad_dass[, 7:9])
bad_dass <- bad_dass |> cor()




rows <- rbind(dass, bad_dass) |>
  rownames()

plt_dat <- rbind(dass, bad_dass) |>
  as_tibble() |>
  mutate(rows = rows) |>
  mutate(measure = c(rep('1. homogenious $\\alpha$', 10), 
                     rep('2. heterogenious $\\alpha$', 10))) |>
  pivot_longer(1:10)

plt_dat|>
  ggplot(aes(name, rows, fill = value)) + 
  geom_tile(color = 'black') +
  scale_x_discrete(labels = TeX(plt_dat$name)) + 
  scale_y_discrete(labels = TeX(plt_dat$rows)) + 
  geom_text(aes(name, rows, label = round(value, 2))) +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', midpoint = 0, limits = c(-1, 1)) + 
  facet_wrap(~TeX(as.character(measure), output = 'character'), 
             scales = 'free_y', 
             labeller = label_parsed) + 
  theme_bw() + 
  labs(fill = NULL, x = NULL, y = NULL) + 
  theme(panel.grid = element_blank(), legend.position = 'bottom') 

  
ggsave('figures/fig2.pdf', width = 10, height = 5)

