library(mirt)
library(latex2exp)

set.seed(2)
anxiety_irt <- readr::read_rds('anxiety_irt.rds')
stress_irt <- readr::read_rds('stress_irt.rds')
depression_irt <- readr::read_rds('depression_irt.rds')

params <- coef(anxiety_irt)
params <- do.call('rbind', params[1:14])

params[c(order(params[, 1])[1:5]),1] <- runif(5, 0, .5)
params[c(order(params[, 1])[11:14]), 1] <- runif(4, 2.5, 3.5)

bad_anx_items <- as.data.frame(params)
bad_anx_items$item <- c(2, 4, 7, 9, 15, 19, 20, 23, 25, 28, 30, 36, 40, 41)
bad_anx_items$scale <- 'anxiety'


params <- coef(depression_irt)
params <- do.call('rbind', params[1:14])

params[c(order(params[, 1])[1:5]),1] <- runif(5, 0, .5)
params[c(order(params[, 1])[11:14]), 1] <- runif(4, 2.5, 3.5)

bad_dep_items <- as.data.frame(params)
bad_dep_items$item <- c(3, 5, 10, 13, 16, 17, 21, 24, 26, 31, 34, 37, 38, 42)
bad_dep_items$scale <- 'depression'



params <- coef(stress_irt)
params <- do.call('rbind', params[1:14])

params[c(order(params[, 1])[1:5]),1] <- runif(5, 0, .5)
params[c(order(params[, 1])[11:14]), 1] <- runif(4, 2.5, 3.5)

bad_stress_items <- as.data.frame(params)
bad_stress_items$item <- c(1, 6, 8, 11, 12, 14, 18, 22, 27, 29, 32, 33, 35, 39)
bad_stress_items$scale <- 'stress'

bad_dass <- rbind(bad_anx_items, bad_dep_items, bad_stress_items) |>
  select(a1, item, scale) |>
  dplyr::arrange(item) |>
  rename(`2. heterogenious $\\alpha$` = a1) |>
  mutate(item = paste('Item', item))


anx_items <- as.data.frame(do.call('rbind', coef(anxiety_irt)[1:14]))
anx_items$item <- c(2, 4, 7, 9, 15, 19, 20, 23, 25, 28, 30, 36, 40, 41)
anx_items$scale <- 'anxiety'

dep_items <- as.data.frame(do.call('rbind', coef(depression_irt)[1:14]))
dep_items$item <- c(3, 5, 10, 13, 16, 17, 21, 24, 26, 31, 34, 37, 38, 42)
dep_items$scale <- 'depression'

stress_items <- as.data.frame(do.call('rbind', coef(stress_irt)[1:14]))
stress_items$item <- c(1, 6, 8, 11, 12, 14, 18, 22, 27, 29, 32, 33, 35, 39)
stress_items$scale <- 'stress'


dass <- rbind.data.frame(anx_items, dep_items, stress_items) |>
  select(a1, item, scale) |>
  arrange(item) |>
  rename(`1. homogenious $\\alpha$` = a1) |>
  dplyr::mutate(item = paste('Item', item))



plt_dat <- dass |>
  left_join(bad_dass) |>
  mutate(order = n():1) |>
  pivot_longer(cols = contains('alpha')) |>
  mutate(swap = ifelse('homogenious' %in% name, 1, 2)) |>
  mutate(scale = factor(scale, levels = c('depression', 'anxiety', 'stress')), 
         name = factor(name, levels = c('1. homogenious $\\alpha$', '2. heterogenious $\\alpha$')))
  
plt_dat |>
  ggplot(aes(value, reorder(item, order))) + 
  geom_point() + 
  facet_grid(scale ~ TeX(as.character(name), output = 'character'), 
             scales = 'free_y', 
             labeller = label_parsed) + 
  labs(y = 'Items', x = TeX("IRT slope parameter $\\alpha$")) + 
  theme_bw() + 
  theme(legend.position = 'bottom', panel.grid = element_blank()) 

ggsave('figures/fig1.pdf', width = 7, height = 5)
