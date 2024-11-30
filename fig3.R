source('load_dass.R')
source('dass_latent_latent.R')
source('dass_latent_item.R')
source('dass_item_item.R')



dass_sims <- rbind(
item_item |> 
  bind_rows() |>
  mutate(dgp = 'item response, item assignment'),

latent_item |> 
  bind_rows() |>
  mutate(dgp = 'latent response, item assignment'),

latent_latent |> 
  bind_rows() |>
  mutate(dgp = 'latent response, latent assignment')
)



dass_sims |>
  bind_rows() |>
  select(sate, sd_y, contains('coef'), dgp) |>
  pivot_longer(3:6) |>
  mutate(zbias = (sate - value)/sd_y) |>
  ggplot(aes(zbias, fill = name)) + 
  geom_density(alpha = .6) + 
  facet_wrap(~dgp, scales = 'free_y') + 
  geom_vline(xintercept = 0) + 
  theme_classic()


zbias <- dass_sims |>
  bind_rows() |>
  select(sate, sd_y, contains('coef'), dgp) |>
  pivot_longer(3:6) |>
  mutate(zbias = (sate - value)/sd_y) |>
  group_by(name, dgp) |>
  summarise(zbias = mean(zbias)) |>
  ungroup()

cover <- dass_sims |>
  bind_rows() |>
  select(sate, sd_y, contains('cover'), dgp) |>
  pivot_longer(3:6) |>
  group_by(name, dgp) |>
  summarise(coverage = mean(value)) |> 
  ungroup()


fig3 <- cbind(zbias, coverage = cover$coverage)


fig3 |>
  mutate(name = case_when(
    name == 'lm_irt_coef' ~ 'IRT', 
    name == 'lm_sum_score_coef' ~ 'Sum Score', 
    name == 'lm_sum_scale_coef' ~ 'Sub-Scale Score',
    name == 'lm_sum_score_coef' ~ 'Sub-Scale Score', 
    name == 'lm_item_coef' ~ 'Individual Items'
  )) |>
  mutate(dgp = case_when(
    dgp == 'latent response, latent assignment' ~ 'DGP1: latent response, latent assignment', 
    dgp == 'latent response, item assignment' ~ 'DGP2: latent response, item assignment', 
    dgp == 'item response, item assignment' ~ 'DGP3: item response, item assignment'
  )) |>
  ggplot(aes(zbias, coverage, col = name)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = .95, linetype = 2) + 
  #scale_x_continuous()
  geom_point(size = 2.5) + 
  facet_wrap(~dgp) + 
  labs(x = 'Standardized Bias', y = 'Coverage', col = NULL) +  
  theme_bw() + 
  theme(legend.position = 'bottom', panel.grid = element_blank()) 


ggsave('figures/fig3.pdf', width = 8, height = 3)


