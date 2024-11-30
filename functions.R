causal_sim <- function(y1, y0, pscores, est_theta_anxiety, est_theta_stress, est_theta_depression, stress_dat, anxiety_dat, depression_dat){
  
  ## realize POs
  #y0 <- y0hat + rnorm(n, 0, 1)
  #y1 <- y1hat + rnorm(n, 0, 1)
  
  z <- rbinom(n, 1, pscores)
  
  y <- ifelse(z == 1, y1, y0)
  
  ## generate predictors
  stress_sum <- rowSums(stress_dat)
  anx_sum <- rowSums(anxiety_dat)
  dep_sum <- rowSums(depression_dat)
  sum_score <-  stress_sum + anx_sum + dep_sum

  irt_fit <- lm(y ~ z + est_theta_anxiety + est_theta_depression + est_theta_stress)
  lm_irt_coef <- irt_fit$coef['z']
  lm_irt_ci <- confint(irt_fit)['z', ]
  
  sum_total_fit <- lm(y~ z + sum_score)
  lm_sum_score_coef <- sum_total_fit$coef['z']
  lm_sum_score_ci <- confint(sum_total_fit)['z',]
  
  sum_scale_fit <- lm(y~ z + anx_sum + dep_sum + stress_sum)
  lm_sum_scale_coef <- sum_scale_fit$coef['z']
  lm_sum_scale_ci <- confint(sum_scale_fit)['z',]
  
  
  
  item_dat <- cbind.data.frame(sim_depression, sim_anxiety, sim_stress)
  #item_dat <- as.data.frame(apply(item_dat, 2, as.factor))
  names(item_dat) <- paste0('X', 1:length(item_dat))
  
  item_fit <- lm(y~ z + ., data = item_dat)
  lm_item_coef <- item_fit$coef['z']
  lm_item_ci <- confint(item_fit)['z',]
  
  
  
  out <- data.frame(sate = mean(y1 - y0), 
             lm_irt_coef, lm_irt_lci = lm_irt_ci[1], lm_irt_uci = lm_irt_ci[2], 
             lm_sum_score_coef, lm_sum_score_lci = lm_sum_score_ci[1], lm_sum_score_uci = lm_sum_score_ci[2],
             lm_sum_scale_coef, lm_sum_scale_lci = lm_sum_scale_ci[1], lm_sum_scale_uci = lm_sum_scale_ci[2], 
             lm_item_coef, lm_item_lci = lm_item_ci[1], lm_item_uci = lm_item_ci[2], 
             sd_y = sd(y))
  
  out <- out |>
    mutate(lm_irt_cover = ifelse(lm_irt_lci <= sate & lm_irt_uci >= sate, 1, 0), 
           lm_sum_score_cover = ifelse(lm_sum_score_lci <= sate & lm_sum_score_uci >= sate, 1, 0), 
           lm_sum_scale_cover = ifelse(lm_sum_scale_lci <= sate & lm_sum_scale_uci >= sate, 1, 0), 
           lm_item_cover = ifelse(lm_item_lci <= sate & lm_item_uci >= sate, 1, 0) 
    )
  
  out
  
}
