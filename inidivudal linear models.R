library(tidyverse)
library(tidymodels)

lm_boots_rmse_fn_zq <- function(data){
  
  boots <- bootstraps(data, times = 500)
  lm_rec <- recipe(AM_zq_score ~ lag1_AM_zq_score + AM_soreness + AM_stress + AM_sleep_qualilty + lag2_AM_zq_score, data ) 
  lm_wf <- workflow() %>%
    add_recipe(lm_rec) %>%
    add_model(linear_reg() %>%
                set_engine("lm"))
  
  resamps <- fit_resamples(lm_wf, boots, control = control_resamples(parallel_over = "everything"))
  
  metrics <- resamps %>% collect_metrics() 
  
  metrics
  
}
lm_boots_rmse_fn_hrv <- function(data){
  
  boots <- bootstraps(data, times = 500)
  lm_rec <- recipe(hrv_chng ~ lag1_roll_7d_avg_hrv_delta + lag1_hrv_chng + lag7_hrv_chng + lag1_hrv +
                     lag2_hrv_chng, data) 
  lm_wf <- workflow() %>%
    add_recipe(lm_rec) %>%
    add_model(linear_reg() %>%
                set_engine("lm"))
  
  resamps <- fit_resamples(lm_wf, boots, control = control_resamples(parallel_over = "everything"))
  
  metrics <- resamps %>% collect_metrics() #%>% filter(.metric == "rmse") %>% pull(mean)
  
  metrics
  
}

intercept_boots_rmse_fn_zq <- function(data){
  
  boots <- bootstraps(data, times = 500)
  lm_rec <- recipe(AM_zq_score ~ 1, data ) 
  lm_wf <- workflow() %>%
    add_recipe(lm_rec) %>%
    add_model(linear_reg() %>%
                set_engine("lm"))
  
  resamps <- fit_resamples(lm_wf, boots, control = control_resamples(parallel_over = "everything"))
  
  metrics <- resamps %>% collect_metrics() %>% filter(.metric == "rmse") %>% pull(mean)
  
  metrics
  
}
intercept_boots_rmse_fn_hrv <- function(data){
  
  boots <- bootstraps(data, times = 500)
  lm_rec <- recipe(hrv_chng ~ 1, data) 
  lm_wf <- workflow() %>%
    add_recipe(lm_rec) %>%
    add_model(linear_reg() %>%
                set_engine("lm"))
  
  resamps <- fit_resamples(lm_wf, boots, control = control_resamples(parallel_over = "everything"))
  
  metrics <- resamps %>% collect_metrics() %>% filter(.metric == "rmse") %>% pull(mean)
  
  metrics
  
}

# lm on each person based on the top 5 variables from the group
group_tbl <- read_rds("synthed_data.rds")

individ_lm_map_tbl <- group_tbl %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    zq_boots_metrics = map(data, ~ lm_boots_rmse_fn_zq(.x)),
    hrv_boots_metrics = map(data, ~ lm_boots_rmse_fn_hrv(.x)),
    zq_lm_intercept_rmse = map_dbl(data, ~ intercept_boots_rmse_fn_zq(.x)),
    hrv_lm_intercept_rmse = map_dbl(data, ~ intercept_boots_rmse_fn_hrv(.x)),
    people = "Individual models"
  ) 


boots_lm_metrics <- individ_lm_map_tbl %>% 
  unnest(zq_boots_metrics) %>% select(-c(people, .estimator, n:.config)) %>% pivot_wider(names_from = ".metric", values_from = "mean") %>% rename("zq_boots_rmse" = rmse, "zq_boots_rsq" = rsq) %>% 
  unnest(hrv_boots_metrics) %>% select(-c(.estimator, n:.config)) %>% pivot_wider(names_from = ".metric", values_from = "mean") %>% rename("hrv_boots_rmse" = rmse, "hrv_boots_rsq" = rsq) %>% 
  pivot_longer(zq_lm_intercept_rmse:hrv_boots_rsq) %>% 
  mutate(
    vars =  ifelse(str_detect(name, "intercept"), "intercept only", "top_5_from_stack"),
    dv = case_when(
      str_detect(name, "zq") ~ "AM PRS",
      str_detect(name, "hrv") ~ "HRV change"),
    name = str_remove_all(name, "(zq_boots_)|(tf_boots_)|(hrv_boots_)|(zq_lm_intercept_)|(tf_lm_intercept_)|(hrv_lm_intercept_)")
  ) %>% 
  rename("metric" = name) %>% 
  mutate(
    across(c(metric, dv, vars), ~as.factor(.)),
  ) %>% select(-c(data)) %>% 
  pivot_wider(names_from = "metric", values_from = "value") 

boots_lm_metrics
