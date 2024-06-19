library(tidyverse)
library(tidymodels)
library(conflicted)
conflict_prefer("tune", "tune")
conflict_prefer("slice", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")
conflict_prefer("select", "dplyr")

variables <- "main_vars" 
zq_version <- "normal" 

#
# zq ----------------------------------------------------------------------
## AM zq score refers to the AM PRS variable

dependent_variable <- "AM_zq_score"

group_markov_tbl_subset <- read_rds("synthed_data.rds") %>% 
  nest(.by = subject_id) %>% 
  mutate(
    data = map(data, ~.x %>%
                 mutate(across(contains("AM_zq_score"), ~as.numeric(scale(.x, scale = FALSE))))
    )
  ) %>% unnest(data)

# zq ----------------------------------------------------------------------
dependent_variable <- "AM_zq_score"

n_subs <- length(unique(group_markov_tbl_subset$subject_id))

g_folds <- group_vfold_cv(group_markov_tbl_subset, group = subject_id, v = n_subs) 

recipe <- recipe(AM_zq_score ~ 1, data = group_markov_tbl_subset)

lm_mod <- linear_reg() 


boot_models <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(lm_mod, formula = AM_zq_score ~ 1) %>%
  fit_resamples(resamples = g_folds)


boot_metrics <- boot_models %>% collect_metrics()

test_rmse <- boot_metrics %>% 
  filter(.metric == "rmse") %>% pull(mean) %>% round(.,1)

boot_mets <- boot_models %>% 
  collect_metrics(summarize = FALSE) %>% 
  filter(.metric == "rmse")

boot_cis <- confintr::ci_mean(boot_mets$.estimate)


intercept_result_tbl <- tibble(model = "intercept only centered",
                               dv = dependent_variable,
                               vars = variables,
                               test_rmse = test_rmse,
                               rmse_ci_low = round(boot_cis$interval[1],2),
                               rmse_ci_high = round(boot_cis$interval[2],2),
) %>% 
  mutate(
    across(model:vars, ~as.factor(.))
  )

intercept_result_tbl





set.seed(3332)
group_split <- group_initial_split(group_markov_tbl_subset, group = subject_id)
group_train <- training(group_split) %>% select(all_of(dependent_variable))
group_test <- testing(group_split)  %>% select(all_of(dependent_variable))

mod_train <- lm(AM_zq_score~1, group_train)
intercept <- mod_train %>% tidy() %>% pull(estimate)

summary(mod_train)

group_test_pred <- predict(mod_train, group_test) %>%   bind_cols(group_test) %>% rename(".pred" = ...1)

intercept_metrics <- metric_set(rmse)

test_rmse <- group_test_pred %>% intercept_metrics(group_test_pred[[dependent_variable]], .pred) %>% 
  filter(.metric == "rmse") %>% pull(.estimate) %>% round(.,1)


intercept_result_tbl <- tibble(model = "intercept only non-normed subject split",
                               dv = dependent_variable,
                               vars = variables,
                               test_rmse = test_rmse
) %>% 
  mutate(
    across(model:vars, ~as.factor(.))
  )

intercept_result_tbl

