library(tidyverse)
library(tidymodels)
library(bonsai)
library(caret)
library(Cairo)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("annotate", "ggplot2")
conflict_prefer("lag", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("map", "purrr")

source("helper functions.R")

## AM zq score refers to the AM PRS variable
dependent_variable <- "AM_zq_score"   #   AM_zq_score    hrv_chng  
variables <- "main_vars"  #    main_vars     actionable_vars     

zq_version <- "normal" # alternatively could have used box-cox transformations
tf_version <- "normal" # leave as-is for functions

# read data ---------------------------------------------------------------
group_tbl <- read_rds("synthed_data.rds")

#select vars
group_markov_tbl_subset <- nzv_fct_var_remove_fn(group_tbl)


n_subs <- length(unique(group_markov_tbl_subset$subject_id))


#center by subject
group_markov_tbl_subset <- group_markov_tbl_subset %>% 
  nest(.by = subject_id) %>% 
  mutate(
    data = map(data, ~.x %>%
                 mutate(across(contains("AM_zq_score"), ~as.numeric(scale(.x, scale = FALSE))))
    )
  ) %>% unnest(data)

group_markov_tbl_subset %>% 
  # select(contains("AM_zq_score"))
  ggplot(aes(AM_zq_score)) + geom_histogram()
#

# remove cor'd ------------------------------------------------------------

# remove highly correlated variables
only_numeric <- group_markov_tbl_subset %>% select(where(is.numeric)) %>% 
  select(-matches("(^AM_zq)|(study_day)|(subject)|(lambda)")) 

cor_mat <- cor(only_numeric, use = "pairwise.complete.obs")
full_cor_to_remove <- caret::findCorrelation(cor_mat, 0.85, names = T, exact = T) %>% 
  str_c(., "$", collapse = ")|(") %>% 
  str_c("(", ., ")")


# subset ------------------------------------------------------------------
group_markov_tbl_subset <- group_markov_tbl_subset %>%  select(-matches(full_cor_to_remove), -study_day)  

#
# CV folds ---------------------------------------------------------------

group_folds <- group_vfold_cv(group_markov_tbl_subset, group = subject_id, v = n_subs)

# model specs --------------------------------------------------------------------

pen_vals <- 10^seq(-3, 0, length.out = 50)

lasso_linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine('glmnet', path_values = pen_vals)

ridge_linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine('glmnet', path_values = pen_vals)

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = 1500, learn_rate = tune(), loss_reduction = tune(), min_n = tune()) %>%
  set_engine('xgboost', importance = TRUE) %>%
  set_mode('regression')

lightgbm_model <-
  parsnip::boost_tree(
    mode = "regression", trees = 1500, min_n = tune(),  tree_depth = tune()) %>%
  set_engine("lightgbm")

svm_poly_kernlab_spec <-
  svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('regression')

nearest_neighbor_kknn_spec <-
  nearest_neighbor(neighbors = tune()) %>%
  set_engine('kknn') %>%
  set_mode('regression')

mars_earth_spec <-
  mars(prod_degree = tune(), num_terms = tune()) %>%
  set_engine('earth') %>%
  set_mode('regression')

nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("regression")

# recipe ------------------------------------------------------------------

group_basic_rec <-   
  recipe(AM_zq_score ~ .,
         data = group_markov_tbl_subset)  %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_lincomb(all_numeric_predictors()) %>%
  step_nzv(all_numeric_predictors())

group_basic_rec  %>% prep() %>% juice() %>% glimpse()
group_basic_juiced <- group_basic_rec  %>% prep() %>% juice() 


group_basic_rec_xgb <-   
  recipe(AM_zq_score ~ .,
         data = group_markov_tbl_subset)  %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = T) %>%
  step_lincomb(all_numeric_predictors()) %>%
  step_nzv(all_numeric_predictors())


group_basic_rec_xgb  %>% prep() %>%  juice() %>% glimpse()
group_basic_rec_xgb_juiced <- group_basic_rec_xgb  %>% prep() %>%  juice() 


group_interact_rec <-   
  recipe(AM_zq_score ~ .,
         data = group_markov_tbl_subset) %>%
  step_nzv(all_numeric_predictors()) %>%
  step_interact(~ lag1_roll_7d_exercise_load:lag1_diet_carb_g_kg) %>%
  step_interact(~ lag1_roll_strain:lag1_diet_carb_g_kg) %>%
  step_normalize(all_numeric(),-all_outcomes()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) 


group_wfs <- 
  workflow_set(
    preproc = list(basic_lasso = group_basic_rec, 
                   interact_lasso = group_interact_rec,
                   basic_ridge = group_basic_rec, 
                   basic_xgb = group_basic_rec_xgb, 
                   basic_lightgbm = group_basic_rec_xgb,
                   basic_svm = group_basic_rec,
                   basic_knn = group_basic_rec_xgb,
                   basic_mars = group_basic_rec,
                   basic_nnet = group_basic_rec
                   
    ),  
    models = list(lasso_linear_reg_glmnet_spec, 
                  lasso_linear_reg_glmnet_spec,
                  ridge_linear_reg_glmnet_spec,   
                  boost_tree_xgboost_spec, 
                  lightgbm_model,
                  svm_poly_kernlab_spec,
                  nearest_neighbor_kknn_spec,
                  mars_earth_spec,
                  nnet_spec
    ),
    cross = F  )


# tune --------------------------------------------------------------------

library(finetune)

doParallel::registerDoParallel()

group_wfs_tune <- 
  group_wfs %>%  
  workflow_map(
    seed = 67, 
    fn = "tune_race_anova",
    grid = 25, 
    resamples = group_folds,
    verbose = T,
    control = control_race(save_pred = F,
                           parallel_over = "everything",
                           save_workflow = TRUE,
                           verbose_elim = TRUE)
  )



group_wfs_tune 

doParallel::stopImplicitCluster()


# select best -------------------------------------------------------------

autoplot(
  group_wfs_tune,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) 

rank_results(group_wfs_tune, select_best = T)


best_results <- group_wfs_tune %>% 
  extract_workflow_set_result("basic_lasso_linear_reg") %>% 
  select_best(metric = "rmse")

final_fit <- 
  group_wfs_tune %>% 
  extract_workflow("basic_lasso_linear_reg") %>% 
  finalize_workflow(best_results) %>% 
  fit(group_markov_tbl_subset)

final_resamps_fit <- 
  group_wfs_tune %>% 
  extract_workflow("basic_lasso_linear_reg") %>% 
  finalize_workflow(best_results) %>% 
  fit_resamples(group_folds, control = control_resamples(save_pred = T))


cv_metrics <- collect_metrics(final_resamps_fit)
cv_preds <- collect_predictions(final_resamps_fit, summarize = T) %>% 
  select(pred = .pred) %>% 
  bind_cols(group_markov_tbl_subset)

boot_mets <- final_resamps_fit %>% 
  collect_metrics(summarize = FALSE) %>% 
  filter(.metric == "rmse")

boot_cis <- confintr::ci_mean(boot_mets$.estimate)
boot_cis$interval
mean(boot_mets$.estimate)

boot_mets_rsq <- final_resamps_fit %>% 
  collect_metrics(summarize = FALSE) %>% 
  filter(.metric == "rsq")

boot_cis_rsq <- confintr::ci_mean(boot_mets_rsq$.estimate)
boot_cis_rsq$interval
mean(boot_mets_rsq$.estimate)

group_rmse <- cv_metrics %>% 
  filter(.metric == "rmse") %>% pull(mean) %>% round(.,1)

group_rsq <- cv_metrics %>% 
  filter(.metric == "rsq") %>% pull(mean) %>% round(.,2)


cv_preds %>% 
  ggplot(aes(x = pred, y = AM_zq_score)) +
  geom_abline(col = "red") +
  geom_point(alpha = .1) +
  ggpubr::stat_cor(aes(label = paste(..r.label..)), cor.coef.name = 'r') +
  coord_obs_pred() +
  theme(axis.title = element_text(size = 10))


