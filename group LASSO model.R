library(tidyverse)
library(tidymodels)
library(finetune)
library(conflicted)
conflict_prefer("tune", "tune")
conflict_prefer("slice", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")
conflict_prefer("select", "dplyr")


pen_vals <- 10^seq(-3, 0, length.out = 50)

lasso_linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine('glmnet', path_values = pen_vals)

variables <- "main_vars" 
zq_version <- "normal" 
tf_version <- "normal" 

lasso_tune_fn <- function(wf){
  doParallel::registerDoParallel()
  
  lasso_tune <- 
    tune_race_anova(
      wf,
      grid = 25,
      resamples = group_cv_folds,
      control = control_race(save_pred = F,
                             parallel_over = "everything",
                             save_workflow = FALSE,
                             verbose_elim = TRUE)
    )
  
  lasso_wf 
  
  doParallel::stopImplicitCluster()
  
  ## Fit the final model & extract the workflow
  pen <- select_best(lasso_tune, "rmse") %>% pull(penalty)
  pen
  
  train_rmse <- collect_metrics(lasso_tune) %>% filter(.metric == "rmse") %>% arrange(mean) %>% slice(1) %>% pull(mean)
  train_rsq <- collect_metrics(lasso_tune) %>% filter(.metric == "rsq") %>% arrange(-mean) %>% slice(1) %>% pull(mean)
  
  
  # re-specify the model with the optimized values
  lasso_spec_tuned <-  linear_reg(penalty = pen, mixture = 1) %>%
    set_engine('glmnet')
  
  
  # re-set workflow
  lasso_wf_tuned <- workflow() %>%
    add_recipe(group_basic_rec) %>%
    add_model(lasso_spec_tuned)
  
  final_fit <- last_fit(lasso_wf_tuned, group_split)
  
  lasso_metrics <- final_fit %>% collect_metrics()
  
  test_rmse <- lasso_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
  test_rsq <- lasso_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
  
  
  lasso_result_tbl <- tibble(model = "lasso non-normed subject split",
                             dv = dependent_variable,
                             vars = variables,
                             test_rmse = test_rmse,
                             test_rsq = test_rsq, 
                             train_rmse = train_rmse,
                             train_rsq = train_rsq) %>% 
    mutate(
      across(model:vars, ~as.factor(.))
    )
  
  lasso_result_tbl
}


# #
# zq ----------------------------------------------------------------------
## AM zq score refers to the AM PRS variable

dependent_variable <- "AM_zq_score"   #   AM_zq_score    hrv_chng  exercise_TF

group_markov_tbl_subset <- read_rds("synthed_data.rds")

# split 2 ---------------------------------------------------------------
set.seed(3332)
group_split <- group_initial_split(group_markov_tbl_subset, group = subject_id)
group_train <- training(group_split)
group_test <- testing(group_split)

set.seed(3458)
group_cv_folds <- group_vfold_cv(
  data = group_train, 
  group = subject_id,
  v = 10, strata = all_of(dependent_variable),
  repeats = 5
) 

# *recipe ------------------------------------------------------------------

group_basic_rec <-   
  recipe(AM_zq_score ~ .,
         data = group_train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) 


lasso_wf <- workflow() %>%
  add_recipe(group_basic_rec) %>%
  add_model(lasso_linear_reg_glmnet_spec)


lasso_result_tbl <- lasso_tune_fn(lasso_wf)
lasso_result_tbl
