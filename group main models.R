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
dependent_variable <- "AM_zq_score"   #   AM_zq_score    hrv_chng  exercise_TF
variables <- "main_vars"  #    main_vars     actionable_vars     

zq_version <- "normal" # alternatively could have used box-cox transformations
tf_version <- "normal" 

# read data ---------------------------------------------------------------
group_tbl <- read_rds("synthed_data.rds")

#select vars
group_markov_tbl_subset <- nzv_fct_var_remove_fn(group_tbl)

#
# split 1 ---------------------------------------------------------------
set.seed(3332)
group_split <- group_initial_split(group_markov_tbl_subset, group = subject_id)
group_train <- training(group_split)
group_test <- testing(group_split)

# remove cor'd ------------------------------------------------------------

only_numeric <- group_train %>% select(where(is.numeric)) %>% 
  select(-matches("(^AM_zq)|(study_day)|(subject)|(lambda)")) #|(^exercise_TF)|(^hrv_chng)|(^hrv$)

cor_mat <- cor(only_numeric, use = "pairwise.complete.obs")
full_cor_to_remove <- caret::findCorrelation(cor_mat, 0.85, names = T, exact = T) %>% 
  str_c(., "$", collapse = ")|(") %>% 
  str_c("(", ., ")")



# subset ------------------------------------------------------------------
group_markov_tbl_subset <- group_markov_tbl_subset %>%  select(-matches(full_cor_to_remove), -study_day)  

#
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
         data = group_train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) 


group_basic_juiced <- group_basic_rec  %>% prep() %>% juice() 


group_basic_rec_xgb <-   
  recipe(AM_zq_score ~ .,
         data = group_train) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = T) 


group_basic_rec_xgb_juiced <- group_basic_rec_xgb  %>% prep() %>%  juice() 


group_interact_rec <-   
  recipe(AM_zq_score ~ .,
         data = group_train) %>%
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
    resamples = group_cv_folds,
    verbose = T,
    control = control_race(save_pred = TRUE,
                           parallel_over = "everything",
                           save_workflow = TRUE,
                           verbose_elim = TRUE)
  )



group_wfs_tune 

doParallel::stopImplicitCluster()


# stack -------------------------------------------------------------------
library(stacks)
tidymodels_prefer()


group_stack <- 
  stacks() %>% 
  add_candidates(group_wfs_tune)


set.seed(2001)
group_ens <- blend_predictions(group_stack)
group_ens

group_ens_fit <- fit_members(group_ens)



group_ens1_wt <- stacks:::top_coefs(group_ens) %>% slice(1) %>% pull(weight)
group_ens2_wt <- stacks:::top_coefs(group_ens) %>% slice(2) %>% pull(weight)
group_ens3_wt <- stacks:::top_coefs(group_ens) %>% slice(3) %>% pull(weight)
group_ens4_wt <- stacks:::top_coefs(group_ens) %>% slice(4) %>% pull(weight)
group_ens5_wt <- stacks:::top_coefs(group_ens) %>% slice(5) %>% pull(weight)
group_ens6_wt <- stacks:::top_coefs(group_ens) %>% slice(6) %>% pull(weight)
group_ens7_wt <- stacks:::top_coefs(group_ens) %>% slice(7) %>% pull(weight)
group_ens8_wt <- stacks:::top_coefs(group_ens) %>% slice(8) %>% pull(weight)
group_ens9_wt <- stacks:::top_coefs(group_ens) %>% slice(9) %>% pull(weight)
group_ens10_wt <- stacks:::top_coefs(group_ens) %>% slice(10) %>% pull(weight)


group_ens_test_pred <- 
  predict(group_ens_fit, group_test) %>% 
  bind_cols(group_test) %>% 
  select(.pred, .data[[dependent_variable]]) %>% 
  mutate(set = "Test set")


reg_metrics <- metric_set(rmse, rsq)

group_stack_rmse <- group_ens_test_pred %>% reg_metrics(group_ens_test_pred[[dependent_variable]], .pred) %>% 
  filter(.metric == "rmse") %>% pull(.estimate) %>% round(.,1)

group_stack_rsq <- group_ens_test_pred %>% reg_metrics(group_ens_test_pred[[dependent_variable]], .pred) %>% 
  filter(.metric == "rsq") %>% pull(.estimate) %>% round(.,2)


# pred plot ---------------------------------------------------------------

group_ens_train_pred <- 
  predict(group_ens_fit, group_train) %>% 
  bind_cols(group_train) %>% 
  select(.pred, .data[[dependent_variable]]) %>% 
  mutate(set = "Training set")

group_ens_pred_combined <-  group_ens_test_pred %>% 
  bind_rows(group_ens_train_pred)

max_dv <- max(group_ens_test_pred[[dependent_variable]])


group_stack_pred_plot <- group_ens_test_pred %>%
  ggplot(aes(x = .data[[dependent_variable]], y = .pred, color = set)) +
  geom_point(alpha = 0.0001)+
  geom_abline(col = "grey") +
  geom_point(data =  group_ens_pred_combined %>% filter(set == "Test set") , aes(), color = "black", alpha = .4, shape = 1, size = 1.5) +
  geom_point(data =  group_ens_pred_combined %>% filter(set == "Test set") , aes(), color = "#1d1d80", alpha = .25, size = 1) +
  annotate("text", x = max_dv*.8, y = max_dv*.25, label = paste0("RMSE = ", group_stack_rmse), size = 2.5)+
  annotate("text", x = max_dv*.8, y = max_dv*.15, label = paste0("Rsq = ", group_stack_rsq), size = 2.5)+
  coord_obs_pred() +
  labs(x = paste0("Actual AM PRS score"), y = "Predicted", color = NULL) +
  theme(axis.title = element_text(size = 10),
        legend.position = "none", 
  )

group_stack_pred_plot



# dalex -------------------------------------------------------------------

library(DALEXtra)

explainer_group <- 
  explain_tidymodels(
    group_ens_fit, 
    data = dplyr::select(group_train, - dependent_variable),
    y = group_train[[dependent_variable]],
    label = "Ens",
    verbose = T  )

set.seed(1980)
ens_var_imp <- 
  model_parts(
    explainer_group
  )

ens_var_imp 


ens_imp_plot <- ggplot_importance(ens_var_imp, "zq")
ens_imp_plot

pdp_predictor_names <- ens_var_imp %>%
  filter(variable != "_baseline_") %>%
  filter(variable != "_full_model_",
         variable != "subject_diet_app",
         variable != "subject_id") %>%
  group_by(variable) %>%
  summarise(dropout_loss = mean(dropout_loss)) %>%
  arrange(-dropout_loss)  %>%
  slice_head(n = 9) %>%
  pull(variable)


p1pdp <- ggplot_pdp_no_color(pdp_profile, pdp_predictor_names)+
  geom_line(color = "#1d1d80", size = 1.2, alpha = 0.8) +   
  labs(x = NULL, y =  "AM PRS score")+  
  theme(
    strip.background = element_rect(fill = "grey40", color = "grey80", size = 1),
    strip.text = element_text(colour = "white"))

p1pdp



