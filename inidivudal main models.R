library(tidyverse)
library(timetk)
library(lubridate)
library(tidymodels)
library(bonsai)
library(conflicted)
conflict_prefer("tune", "tune")
conflict_prefer("slice", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("map", "purrr")
conflict_prefer("select", "dplyr")

source("helper functions.R")

subject <-   "529"  # select single subject
zq_version <- "normal" # alternatively could have used box-cox transformations
tf_version <- "normal" 

## AM zq score refers to the AM PRS variable
dependent_variable <- "AM_zq_score"   #   AM_zq_score    hrv_chng  exercise_TF
variables <- "main_vars"  #    main_vars     actionable_vars     


# read data ---------------------------------------------------------------

subject_tbl <- read_rds("synthed_data.rds") %>% 
  filter(subject_id == subject)

#select vars
subject_tbl_subset <- nzv_fct_var_remove_fn(subject_tbl)


# remove highly correlated variables ------------------------------------------------------------

only_numeric <- subject_tbl_subset %>% 
  select(where(is.numeric)) %>% 
  select(-matches("(^AM_zq)|(study_day)|(subject)")) #|(^exercise_TF)|(^hrv_chng)

cor_mat <- cor(only_numeric, use = "pairwise.complete.obs")
full_cor_to_remove <- caret::findCorrelation(cor_mat, 0.85, names = T, exact = T) %>% 
  str_c(., "$", collapse = ")|(") %>% 
  str_c("(", ., ")")



# subset ------------------------------------------------------------------

subject_tbl_subset <- subject_tbl_subset %>% select(-matches(full_cor_to_remove),  -study_day)  %>% drop_na()

if (variables == "actionable_vars") {subject_tbl_subset <- subject_tbl_subset %>% select(-c(contains("exercise_TF"), contains("hrv"), matches("lag.*AM_zq")))}


# splits ---------------------------------------------------------------

set.seed(3457)

  individ_cv_folds <- vfold_cv(
    data = subject_tbl_subset, 
    v = 10, strata = all_of(dependent_variable),
    repeats = 10  
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
  boost_tree(tree_depth = tune(), trees = 1000, learn_rate = tune(), loss_reduction = tune(), min_n = tune()) %>%
  set_engine('xgboost', importance = TRUE) %>%
  set_mode('regression')

lightgbm_model <-
  parsnip::boost_tree(
    mode = "regression", trees = 1000, min_n = tune(),  tree_depth = tune()) %>%
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
  mars(prod_degree = tune()) %>%
  set_engine('earth') %>%
  set_mode('regression')

# recipe ------------------------------------------------------------------
individ_basic_rec <-
    recipe(AM_zq_score ~ .,
           data = subject_tbl_subset) %>%
    step_normalize(all_numeric(),-all_outcomes()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_nzv(all_numeric_predictors())
  

individ_basic_rec  %>% prep() %>% juice() %>% glimpse()
individ_basic_juiced <- individ_basic_rec  %>% prep() %>% juice() 


 individ_basic_rec_xgb <-   
    recipe(AM_zq_score ~ .,
           data = subject_tbl_subset) %>%
    step_normalize(all_numeric(), -all_outcomes()) %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = T) %>% 
    step_nzv(all_numeric_predictors())
  
individ_basic_rec_xgb  %>% prep() %>% juice() %>% glimpse()
individ_basic_rec_xgb_juiced <- individ_basic_rec_xgb %>% prep() %>% juice() 


individ_interact_rec <-   
    recipe(AM_zq_score ~ .,
           data = subject_tbl_subset) %>%
    step_interact(~ lag1_roll_7d_exercise_load:lag1_roll_3d_diet_carb_g_kg) %>%
    step_interact(~ lag1_roll_monotony:roll_3d_diet_carb_g_kg) %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_nzv(all_numeric_predictors())
  
individ_interact_rec %>% prep() %>% juice() %>% glimpse()


individ_wfs <- 
    workflow_set(
      preproc = list(basic_lasso = individ_basic_rec, basic_ridge = individ_basic_rec, basic_xgb = individ_basic_rec_xgb, 
                     basic_lightgbm = individ_basic_rec_xgb, interact = individ_interact_rec,  
                     svm = individ_basic_rec, knn = individ_basic_rec_xgb, mars = individ_basic_rec
      ),  
      models = list(lasso_linear_reg_glmnet_spec, ridge_linear_reg_glmnet_spec, boost_tree_xgboost_spec, 
                    lightgbm_model, lasso_linear_reg_glmnet_spec, svm_poly_kernlab_spec, nearest_neighbor_kknn_spec, mars_earth_spec),
      cross = F  )


individ_wfs


# tune --------------------------------------------------------------------
library(finetune)

doParallel::registerDoParallel()
individ_wfs_tune <- 
  individ_wfs %>%  
  workflow_map(
    seed = 67, 
    fn = "tune_race_anova",
    grid = 25,
    resamples = individ_cv_folds,
    verbose = T,
    control = control_race(save_pred = F,
                           parallel_over = "everything",
                           save_workflow = TRUE,
                           verbose_elim = TRUE)
  )


## get the ID for the best fit model

individ_best_id <- individ_wfs_tune %>%  
  rank_results(
    rank_metric = "rmse",
    select_best = TRUE
  ) %>% 
  dplyr::slice(1) %>% 
  pull(wflow_id)

## Get the best workflowset
individ_wf_best <- extract_workflow(individ_wfs_tune, id = individ_best_id)

## extract the tuned results from the best workflow
individ_wf_best_tuned <- individ_wfs_tune[individ_wfs_tune$wflow_id == individ_best_id,
                                          "result"][[1]][[1]]

wf_best_final <- finalize_workflow(individ_wf_best, select_best(individ_wf_best_tuned, "rmse"))

# resamples ----------------------------------------------------------------
validation_boots <-  bootstraps(subject_tbl_subset, times = 500)

resampd <- fit_resamples(
  wf_best_final,
  validation_boots,
  control = control_resamples(save_pred = TRUE)
)

doParallel::stopImplicitCluster()


resamp_metrics <- resampd %>% collect_metrics()


resamp_metrics


# predictions -------------------------------------------------------------------

individ_test_pred <- 
    collect_predictions(resampd, summarize = T) 


individ_rmse <- resamp_metrics %>% 
  filter(.metric == "rmse") %>% pull(mean) %>% round(.,1)

individ_rsq <- resamp_metrics %>% 
  filter(.metric == "rsq") %>% pull(mean) %>% round(.,2)

max_dv <- max(individ_test_pred[[dependent_variable]])

individ_pred_plot <- individ_test_pred %>%
  ggplot(aes(x = .data[[dependent_variable]], y = .pred)) +
  geom_abline(col = "grey") +
  geom_point(alpha = .5, color = "limegreen") +
  annotate("text", x = max_dv*.85, y = max_dv*.47, label = paste0("RMSE = ", individ_rmse), size = 2.5)+
  annotate("text", x = max_dv*.85, y = max_dv*.4, label = paste0("Rsq = ", individ_rsq), size = 2.5)+
  coord_obs_pred() +
  labs(x = paste0("Actual AM PRS score"), y = "Predicted") +
  theme(axis.title = element_text(size = 10))

individ_pred_plot



# VIP  ---------------------------------------------------------------

## fit the final model
individ_bestfinal_1 <- wf_best_final %>% fit(subject_tbl_subset)

individ_lowest_rmse <- individ_wf_best_tuned %>%
  show_best("rmse") %>% 
  dplyr::slice(1)

individ_best_id

### use this for lasso/ridge
individ_vi_tbl <- individ_bestfinal_1 %>%
  extract_fit_parsnip() %>%
  vip::vi(lambda = individ_lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = factor(Variable), .keep = "unused") %>% 
  mutate(
    Sign = ifelse(Importance == 0, "Neutral", Sign)
  )

### use this for XGBoost
individ_vi_tbl <- individ_bestfinal_1 %>%
  extract_fit_parsnip() %>%
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    Variable = factor(Variable), .keep = "unused") 


#### use this for lightgbm models
myModel1<-extract_fit_engine(individ_bestfinal_1)
impObj1 <- lgb.importance(myModel1, percentage =FALSE)

individ_vi_tbl <- impObj1 %>% as_tibble() %>%
  mutate(
    Importance = Gain,
    Variable = factor(Feature), .keep = "unused") %>% 
  dplyr::select(Variable, Importance)

########use this for svm models
individ_vi_tbl <- individ_bestfinal_1 %>%
    extract_fit_parsnip() %>%
    vip::vi(method = "permute", 
            target = "AM_zq_score", metric = "rmse",
            pred_wrapper = kernlab::predict, train = individ_basic_juiced)%>%
    mutate(
      Importance = abs(Importance),
      Variable = factor(Variable), .keep = "unused")


########use this for /knn models
individ_vi_tbl <- individ_bestfinal_1 %>%
    extract_fit_parsnip() %>%
    vip::vi(method = "permute", 
            target = "AM_zq_score", metric = "rmse",
            pred_wrapper = kernlab::predict, train = individ_basic_rec_xgb_juiced)%>%
    mutate(
      Importance = abs(Importance),
      Variable = factor(Variable), .keep = "unused")



# join tbls ---------------------------------------------------------------

individ_vi_joined <- individ_vi_tbl %>% 
  mutate(across(2:ncol(.), ~ifelse(is.na(.), 0, .)),
         total_importance = Importance/ sum(Importance),
  )  

individ_vi_joined 


# bar plot vip ------------------------------------------------------------
if (str_detect(individ_best_id, "linear")){
  individ_vip_bar_plot <- 
    individ_vi_joined %>% 
    arrange(desc(total_importance)) %>%
    filter(!grepl('wday|Other', Variable)) %>% 
    slice_head(n = 10) %>% 
    filter(abs(total_importance)>0) %>% 
    ggplot(aes(total_importance, fct_reorder(Variable, total_importance), fill = Sign))+ #
    geom_col(color = "black")+
    scale_x_continuous(labels = scales::percent_format())+
    labs(y = NULL, x = paste0("Variable Importance\n", "AM PRS score"))+
    theme(legend.position = "none")
} else {
  individ_vip_bar_plot <- 
    individ_vi_joined %>% 
    arrange(desc(total_importance)) %>%
    filter(!grepl('wday|Other', Variable)) %>% 
    slice_head(n = 10) %>% 
    filter(abs(total_importance)>0) %>% 
    ggplot(aes(total_importance, fct_reorder(Variable, total_importance)))+ #, fill = Sign
    geom_col(color = "black")+
    scale_x_continuous(labels = scales::percent_format())+
    labs(y = NULL, x = paste0("Variable Importance\n", "AM PRS score"))+
    theme(legend.position = "none")
}


individ_vip_bar_plot


# explainer ---------------------------------------------------------------
library(DALEXtra)

explainer_individ <- 
    explain_tidymodels(
      individ_bestfinal_1, 
      data = dplyr::select(subject_tbl_subset, - dependent_variable),
      y = subject_tbl_subset[[dependent_variable]],
      label = "Ens",
      verbose = T  )



#find top 6 variables

pdp_predictor_names <- individ_vi_joined %>% arrange(desc(total_importance)) %>%
  filter(!grepl('_X1|wday|mode', Variable)) %>% 
  slice_head(n = 9) %>%
  filter(abs(total_importance)>0) %>% 
  pull(Variable) %>% as.vector(.)


pdp_profile <- model_profile(
  explainer_individ,
  variables = pdp_predictor_names,
  N = NULL,
  center = T)

# pdp ---------------------------------------------------------------------

p1pdp <- ggplot_pdp(pdp_profile, pdp_predictor_names)+
    labs(x = NULL, y = paste0("AM PRS score") )+  #  , color = "Workouts\n  per day"
    theme(
      strip.background = element_rect(fill = "grey40", color = "grey80", size = 1),
      strip.text = element_text(colour = "white"))


p1pdp

