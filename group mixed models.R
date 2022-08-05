library(tidyverse)
library(tidymodels)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("annotate", "ggplot2")
conflict_prefer("lag", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("map", "purrr")
tidymodels_prefer()

library(multilevelmod)


variables <- "top_5_from_stack"


fit_and_metrics_lmer_fn <- function(data){
  
  group_lmer_iv_vars <- colnames(group_lmer_tbl  %>% select(-subject_id, - all_of(dependent_variable)))
  
  dv <- paste0(dependent_variable, " ~ ")
  iv <-  paste(group_lmer_iv_vars, collapse=" + ")
  
  fixed.part <- paste(dv,iv )
  random.part <- " + (1|subject_id)"
  
  
  set.seed(3332)
  group_split <- group_initial_split(data, group = subject_id)
  group_train <- training(group_split)
  group_test <- testing(group_split)
  
  
  mixed_model_spec <- linear_reg() %>% set_engine("lmer")
  
  mixed_model_wf <- workflow() %>%
    add_model(mixed_model_spec, formula = formula(paste(fixed.part, random.part))) %>%
    add_variables(outcomes = all_of(dependent_variable), predictors = colnames(group_lmer_tbl %>% dplyr::select(- all_of(dependent_variable))))
  
  fit2 <- fit(mixed_model_wf, group_train)
  fit_extract <- extract_fit_parsnip(fit2) 
  
  train_rmse <- performance::model_performance(fit_extract) %>% as_tibble() %>% pull(RMSE) %>%  round(.,2)
  train_rsq <- performance::model_performance(fit_extract) %>% as_tibble() %>% pull(R2_marginal) %>%  round(.,2)
  
  test_preds <- predict(fit2, group_test) %>% 
    bind_cols(group_test) %>% 
    select(all_of(dependent_variable), .pred)
  
  reg_metrics <- metric_set(rmse, rsq)
  
  test_rmse <- test_preds %>% reg_metrics(test_preds[[dependent_variable]], .pred) %>% 
    filter(.metric == "rmse") %>% pull(.estimate) %>% round(.,2)
  
  test_rsq <- test_preds %>% reg_metrics(test_preds[[dependent_variable]], .pred) %>% 
    filter(.metric == "rsq") %>% pull(.estimate) %>% round(.,2)
  
  
  lmer_result_tbl <- tibble(model = "lmer",
                            dv = dependent_variable,
                            vars = variables,
                            test_rmse = test_rmse,
                            test_rsq = test_rsq, 
                            train_rmse = train_rmse,
                            train_rsq = train_rsq) %>% 
    mutate(
      across(model:vars, ~as.factor(.))
    )
  
  lmer_result_tbl
}



#
# zq  ----------------------------------------------------------------------
## AM zq score refers to the AM PRS variable

dependent_variable <- "AM_zq_score"   #   AM_zq_score    hrv_chng  exercise_TF

group_tbl <- read_rds("synthed_data.rds")
group_lmer_vars <- c("lag1_AM_zq_score",  "AM_soreness" , "AM_stress" , "AM_sleep_qualilty" , "lag2_AM_zq_score", "subject_id", dependent_variable)
group_lmer_tbl <- group_tbl[,names(group_tbl) %in% group_lmer_vars] %>% drop_na()
group_lmer_tbl <- group_lmer_tbl %>% mutate(subject_id = factor(subject_id))

lmer_result_tbl <- fit_and_metrics_lmer_fn(group_lmer_tbl)

lmer_result_tbl

