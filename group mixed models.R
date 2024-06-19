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


variables <- "top_5_from_main"

fit_and_metrics_lmer_fn <- function(data){
  
  group_lmer_iv_vars <- colnames(group_lmer_tbl  %>% select(-subject_id, - all_of(dependent_variable)))
  
  dv <- paste0(dependent_variable, " ~ ")
  iv <-  paste(group_lmer_iv_vars, collapse=" + ")
  
  fixed.part <- paste(dv,iv )
  random.part <- " + (1|subject_id)"
  
  n_subs <- length(unique(data$subject_id))
  
  group_folds <- group_vfold_cv(data, group = subject_id, v = n_subs)
  
  
  
  mixed_model_spec <- linear_reg() %>% set_engine("lmer")
  
  mixed_model_wf <- workflow() %>%
    add_model(mixed_model_spec, formula = formula(paste(fixed.part, random.part))) %>%
    add_variables(outcomes = all_of(dependent_variable), predictors = colnames(group_lmer_tbl %>% dplyr::select(- all_of(dependent_variable))))
  
  fit2 <- fit_resamples(mixed_model_wf, group_folds, control = control_resamples(save_pred = TRUE))
  fit_metrics <- collect_metrics(fit2)
  fit_preds <- collect_predictions(fit2, summarize = T)
  
  boot_mets <- fit2 %>% 
    collect_metrics(summarize = FALSE) %>% 
    filter(.metric == "rmse")
  
  boot_cis <- confintr::ci_mean(boot_mets$.estimate)
  
  boot_mets_rsq <- fit2 %>% 
    collect_metrics(summarize = FALSE) %>% 
    filter(.metric == "rsq")
  
  boot_cis_rsq <- confintr::ci_mean(boot_mets_rsq$.estimate)
  
  
  test_rmse <- fit_metrics %>% 
    filter(.metric == "rmse") %>% pull(mean) %>% round(.,2)
  
  test_rsq <- fit_metrics %>% 
    filter(.metric == "rsq") %>% pull(mean) %>% round(.,2)
  
  
  
  
  lmer_result_tbl <- tibble(model = "lmer",
                            dv = dependent_variable,
                            vars = variables,
                            test_rmse = test_rmse,
                            test_rsq = test_rsq, 
                            rmse_ci_low = round(boot_cis$interval[1],2),
                            rmse_ci_high = round(boot_cis$interval[2],2),
                            rsq_ci_low = round(boot_cis_rsq$interval[1],2),
                            rsq_ci_high = round(boot_cis_rsq$interval[2],2),
  ) %>% 
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

