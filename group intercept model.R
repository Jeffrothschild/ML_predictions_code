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
tf_version <- "normal" 

#
# zq ----------------------------------------------------------------------
## AM zq score refers to the AM PRS variable

dependent_variable <- "AM_zq_score"

group_markov_tbl_subset <- read_rds("synthed_data.rds")

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

