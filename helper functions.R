
nzv_fct_var_remove_fn <- function(df){
  
  df <- df %>% 
    mutate(
      subject_id = factor(subject_id),
      subject_sleep_app = ifelse(str_detect(subject_sleep_app, "Garmin"), "Garmin", subject_sleep_app),
    )  %>% 
    mutate(
      across(c(where(is.numeric)),  ~ifelse(. == "NaN", NA, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "Inf", NA, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "-Inf", NA, .)),
      across(c(subject_diet_app, subject_id:subject_level, subject_primary_sport:subject_sleep_app, subject_hrv_app) , ~as.factor(.))
    )
  
  #transformation options
  if (zq_version == "normal") {df <- df %>% select(-contains("score_boxcox"))}
  if (zq_version == "boxcox") {df <- df %>% select(-matches("(zq_score$)"))}
  
  if (tf_version == "normal") {df <- df %>% select(-contains("TF_boxcox"))}
  if (tf_version == "boxcox") {df <- df %>% select(-matches("(exercise_TF$)"))}
  
  #variable names
  if (dependent_variable == "exercise_TF"){ dfz1 <- df %>% filter(!is.na(exercise_CARB_before)) }
  if (dependent_variable == "exercise_TF"){ dfz <- dfz1[,!names(dfz1) %in% TF_post_measurement_vars] }
  
  
  
  if (dependent_variable == "AM_zq_score"){ dfz <- df[,!names(df) %in% post_measurement_vars] }
  if (dependent_variable == "hrv_chng"){
    dfz <- df[,!names(df) %in% post_measurement_vars]
    dfz<- dfz %>% select(-c(hrv, roll_7d_avg_hrv_delta, roll_7d_hrv_MA)) }
  
  
  if (variables == "main_vars")  { dfx <- dfz %>% select(contains(main_var_names)) %>% select(-c(matches("(hrv_low)"))) }  
  if (variables == "actionable_vars") {dfx <- dfz %>% select(contains(actionable_var_names)) %>% select(-c(matches("(hrv_low)"))) }  
  
  factors_to_remove <- dfx %>% 
    select(where(is.factor)) %>%
    mutate_all(list(fact_len = ~ ifelse(length(unique(.)) == 1, 0, 1))) %>%
    select(contains("_fact_len")) %>%
    summarise_all( ~ sum(.)) %>%
    pivot_longer(1:ncol(.)) %>%
    filter(value == 0) %>%
    pivot_wider(.) %>%
    colnames(.) %>%
    str_remove_all(., "_fact_len")
  
  dfx[,!names(dfx) %in% factors_to_remove] 
  
  
}



main_var_names <- c("AM_sleep_qualilty" ,      "AM_soreness"             ,"AM_stress"            ,   "AM_zq_score"         ,    "date"             ,      
                    "diet_carb_g"       ,      "diet_carb_g_kg"          ,"diet_fat_g"         ,     "diet_fat_g_kg"     ,      "diet_kcal"        ,      
                    "diet_kcal_kg"      ,      "diet_protein_g"       ,   "diet_protein_g_kg" ,      "exercise_CARB_before",    "exercise_duration_min",  
                    "exercise_fasted"    ,     "exercise_load"        ,   "exercise_mode_Bike"  ,    "exercise_mode_Other",     "exercise_mode_Run"   ,   
                    "exercise_mode_Strength",  "exercise_mode_Swim"    ,  "exercise_TF"          ,   "exercise_tss"        ,    "exercise_wrkts_per_day", 
                    "roll_3d_diet_carb_g_kg",  "roll_7d_diet_carb_g_kg",  "roll_7d_diet_kcal_kg" ,   "roll_7d_sleep_hours",     "roll_7d_sleep_index" ,   "roll_3d_diet_fat_g_kg",
                    "roll_7d_diet_fat_g_kg",  "roll_7d_exercise_load" ,   "roll_monotony"    ,       "roll_strain"          ,   "sleep_hours"        ,     "sleep_index"         ,   
                    "study_day"          ,     "wday.lbl"          ,      "weight_kg"           ,    "delta_pulse_chng"   ,     "pulse"               ,   
                    "roll_7d_avg_pulse_delta", "hrv"              ,       "hrv_chng"   ,    "roll_7d_avg_hrv_delta",   "roll_7d_hrv_MA"      ,   
                    "subject_age"         ,    "subject_diet_app" ,          "subject_id"       ,       "subject_level"       ,   
                    "subject_primary_sport",   "subject_sex"      ,       "subject_sleep_app"   ,    "subject_training_age",    "subject_hrv_app"      ,  
                    "previous_training_days" ,    "subject_missing_data", "roll_3d_diet_kcal_kg", "roll_3d_diet_protein_g_kg", "roll_7d_diet_protein_g_kg", 
                    "roll_carb_monotony", "roll_7d_carb_g_sd", "exercise_RPE_weighted", "exercise_RPE_max",  "roll_7d_exercise_load_EWMA", "roll_7d_exercise_load",
                    "roll_7d_exercise_load_max", "roll_7d_exercise_rpe_max", "AM_zq_score_boxcox", "exercise_TF_boxcox") %>% sort(.)   # "roll_7d_tss_EWMA", "roll_7d_tss",

actionable_var_names <- c( "AM_soreness"             ,"AM_stress"            ,      "date"             ,      
                           "diet_carb_g"       ,      "diet_carb_g_kg"          ,"diet_fat_g"         ,     "diet_fat_g_kg"     ,      "diet_kcal"        ,      
                           "diet_kcal_kg"      ,      "diet_protein_g"       ,   "diet_protein_g_kg" ,      "exercise_CARB_before",    "exercise_duration_min",  
                           "exercise_fasted"    ,     "exercise_load"        ,   "exercise_mode_Bike"  ,    "exercise_mode_Other",     "exercise_mode_Run"   ,   
                           "exercise_mode_Strength",  "exercise_mode_Swim"    ,  "exercise_TF"          ,   "exercise_tss"        ,    "exercise_wrkts_per_day", 
                           "roll_3d_diet_carb_g_kg",  "roll_7d_diet_carb_g_kg",  "roll_7d_diet_kcal_kg" ,   "roll_7d_sleep_hours",      "roll_3d_diet_fat_g_kg",
                           "roll_7d_diet_fat_g_kg",  "roll_7d_exercise_load" ,   "roll_monotony"    ,       "roll_strain"          ,   "sleep_hours"        ,     
                           "study_day"          ,    "exercise_RPE_weighted", "exercise_RPE_max" ,  "AM_zq_score", "hrv_chng",
                           "subject_id"       ,          "roll_carb_monotony", "roll_7d_carb_g_sd"    ,   
                           "subject_primary_sport",   "subject_sex"      ,       "subject_sleep_app"   ,    "subject_training_age",    "subject_hrv_app"      ,  
                           "previous_training_days" ,    "subject_missing_data", "roll_3d_diet_kcal_kg", "roll_3d_diet_protein_g_kg", "roll_7d_diet_protein_g_kg",
                           "roll_7d_exercise_load_EWMA", "roll_7d_exercise_load", "roll_7d_exercise_rpe_max", "roll_7d_exercise_load_max") %>% sort(.)   #  "roll_7d_tss_EWMA", 



post_measurement_vars <- c("diet_carb_g", "diet_carb_g_kg", "diet_fat_g", "diet_fat_g_kg", "diet_kcal", "diet_kcal_kg",
                           "diet_protein_g", "diet_protein_g_kg", "exercise_CARB_before", "exercise_duration_min",
                           "exercise_load", "exercise_TF", "exercise_tss", "exercise_wrkts_per_day", "exercise_fasted",
                           "exercise_mode_Bike", "exercise_mode_Other", "exercise_mode_Run", "exercise_mode_Strength",
                           "exercise_RPE_max", "exercise_RPE_weighted", "exercise_TF_boxcox",
                           "exercise_mode_Swim", "roll_7d_tss_EWMA", "roll_7d_exercise_load_EWMA", #"roll_7d_exercise_rpe_max", 
                           "roll_7d_exercise_load_max", "roll_7d_exercise_load")

TF_post_measurement_vars <- c("diet_carb_g", "diet_carb_g_kg", "diet_fat_g", "diet_fat_g_kg", "diet_kcal", "diet_kcal_kg",
                              "diet_protein_g", "diet_protein_g_kg")

ggplot_pdp <- function(obj, features) {
  
  cp_tib <- as_tibble(obj$cp_profiles) %>%    
    rename_with(~ str_remove_all(., '_'), `_yhat_`:`_label_`) %>% 
    select(all_of(features), yhat, vname, ids, label) %>% 
    pivot_longer(all_of(features))  %>% 
    filter(vname == name)
  
  mp_tib <- as_tibble(obj$agr_profiles) %>%  
    rename_all(.funs = ~str_remove_all(., '_'))  %>% 
    rename(value = x) %>% 
    mutate(label = str_remove_all(label, "GLM_"))
  
  pp <- 
    ggplot(mp_tib, aes(x = value, y = yhat, group = ids)) +
    geom_line(aes(group = label)) +
    geom_line(data = cp_tib, size = 0.5, alpha = 0.05, color = "gray50") +
    facet_wrap(~vname, scales = "free_x")  #
  
  
  num_colors <- n_distinct(obj$agr_profiles$`_label_`)
  
  if (num_colors > 1) {
    pp <- pp + geom_line(aes(group = label, color = label), size = 1.2, alpha = 0.8)
  } else {
    pp <- pp + geom_line(color = "#1d1d80", size = 1.2, alpha = 0.8)
  }
  
  pp
  
}


ggplot_importance <- function(parts, dv) {

  metric_lab <- paste( #metric_name, 
    "RMSE after permutations\n(higher indicates more important)")
  
  full_vip <- parts %>%
    filter(variable != "_baseline_")
  
  var_names <- full_vip %>% 
    filter(variable != "_full_model_",
           variable != "subject_diet_app") %>% 
    group_by(variable) %>% 
    summarise(dropout_loss = mean(dropout_loss)) %>% 
    arrange(-dropout_loss)  %>% 
    slice_head(n = 10) %>% 
    pull(variable)
  
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_",
           variable != "subject_diet_app") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <-   full_vip %>%
    filter(variable %in% var_names) %>%
    as_tibble() %>%
    rename("Variable" = variable) %>%
    ggplot(aes(dropout_loss, fct_reorder(factor(Variable), dropout_loss))) +
    geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
               size = 1.4, lty = 2, alpha = 0.7) +
    labs(x = metric_lab,
         y = NULL,  fill = NULL,  color = NULL)
  
  
  if(dv == "zq") { p1 <- p +  geom_boxplot(fill = "#6f6fde", alpha = 0.4)+
    labs(title = "AM PRS")}
  if(dv == "tf") { p1 <-  p +  geom_boxplot(fill = "#6fde6f", alpha = 0.4)+
    labs(title = "Exercise TF")}
  if(dv == "hrv") {p1 <-  p +  geom_boxplot(fill = "#de6f6f", alpha = 0.4)+
    labs(title = "HRV change")}
  
  p1 +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) 
}


ggplot_pdp_no_color <- function(obj, features) {
  
  cp_tib <- as_tibble(obj$cp_profiles) %>%    
    rename_with(~ str_remove_all(., '_'), `_yhat_`:`_label_`) %>% 
    select(all_of(features), yhat, vname, ids, label) %>% 
    pivot_longer(all_of(features))  %>% 
    filter(vname == name)
  
  mp_tib <- as_tibble(obj$agr_profiles) %>%  
    rename_all(.funs = ~str_remove_all(., '_'))  %>% 
    rename(value = x) %>% 
    mutate(label = str_remove_all(label, "GLM_"))
  
  pp <- 
    ggplot(mp_tib, aes(x = value, y = yhat, group = ids)) +
    geom_line(aes(group = label)) +
    geom_line(data = cp_tib, size = 0.5, alpha = 0.05, color = "gray50") +
    facet_wrap(~factor(vname,features), scales = "free_x", labeller = as_labeller(facet_names))  #
  
  
  num_colors <- n_distinct(obj$agr_profiles$`_label_`)
  
  if (num_colors > 1) {
    pp <- pp + geom_line(aes(group = label, color = label), size = 1.2, alpha = 0.8)
  } 
  
  pp
  
}



panel_hist_fn <- function(df){
  df %>% select(where(is.numeric)) %>%  gather() %>% 
    ggplot(aes(x=value)) + 
    geom_histogram(fill="steelblue", alpha=.7, bins = 8) +
    theme_minimal() +
    facet_wrap(~key, scales="free")
}



joined_imputed_fn <- function(x){
  updated <- x %>%
    mutate(
      weight_kg = timetk::ts_impute_vec(weight_kg),
      sleep_index = sleep_hours * sleep_qualilty,
      across(c(duration_min:wrkts_per_day, previous_training_days), ~ifelse(is.na(duration_min), 0, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "NaN", NA, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "Inf", NA, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "-Inf", NA, .)),
      
    )
  
  impute_1 <-   recipe(TF ~ ., data = updated) %>% 
    update_role(date, fasted, weight_kg, new_role = "id") %>% 
    step_impute_linear(contains("diet"),  impute_with = imp_vars(contains("diet"),  tss, duration_min, load)) %>% 
    step_impute_median(all_numeric_predictors(), -c(CARB_before, sleep_index, RPE_weighted, RPE_max)) %>%   
    prep() %>% juice() %>% 
    mutate(
      sleep_index = sleep_hours* sleep_qualilty
    )
  
  impute_2 <-   recipe(zq_score ~ ., data = impute_1) %>%
    update_role(date, fasted, weight_kg, new_role = "id") %>%
    step_impute_linear(TF,  impute_with = imp_vars(zq_score, soreness, stress, hrv)) %>%
    prep() %>% juice() %>% 
    mutate(
      TF = ifelse(RPE_max == "NA", NA, TF),
      across(fasted:mode_Other, ~as.character(.)),
      across(fasted:mode_Other, ~ifelse(duration_min == 0, "0", .)),
      across(fasted:mode_Other, ~as.factor(.)),
    )
  
  impute_2 
  
}


feat_eng_fn1 <- function(x){
  
  total_missing <- colSums(is.na(joined_tbl)) %>% as.data.frame() %>% rownames_to_column(.) %>% 
    rename(numb_missing = ".") %>% 
    filter(rowname != "TF", rowname != "CARB_before", rowname != "weight_kg", rowname != "exercise_rpe_max") %>% 
    summarise(total_NA = sum(numb_missing)) %>% 
    pull(total_NA)
  
  total_points <- ncol(joined_tbl)*nrow(joined_tbl)
  
  percent_missing_data <- round(total_missing/total_points * 100,1)
  
  diet_missing <- colSums(is.na(diet_daily_tbl)) %>% as.data.frame() %>% rownames_to_column(.) %>% 
    rename(diet_missing = ".") %>% 
    filter(rowname == "diet_kcal") %>% 
    pull(diet_missing)
  
  percent_missing_diet <- round(diet_missing/nrow(joined_tbl) * 100,1)
  
  
  x %>% 
    tk_augment_timeseries_signature() %>% 
    select(-c(index.num:wday.xts, mday:mday7)) %>% 
    mutate(
      #cleaning
      across(where(is.numeric), ~ifelse(. == -Inf, NA, .)),
      across(where(is.numeric), ~ifelse(is.nan(.), NA, .)),
      sleep_index = sleep_hours*sleep_qualilty, 
      study_day = 1:n(),
      
      #rolling
      roll_7d_tss = slidify_vec(tss, .f = mean, .period = 7, .align = "right", .partial = T ),#
      roll_7d_tss_EWMA = pracma::movavg(tss, n = 7, type = "e"),  #exponential weighted average tss
      roll_7d_sd_tss = slidify_vec(tss, .f = sd, .period = 7, .align = "right", .partial = T ),  #used for calculation
      roll_7d_tss_total = slidify_vec(tss, .f = sum, .period = 7, .align = "right", .partial = T ), #used for calculation
      
      roll_7d_exercise_load_total = slidify_vec(load, .f = sum, .period = 7, .align = "right", .partial = T ),#used for calculation
      roll_7d_exercise_load = slidify_vec(load, .f = mean, .period = 7, .align = "right", .partial = T ),#
      roll_7d_exercise_load_sd = slidify_vec(load, .f = sd, .period = 7, .align = "right", .partial = T ),##used for calculation
      roll_7d_exercise_load_EWMA = pracma::movavg(load, n = 7, type = "e"),  #exponential weighted average tss
      
      roll_monotony = roll_7d_exercise_load/roll_7d_exercise_load_sd,
      roll_strain = roll_monotony*roll_7d_exercise_load_total,
      
      roll_7d_carb_g = slidify_vec(diet_carb_g, .f = mean, .period = 7, .align = "right", .partial = T ),#
      roll_7d_carb_g_sd = slidify_vec(diet_carb_g, .f = sd, .period = 7, .align = "right", .partial = T ), #
      roll_carb_monotony = roll_7d_carb_g/roll_7d_carb_g_sd,
      
      #hrv
      hrv_lag = lag_vec(hrv),   #used for calculation
      hrv_chng = hrv-hrv_lag,  #change from yesterday
      #next_day_hrv_chng = lead_vec(delta_hrv_chng), #change for next day
      roll_7d_hrv_MA = slidify_vec(hrv, .f = mean, .period = 7, .align = "right", .partial = T ),
      
      #pulse
      pulse_lag = lag_vec(pulse),   #used for calculation
      delta_pulse_chng = pulse-pulse_lag,  #change from yesterday
      
      #subject info
      subject_missing_data = percent_missing_data,
      subject_missing_diet = percent_missing_diet,
      subject_weekly_training_h = sum(duration_min)/ nrow(.)/60 * 7,
      
      #diet add-ons
      diet_kcal_kg = diet_kcal/weight_kg,
      diet_carb_g_kg = diet_carb_g/weight_kg,
      diet_fat_g_kg = diet_fat_g/weight_kg,
      diet_protein_g_kg = diet_protein_g/weight_kg,
      
      
    ) %>%
    slice(-1) %>% 
    
    mutate(
      roll_7d_exercise_load_max =  slidify_vec(load, .f = max, na.rm = T, .period = 7, .align = "right", .partial = T ), 
      roll_7d_exercise_rpe_max =  slidify_vec(RPE_max, .f = max, na.rm = T, .period = 7, .align = "right", .partial = T ), 
      
      roll_7d_avg_hrv_delta = slidify_vec(hrv_chng, .f = mean, .period = 7, .align = "right", .partial = T ),
      roll_7d_avg_pulse_delta = slidify_vec(delta_pulse_chng, .f = mean, .period = 7, .align = "right", .partial = T ), 
      roll_7d_sleep_hours = slidify_vec(sleep_hours, .f = mean, .period = 7, .align = "right", .partial = T ), 
      roll_7d_sleep_index = slidify_vec(sleep_index, .f = mean, .period = 7, .align = "right", .partial = T ),
      
      roll_3d_diet_carb_g_kg = slidify_vec(diet_carb_g_kg, .f = mean, .period = 3, .align = "right", .partial = T ),
      roll_7d_diet_carb_g_kg = slidify_vec(diet_carb_g_kg, .f = mean, .period = 7, .align = "right", .partial = T ),
      
      
      roll_3d_diet_kcal_kg = slidify_vec(diet_kcal_kg, .f = mean, .period = 3, .align = "right", .partial = T ),
      roll_7d_diet_kcal_kg = slidify_vec(diet_kcal_kg, .f = mean, .period = 7, .align = "right", .partial = T ),
      
      roll_3d_diet_protein_g_kg = slidify_vec(diet_protein_g_kg, .f = mean, .period = 3, .align = "right", .partial = T ),
      roll_7d_diet_protein_g_kg = slidify_vec(diet_protein_g_kg, .f = mean, .period = 7, .align = "right", .partial = T ),
      
      roll_3d_diet_fat_g_kg = slidify_vec(diet_fat_g_kg, .f = mean, .period = 3, .align = "right", .partial = T ),
      roll_7d_diet_fat_g_kg = slidify_vec(diet_fat_g_kg, .f = mean, .period = 7, .align = "right", .partial = T ),
      
    ) %>% 
    
    select(-c(roll_7d_tss_total, roll_7d_sd_tss,  hrv_lag, pulse_lag, roll_7d_exercise_load_sd, roll_7d_exercise_load_total)) %>% 
    
    
    rename_with(~str_c("AM_", .), c(soreness, stress, zq_score, sleep_qualilty)) %>%
    rename_with(~str_c("exercise_", .), c(duration_min, RPE_max, RPE_weighted, load, tss, wrkts_per_day, CARB_before, TF, fasted, contains("mode_"))) %>%
    
    
    select(sort(tidyselect::peek_vars())) %>% 
    relocate(contains("hrv"), .after = everything()) %>% 
    relocate(contains("pulse"), .before = hrv) %>% 
    relocate(contains("subject_"), .after = everything()) %>% 
    mutate(
      across(c(where(is.numeric)),  ~ifelse(. == "NaN", NA, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "Inf", NA, .)),
      across(c(where(is.numeric)),  ~ifelse(. == "-Inf", NA, .)),
    ) 
  
  
}

