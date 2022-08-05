
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

