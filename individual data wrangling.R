library(tidyverse)
library(tidymodels)

library(timetk)
library(lubridate)

library(naniar)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("annotate", "ggplot2")
conflict_prefer("lag", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("margin", "ggplot2")

source("helper functions.R")


# read data ---------------------------------------------------------------


metrics_file <- read_rds("example_metrics.rds")

workout_file <- read_rds("example_workouts.rds")

diet_file <- read_rds("example_diet.rds")


# metrics -----------------------------------------------------------------

metrics_cleaned_tbl <- metrics_file %>% 
  pivot_wider(names_from = type, values_from = value, names_repair = "minimal") %>% 
  janitor::clean_names() %>% 
  mutate(hrv = log(hrv))

metrics_cleaned_tbl 

panel_hist_fn(metrics_cleaned_tbl)


metrics_daily_tbl <- metrics_cleaned_tbl %>% 
  summarise_by_time(date,
                    sleep_hours = max(sleep_hours, na.rm = T),
                    sleep_qualilty = mean(sleep_qualilty, na.rm = T),
                    soreness = mean(soreness, na.rm = T),
                    stress = mean(stress, na.rm = T),
                    hrv = mean(hrv, na.rm = T),
                    zq_score = max(zq_score, na.rm = T),
                    pulse = mean(pulse, na.rm = T),
                    weight_kg = mean(weight_kilograms, na.rm = T),
                    
  ) %>% 
  mutate(sleep_index = sleep_hours*sleep_qualilty, .after = sleep_qualilty) %>% 
  pad_by_time(date)%>% 
  mutate_if(is.numeric,  ~ifelse(. == "NaN", NA, .)) %>%
  mutate_if(is.numeric,  ~ifelse(. == "Inf", NA, .)) %>%  
  mutate_if(is.numeric,  ~ifelse(. == "-Inf", NA, .))

metrics_daily_tbl

metrics_daily_tbl %>% vis_miss()



# workouts ----------------------------------------------------------------

workouts_cleaned_tbl <- workout_file %>% 
  filter(!is.na(time_total_in_hours),
         workout_type != "Other") %>%
  mutate(
    athlete_comments = str_remove_all(athlete_comments, "\\*"),
    RPE = str_extract(athlete_comments, "(RPE....)|(RPE.\\d+)"),
    RPE = str_extract(RPE, "\\d+"),
    TF = str_extract(athlete_comments, "(TF......)|(TF.\\d+)|(TP.\\d+)"),
    TF = str_extract(TF, "\\d+"),
    CARB_before = str_extract(athlete_comments,"(Carb...\\d+)|(CHO...\\d+)|(CHO..\\d+)|(ChO.\\d+)|(CHo.\\d+)|(CHO.\\d+)"),
    CARB_before = str_extract(CARB_before, "\\d+"),
    across(RPE:CARB_before, ~ as.numeric(.)),
    duration_min = time_total_in_hours * 60,
    load = duration_min * RPE/10,
    workout_count = 1,
    fasted = ifelse(CARB_before < 5, 1, 0),
    mode_Bike = ifelse(workout_type == "Bike", 1, 0),
    mode_Run = ifelse(workout_type == "Run", 1, 0),
    mode_Swim = ifelse(workout_type == "Swim", 1, 0),
    mode_Strength = ifelse(workout_type == "Strength", 1, 0),
    mode_Other = ifelse(workout_type %in% c("Bike", "Run", "Swim", "Strength"), 0, 1),
  ) %>% 
  rename(date = workout_day) %>% 
  select(-c(title, time_total_in_hours)) 


workouts_cleaned_tbl %>% glimpse()
workouts_cleaned_tbl %>% vis_miss()

panel_hist_fn(workouts_cleaned_tbl)

##impute
workout_impute_rec <- recipe(tss ~ ., data = workouts_cleaned_tbl) %>%
  update_role(date, fasted, new_role = "id") %>%
  step_impute_linear(RPE,  impute_with = imp_vars(workout_type, tss, duration_min)) %>%
  step_impute_median(CARB_before) %>% 
  prep() %>% juice() %>%
  mutate(load = RPE*duration_min/10,
         fasted = ifelse(CARB_before < 5, 1, 0)
  )

workout_impute_rec %>% vis_miss()


workouts_daily_tbl <- workout_impute_rec %>% 
  filter(!is.na(duration_min)) %>% 
  summarise_by_time(date, .by = "day",
                    TF = weighted.mean(TF, duration_min, na.rm = T),
                    CARB_before = weighted.mean(CARB_before, duration_min, na.rm = T),
                    RPE_weighted = weighted.mean(RPE, duration_min, na.rm = T),
                    RPE_max = max(RPE, na.rm = T),
                    duration_min = sum(duration_min, na.rm = T),
                    load = sum(load, na.rm = T),
                    tss = sum(tss, na.rm = T),
                    wrkts_per_day = sum(workout_count, na.rm = T),
                    fasted = max(fasted, na.rm = T),
                    mode_Bike = max(mode_Bike),
                    mode_Run = max(mode_Run),
                    mode_Swim = max(mode_Swim),
                    mode_Strength = max(mode_Strength),
                    mode_Other = max(mode_Other)
  ) %>% 
  pad_by_time(date) %>% 
  mutate(
    fasted = ifelse(fasted == "-Inf" , 0, fasted),
    across(c(duration_min:wrkts_per_day, fasted:mode_Other), ~replace_na(., 0)),
    across(fasted:mode_Other, ~ as.factor(.)),
    workt_yn = ifelse(wrkts_per_day > 0, 1, 0),
    previous_training_days = bsd.report::cumsum_reset(workt_yn, 0)-1,
    previous_training_days = ifelse(previous_training_days == -1, 0, previous_training_days),
  ) %>% 
  select(-workt_yn) %>% 
  mutate(
    across(TF:tss, ~ifelse(. == "NaN", NA, .)),
    across(TF:tss, ~ifelse(. == "-Inf", NA, .))
  )



# diet --------------------------------------------------------------------

diet_daily_tbl <- diet_file %>% 
  mutate(date = mdy(date)) %>% 
  summarise_by_time(date,
                    diet_kcal = sum(calories), 
                    diet_carb_g = sum(carbohydrates_g),
                    diet_fat_g = sum(fat_g),
                    diet_protein_g = sum(protein_g)
  ) %>% 
  pad_by_time(date)

diet_daily_tbl %>% vis_miss()
#check for outliers
diet_daily_tbl %>% drop_na() %>% 
  plot_anomaly_diagnostics(date, diet_kcal)


# join --------------------------------------------------------------------

joined_tbl <- metrics_daily_tbl %>% 
  left_join(workouts_daily_tbl, by = "date") %>% 
  left_join(diet_daily_tbl, by = "date")

joined_tbl %>% vis_miss()



#  impute -----------------------------------------------------------------

joined_imputed <- joined_imputed_fn(joined_tbl)
joined_imputed %>% glimpse()

prepared_tbl <- feat_eng_fn1(joined_imputed)
prepared_tbl %>% glimpse()
