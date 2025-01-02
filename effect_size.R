race_estimate <- function(df, outcome, single_sex=FALSE, includedRace) {
  # df[[outcome]] <- log(df[[outcome]])
  # df$age <- log(df$age)
  # df$height <- log(df$height)
  
  df <- df %>% mutate(survey_weights = importance_weights(MEC6YR))
  
  
  if (single_sex) {formula_text <- as.formula(paste0(outcome, " ~ age + height + race + survey_weights"))}
  else {formula_text <- as.formula(paste0(outcome, " ~ sex + age + height + race + survey_weights"))}
  
  lm_rec <- recipe(formula_text, data=df) %>%
    step_dummy(all_nominal_predictors()) %>%
    # step_center(all_numeric_predictors()) %>%
    # step_scale(all_numeric_predictors()) %>%
    step_log(all_outcomes())
  
  trained_rec <- prep(lm_rec, training=df)
  
  df_train <- bake(trained_rec, new_data = df)  
  
  lr_mod <- linear_reg()
  
  wflow <- 
    workflow() %>% 
    add_model(lr_mod) %>% 
    add_recipe(lm_rec) %>%
    add_case_weights(survey_weights)
  
  lm_fit <- 
    wflow %>% 
    fit(data = df)
  
  #lm_fit <- lm(formula_text, data=df, weights=MEC6YR)
  #plot(lm_fit)
  print(lm_fit %>% 
          extract_fit_engine() %>%
          check_heteroscedasticity())
  
  res <- (tidy(lm_fit, conf.int=TRUE) %>% filter(str_detect(term, "race"))) %>%
    mutate(estimate=exp(estimate))
  
  #filter(term==paste0('race', includedRace[includedRace!=3]))) 
  return(res)
}

compareEffectSizes <- function(ageGroup, SelectedSex){
  if (!(ageGroup %in% c("Adults", "Youth"))) stop("Invalid age group")
  if (!(SelectedSex %in% c("Male", "Female"))) stop("Invalid sex")
  
  ref_eval <- table1 %>% 
    filter(age_group==ageGroup) %>%
    filter(sex_text==SelectedSex) %>%
    mutate(survey_weights = importance_weights(MEC6YR),
           no_2nd_hand_smoke=case_when(
             ((is.na(smoker_in_household) | (smoker_in_household != "Yes"))) ~ TRUE, 
             .default=FALSE),
           has_insurance=factor(has_insurance),
           educated=case_when(
             (is.na(education_adults) | (education_adults %in% c("College graduate or above","Some college or AA degree"))) ~ TRUE,
             .default=FALSE),
           non_obese=case_when(
             ((is.na(bmi) | (bmi<=30))) ~ TRUE,
             .default=FALSE),
           insured=case_when(((is.na(has_insurance) | 
                                 (has_insurance==1))) ~ TRUE, 
                             .default=FALSE),  
           physically_active=case_when(
             ((is.na(vigorous_work) | (vigorous_work=="Yes")) | 
                (is.na(vigorous_recreational)   | (vigorous_recreational=="Yes")) | 
                (is.na(moderate_work)   | (moderate_work=="Yes")) | 
                (is.na(moderate_recreational)   | (moderate_recreational=="Yes"))) ~ TRUE,
             .default=FALSE),
           occupational_exposure=case_when(
             ((is.na(mineral_dusts) | (mineral_dusts!="Yes")) & 
                (is.na(organic_dusts)   | (organic_dusts!="Yes")) & 
                (is.na(exhaust_fumes)   | (exhaust_fumes!="Yes")) & 
                (is.na(other_fumes)     | (other_fumes!="Yes"))) ~ TRUE,
             .default=FALSE),
           healthy_eater=case_when(
             ((is.na(healthy_diet) | 
                 (healthy_diet=="Excellent") | 
                 (healthy_diet=="Very Good") | 
                 (healthy_diet=="Good"))) ~ TRUE,
             .default=FALSE),
           race_text_nhanes=factor(race_text_nhanes, levels=c("Non-Hispanic White", 
                                                              "Non-Hispanic Black",
                                                              "Mexican American",
                                                              "Other Hispanic",
                                                              "Non-Hispanic Asian",
                                                              "Other Race - Including Multi-Racial")),
           fev1=1000*fev1) %>%
    drop_na(height) 
  
  if (ageGroup=="Adults") {
    disparity_rec <- recipe(fev1~age+height+
                              #sex+
                              #    occupational_exposure+
                              #    no_2nd_hand_smoke+non_obese+physically_active+healthy_eater+
                              #    insured+educated+home_owner+
                              race_text_nhanes+survey_weights, data=ref_eval) 
  }
  
  if (ageGroup=="Youth") {
    #TODO had to remove occupational exposure and healthy diet for us to have any rows left for complete case analysis. Need to think about how to handle this. 
    disparity_rec <- recipe(fev1~age+height+
                              #sex+
                              race_text_nhanes+survey_weights, data=ref_eval) 
    
  }
  
  summary(disparity_rec)
  
  std_rec <- disparity_rec %>%
    #  step_impute_bag(smoker_in_household) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors()) %>%
    step_log(all_outcomes())
  
  trained_rec <- prep(std_rec, training=ref_eval)
  
  df_train <- bake(trained_rec, new_data = ref_eval)  
  
  
  lr_mod <- linear_reg()
  
  wflow <- 
    workflow() %>% 
    add_model(lr_mod) %>% 
    add_recipe(std_rec) %>%
    add_case_weights(survey_weights)
  
  lm_fit <- 
    wflow %>% 
    fit(data = ref_eval)
  
  res <- 
    tidy(lm_fit) %>%
    arrange(-abs(estimate))
  
  library(performance)
  print(lm_fit %>% 
          extract_fit_engine() %>%
          check_heteroscedasticity())
  
  return(res)
}

tmp <- compareEffectSizes("Adult", "Male")
