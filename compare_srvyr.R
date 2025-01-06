
ref1_bw <- readRDS("nhanes_2007_2012.rds") %>%
  mutate(fev1 = fev1_pre,
         fvc  = fvc_pre) %>%
  drop_na(fev1, fvc) %>%
  filter(spiro_status_pre == 1) %>%
  filter(fev1_quality_pre == "A" | fev1_quality_pre == "B") %>%
  filter(fvc_quality_pre  == "A" | fvc_quality_pre  == "B") %>%
  filter(race %in% c(3,4)) %>%
  filter ((is.na(current_smoke) | (current_smoke == "3")) &
            (is.na(ever_100_smoke) | (ever_100_smoke != "1")) &
            (is.na(last_5_days_cigarettes) |  (last_5_days_cigarettes != "1")) &
            (is.na(last_5_days_pipes) | (last_5_days_pipes != "1")) &
            (is.na(last_5_days_cigars) | (last_5_days_cigars != "1")) &
            (is.na(ever_asthma) | (ever_asthma !="1")) &
            (is.na(ever_bronchitis) | (ever_bronchitis != "1")) &
            (is.na(ever_emphysema) | (ever_emphysema != "1")) &
            (is.na(cough_3_month) | (cough_3_month!=1)) & 
            (is.na(phlegm_3_month) | (phlegm_3_month!=1)) & 
            (is.na(wheezing_past_yr) | (wheezing_past_yr!=1)) & 
            (is.na(dry_cough_night_past_yr) | (dry_cough_night_past_yr!=1))) %>%
  mutate(age_group = ifelse(age>=20, "Adults", "Youth"),
         sex_text   = case_match(sex,
                                 "1" ~ "Male"  ,
                                 "2" ~ "Female" ))

ref1_bw_simple <- readRDS("nhanes_2007_2012.rds") %>%
  mutate(fev1 = fev1_pre,
         fvc  = fvc_pre) %>%
  dplyr::filter(race %in% c(3,4)) 

ref1_svy <- readRDS("nhanes_2007_2012.rds") %>%
  mutate(fev1 = fev1_pre,
         fvc  = fvc_pre) %>%
  srvyr::filter(race %in% c(3,4))


tidy(lm(fev1 ~ age + height + sex + race, data=ref1_bw_simple, weights = MEC6YR))

library(srvyr)
library(survey)

nhanes_des <- ref1_svy %>%
  as_survey_design(
    weights=MEC6YR,
    ids=SDMVPSU,
    strata=SDMVSTRA,
    nest=TRUE
  )

tidy(
  nhanes_des %>%
    svyglm(
      formula = fev1 ~ age + height + sex + race,
      design = .,
      na.action = na.omit
    ))

# Point estimates are the same, but standard errors differ

#now let's compare table 1

library(gtsummary)

nhanes_des %>% srvyr::select(fev1, fvc, age, race, sex, height) %>%
  tbl_svysummary(
    missing_stat = "{N_miss}",
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)}", all_continuous() ~ "{N_miss_unweighted}"))