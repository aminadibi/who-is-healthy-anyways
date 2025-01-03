
library(tidymodels)
library(vip)
library(rspiro)
vb_df <- refs_adult[[1]]
set.seed(123)
vb_split <- initial_split(vb_df)
vb_train <- training(vb_split)
vb_test <- testing(vb_split)



xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), vb_train),
  learn_rate(),
  size = 30
)

xgb_grid

xgb_wf <- workflow() %>%
  add_formula(fev1 ~ age + sex + height) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(123)
vb_folds <- vfold_cv(vb_train)

vb_folds

doParallel::registerDoParallel()

set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res


collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")


best_rmse <- select_best(xgb_res, metric= "rmse")
best_rmse

final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

final_xgb



final_xgb %>%
  fit(data = vb_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")


final_res <- last_fit(final_xgb, vb_split)

collect_metrics(final_res)


final_res %>%
  collect_predictions() %>%
  yardstick::rsq(fev1, .pred)

message('rmse for xgboost is:')

final_res %>%
  collect_predictions() %>%
  yardstick::rmse(fev1, .pred)

final_res %>%
  collect_predictions() %>%
  ggplot() + 
  geom_point(aes(y=.pred, x=fev1)) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) + ylim(0, 6000) + xlim(0, 6000) +
  theme_few()

gli_gl <- 
  vb_test %>%
  mutate(.pred_gl = 1000*rspiro::pred_GLIgl(age, height/100, gender=sex, param="FEV1")) %>%
  select(.pred_gl)

message('rmse for gli global is:')

final_res %>%
  collect_predictions() %>%
  mutate(.pred_gl=gli_gl$.pred_gl) %>%
  yardstick::rmse(fev1, .pred_gl)

final_res %>%
  collect_predictions() %>%
  mutate(.pred_gl=gli_gl$.pred_gl) %>%
  ggplot() + 
  geom_point(aes(y=.pred_gl, x=fev1)) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) + ylim(0, 6000) + xlim(0, 6000) +
  theme_few()



