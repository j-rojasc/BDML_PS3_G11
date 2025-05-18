# =========================================================================
# 0. Workspace configuration
# =========================================================================

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list ()
dir$root <- getwd()
dir$processed <- file.path(dir$root, "stores", "processed")
dir$raw <- file.path(dir$root, "stores", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requirements.R"))

# Load inputs
train <- import(file.path(dir$processed, 'train_clean.rds'))
test <- import(file.path(dir$processed, 'test_clean.rds'))

# =========================================================================
# 1. Specify the model
# =========================================================================

# check the share of test compared to train to identify overfitting risk
nrow(test) / nrow(train)

# train elastic net
elastic_net_spec <- parsnip::linear_reg(
  penalty = tune(),
  mixture = tune()) %>% 
  set_engine('glmnet')

# specify the search grid to select optimal parameters
grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>% 
  expand_grid(mixture = c(0, 0.25, 0.5, 0.75, 1))

# create recipes to transform data before estimation

# recipe with relevant variables and interactions
recipe1 <- recipe(price ~ distancia_parque + area_parque + distancia_estaciones
                  + distancia_mall + area_mall + distancia_unis + rooms
                  + bathrooms + property_type, data = train) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_interact(terms = ~ distancia_parque:matches('property_type_') + 
                  area_parque:matches('property_type_') +
                  distancia_estaciones:matches('property_type_') + 
                  distancia_mall:matches('property_type_') +
                  area_mall:matches('property_type_') +
                  distancia_unis:matches('property_type_')) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# recipe with interactions and with variables^2
recipe2 <- recipe(price ~ distancia_parque + area_parque + distancia_estaciones
                  + distancia_mall + area_mall + distancia_unis + rooms
                  + bathrooms + property_type, data = train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ distancia_parque:matches('property_type_') + 
                  area_parque:matches('property_type_') +
                  distancia_estaciones:matches('property_type_') + 
                  distancia_mall:matches('property_type_') +
                  area_mall:matches('property_type_') +
                  distancia_unis:matches('property_type_')) %>% 
  step_interact(terms = ~ distancia_parque:area_parque +
                  distancia_mall:area:mall) %>% 
  step_poly(distancia_parque, area_parque, distancia_estaciones, distancia_mall,
            area_mall, distancia_unis, degree = 2) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# create workflows
workflow1 <- workflow() %>% 
  add_recipe(recipe1) %>% 
  add_model(elastic_net_spec)

workflow2 <- workflow() %>% 
  add_recipe(recipe2) %>% 
  add_model(elastic_net_spec)

# =========================================================================
# 2. Train parameters with spatial cross validation
# =========================================================================

sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)

set.seed(1111)
block_folds <- spatial_block_cv(sf_train, v = 5)
block_folds

autoplot(block_folds)

walk(block_folds$splits, function(x) print(autoplot(x)))

# train and select hyperparameters
tune_res1 <- tune::tune_grid(
  workflow1,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)

workflowsets::collect_metrics(tune_res1)

tune_res2 <- tune::tune_grid(
  workflow2,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)

workflowsets::collect_metrics(tune_res2)

# select best hyperparameters
best_tuneres1 <- select_best(tune_res1, metric = 'mae')
best_tuneres1

best_tuneres2 <- select_best(tune_res2, metric = 'mae')
best_tuneres2

# finalize workflow with best hyperparameters
res_final1 <- finalize_workflow(workflow1, best_tuneres1)
res_final2 <- finalize_workflow(workflow2, best_tuneres2)

# =========================================================================
# 3. Predict prices for test
# =========================================================================

# train model with selected hyperparameters
EN_final1_fit <- fit(res_final1, data = train)
EN_final2_fit <- fit(res_final2, data = train)

# get predictions over test data
augment(EN_final1_fit, new_data = test) %>% 
  mae(truth = price, estimate = .pred)

augment(EN_final2_fit, new_data = test) %>% 
  mae(truth = price, estimate = .pred)

# export predictions
predicted_prices <- augment(EN_final1_fit, new_data = test)

submission <- predicted_prices %>% 
  select(property_id = property_id, predicted_prices = .pred)

write.csv(submission, file = file.path(dir$root, 'submission.csv'), row.names = F)

