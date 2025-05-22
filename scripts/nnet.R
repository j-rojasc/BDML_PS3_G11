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
dir$models <- file.path(dir$root, "stores", "models")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requirements.R"))

# Load inputs
train <- readRDS(file.path(dir$processed, 'train_clean.rds'))
test <- readRDS(file.path(dir$processed, 'test_clean.rds'))

formula <- as.formula(
  paste("price ~ surface_total + rooms + bathrooms + property_type + usage_type +
        parking_spaces + vigilance + common_areas + gym + humidity_zones +
        deposit + distancia_parque + distancia_estaciones + distancia_mall +
        distancia_unis + estrato")
)

recipe_nnet <- recipes::recipe(
  formula, data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_poly(surface_total, degree = 2) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# specify the nnet
nnet_tune <- parsnip::mlp(
  hidden_units = tune(),
  epochs = tune()) %>% 
  parsnip::set_mode('regression') %>% 
  parsnip::set_engine('nnet')

grid_values <- tidyr::crossing(
  hidden_units = seq(from = 10, to = 30, by = 10),
  epochs = c(150, 200))

workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>% 
  add_model(nnet_tune)

sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)

# select hyperparamenters using spatial cross validation
set.seed(1111)

block_folds <- spatial_block_cv(sf_train, v = 6)
block_folds

set.seed(1111)

tune_nnet <- tune_grid(
  workflow_tune,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae),
  control = control_grid(verbose = T)
)

workflowsets::collect_metrics(tune_nnet)

best_tune_nnet <- select_best(tune_nnet, metric = 'mae')
best_tune_nnet

nnet_tuned_final <- finalize_workflow(workflow_tune, best_tune_nnet)

nnet_tuned_final_fit <- fit(nnet_tuned_final, data = train)

predicted_prices <- broom::augment(nnet_tuned_final_fit, new_data = test)

submission <- test %>%
  select(property_id) %>% 
  mutate(price = predicted_prices$.pred)

write.csv(submission, file = file.path(dir$models, 'NeuralNetwork_20hiddenunits_150epochs.csv'), row.names = F)
