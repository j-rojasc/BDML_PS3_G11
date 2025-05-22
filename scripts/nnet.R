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

library(keras)
install_keras(
  method = 'virtualenv',
  envname = 'r-reticulate',
  tensorflow = '2.13.0',
  restart_session = T
)

# Load inputs
train <- readRDS(file.path(dir$processed, 'train_clean.rds'))
test <- readRDS(file.path(dir$processed, 'test_clean.rds'))

# Eliminar columnas innecesarias
train <- train %>% select(-c(title, description))
test <- test %>% select(-c(title, description))

binarias <- c('vigilance', 'common_areas', 'gym', 'humidity_zones', 'deposit')

train[binarias] <- lapply(train[binarias], function(x) {
  x[is.na(x)] <- as.numeric(names(sort(table(x), decreasing = TRUE))[1])  # valor más frecuente
  return(as.numeric(x))
})

test[binarias] <- lapply(test[binarias], function(x) {
  x[is.na(x)] <- as.numeric(names(sort(table(x), decreasing = TRUE))[1])
  return(as.numeric(x))
})


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

model_keras <- parsnip::mlp(
  hidden_units = 10,
  dropout = 0.2,
  epochs = 20
) %>% 
  set_engine('keras') %>% 
  set_mode('regression')

grid_values <- tidyr::crossing(
  hidden_units = seq(from = 10, to = 30, by = 10),
  epochs = c(150, 200))

workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>% 
  add_model(nnet_tune)

workflow_keras <- workflow() %>% 
  add_recipe(recipe_nnet) %>% 
  add_model(model_keras)

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

tune_nnet2 <- tune_grid(
  workflow_tune2,
  resamples = block_folds,
  grid = grid_values2,
  metrics = metric_set(mae),
  control = control_grid(verbose = T))

workflowsets::collect_metrics(tune_nnet)

workflowsets::collect_metrics(tune_nnet2)

best_tune_nnet <- select_best(tune_nnet, metric = 'mae')
best_tune_nnet

best_tune_nnet2 <- select_best(tune_nnet2, metric = 'mae')
best_tune_nnet2

nnet_tuned_final <- finalize_workflow(workflow_tune, best_tune_nnet)

nnet_tuned_final2 <- finalize_workflow(workflow_tune2, best_tune_nnet2)

nnet_tuned_final_fit <- fit(nnet_tuned_final, data = train)

nnet_tuned_final_fit2 <- fit(nnet_tuned_final2, data = train)

predicted_prices <- broom::augment(nnet_tuned_final_fit, new_data = test)

predicted_prices2 <- broom::augment(nnet_tuned_final_fit2, new_data = test)

submission <- test %>%
  select(property_id) %>% 
  mutate(price = predicted_prices$.pred)

submission2 <- test %>%
  select(property_id) %>% 
  mutate(price = predicted_prices2$.pred)

write.csv(submission, file = file.path(dir$models, 'NeuralNetwork_20hiddenunits_150epochs.csv'), row.names = F)

write.csv(submission2, file = file.path(dir$models, 'NeuralNetwork_20hiddenunits_150epochs.csv'), row.names = F)
