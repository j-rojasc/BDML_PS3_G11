# =========================================================================
# 0. Workspace configuration
# =========================================================================

# Clear workspace
rm(list = ls())

# Set up paths
dir <- list()
dir$root <- getwd()
dir$processed <- file.path(dir$root, "stores", "processed")
dir$raw <- file.path(dir$root, "stores", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries
source(file.path(dir$scripts, "00_load_requirements.R"))

# =========================================================================
# 1. Model Setup
# =========================================================================

# Check the ratio of train/test data
nrow(test) / nrow(train) # check the % to train to avoid overfitting

# Specify the CART model (Decision Tree for Regression)
cart_spec <- parsnip::decision_tree(
  cost_complexity = tune(),  # Complexity of the tree (pruning)
  tree_depth = tune(),       # Maximum depth of the tree
  min_n = tune()             # Minimum number of observations per leaf
) %>% 
  set_engine('rpart') %>% 
  set_mode('regression')     # As this is a regression problem

# Specify the search grid for hyperparameters (tuning parameters)
grid_values <- grid_regular(
  cost_complexity(range = c(0, 0.1)), 
  tree_depth(range = c(1, 10)), 
  min_n(range = c(1, 20)), 
  levels = 5
)

# Create recipes for data preprocessing
recipe1 <- recipe(price ~ distancia_parque + area_parque + distancia_estaciones
                  + distancia_mall + area_mall + distancia_unis + rooms
                  + bathrooms + property_type, data = train) %>% 
  step_interact(terms = ~ distancia_parque:property_type + 
                  area_parque:property_type +
                  distancia_estaciones:property_type + 
                  distancia_mall:property_type +
                  area_mall:property_type +
                  distancia_unis:property_type) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Another recipe with interactions and polynomial transformations
recipe2 <- recipe(price ~ distancia_parque + area_parque + distancia_estaciones
                  + distancia_mall + area_mall + distancia_unis + rooms
                  + bathrooms + property_type + surface_total, 
                  data = train) %>% 
  step_interact(terms = ~ distancia_parque:property_type + 
                  area_parque:property_type +
                  distancia_estaciones:property_type + 
                  distancia_mall:property_type +
                  area_mall:property_type +
                  distancia_unis:property_type) %>% 
  step_interact(terms = ~ distancia_parque:area_parque +
                  distancia_mall:area_mall) %>% 
  step_poly(distancia_parque, area_parque, distancia_estaciones, distancia_mall,
            area_mall, distancia_unis, degree = 2) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Create workflows
workflow1 <- workflow() %>% 
  add_recipe(recipe1) %>% 
  add_model(cart_spec)

workflow2 <- workflow() %>% 
  add_recipe(recipe2) %>% 
  add_model(cart_spec)

# Spatial cross-validation
sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)

set.seed(1111)
block_folds <- spatial_block_cv(sf_train, v = 5)

# Plotting spatial folds
autoplot(block_folds)
walk(block_folds$splits, function(x) print(autoplot(x)))

# Train and select hyperparameters
tune_res1 <- tune::tune_grid(
  workflow1,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)  # Mean Absolute Error as the evaluation metric
)

workflowsets::collect_metrics(tune_res1)

tune_res2 <- tune::tune_grid(
  workflow2,
  resamples = block_folds,
  grid = grid_values,
  metrics = metric_set(mae)
)

workflowsets::collect_metrics(tune_res2)

# Selecting the best hyperparameters
best_tuneres1 <- select_best(tune_res1, metric = 'mae')
best_tuneres1

best_tuneres2 <- select_best(tune_res2, metric = 'mae')
best_tuneres2

# Finalize workflows with best hyperparameters

res_final1 <- finalize_workflow(workflow1, best_tuneres1)
res_final2 <- finalize_workflow(workflow2, best_tuneres2)

# Fit final models on the training data
cart_final1_fit <- fit(res_final1, data = train)
cart_final2_fit <- fit(res_final2, data = train)

# ajuste name

test <- test %>% rename(distancia_unis=distancia_university)

# Get predictions over the test data
augment(cart_final1_fit, new_data = test) %>% 
  mae(truth = price, estimate = .pred)

augment(cart_final2_fit, new_data = test) %>% 
  mae(truth = price, estimate = .pred)

# Si no tienes columna ID, puedes crear una
predicciones$id <- 1:nrow(predicciones)

