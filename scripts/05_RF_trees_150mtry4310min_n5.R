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
dir$models <- file.path(dir$root, "stores", "models")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)
library(dplyr)

filter(mtcars, cyl == 8)

# Load required libraries
source(file.path(dir$scripts, "00_load_requirements.R"))

# Load inputs
train <- import(file.path(dir$processed, 'train_clean.rds'))
test <- import(file.path(dir$processed, 'test_clean.rds'))


# Verificar proporciones originales
prop_original <- train %>%
  count(estrato) %>%
  mutate(prop = n / sum(n))

print(prop_original)

# Resampleo estratificado (25% manteniendo proporciones)
set.seed(123)  # Para reproducibilidad
train <- train %>%
  group_by(estrato) %>%
  slice_sample(prop = 0.30) %>%  # 25% por estrato
  ungroup()

# Verificar nuevas proporciones
prop_resampled <- train %>%
  count(estrato) %>%
  mutate(prop = n / sum(n))

print(prop_resampled)
# -------------------------------------------------------------------------
# 1. Modelo Random Forest (versión reducida de prueba)
# -------------------------------------------------------------------------

rf_spec <- parsnip::rand_forest(
  mtry = tune(), # Número de variables en cada split
  trees = 150, # Igual que en XGBoost para tiempo comparable
  min_n = tune() # Controla profundidad (equivalente a tree_depth)
) %>%
  set_engine("ranger", num.threads = parallel::detectCores()) %>% # Paralelización
  set_mode("regression")


train_df <- train %>% select(-lon, -lat)
receta <- recipe(price ~ ., data = train_df) %>%
  step_rm(precio_m2, precio_m2_sc) %>%
  step_tokenize(title, description) %>%
  step_stopwords(title, description) %>%
  step_tokenfilter(title, description, max_tokens = 100) %>%
  step_tfidf(title, description) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

workflow_rf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(receta)


sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)
block_folds <- spatial_block_cv(sf_train, v = 5)


num_predictors <- ncol(juice(prep(receta))) - 1
grid_rf <- expand.grid(
  mtry = floor(c(0.4) * num_predictors),  # Un solo valor intermedio (0.4)
  min_n = c(5, 10)                        # Dos valores para control de profundidad
) %>%
  distinct()

ctrl <- control_grid(
  verbose = TRUE,
  save_pred = FALSE, # No guardar predicciones para ahorrar memoria
  allow_par = TRUE # Permitir paralelización
)

tune_res_rf <- tune_grid(
  workflow_rf,
  resamples = block_folds,
  grid = grid_rf,
  metrics = metric_set(mae, rmse, rsq),
  control = ctrl
)

best_rf <- select_best(tune_res_rf, metric = "rmse")

final_rf <- finalize_workflow(workflow_rf, best_rf)
final_fit_rf <- fit(final_rf, data = train)

test_fixed <- test %>%
  mutate(
    precio_m2 = ifelse("precio_m2" %in% names(.), precio_m2, NA_real_),
    precio_m2_sc = ifelse("precio_m2_sc" %in% names(.), precio_m2_sc, NA_real_)
  )

predicted_prices_rf <- augment(final_fit_rf, new_data = test_fixed)

submission_rf <- test %>%
  select(property_id) %>%
  mutate(price = predicted_prices_rf$.pred)

mtry_rf <- gsub("\\.", "_", as.character(best_rf$mtry))
min_n_rf <- best_rf$min_n

name_rf <- paste0(
  "RF_trees_150",
  "mtry", mtry_rf,
  "min_n", min_n_rf,
  ".csv"
)

write.csv(submission_rf, file.path(dir$models, name_rf), row.names = FALSE)

saveRDS(final_fit_rf, file.path(dir$models,"randomforest_optimizado.rds"))