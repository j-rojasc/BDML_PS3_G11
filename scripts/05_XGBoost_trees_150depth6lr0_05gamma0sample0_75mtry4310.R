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

# Load required libraries
source(file.path(dir$scripts, "00_load_requirements.R"))

# Load inputs
train <- import(file.path(dir$processed, 'train_clean.rds'))
test <- import(file.path(dir$processed, 'test_clean.rds'))


# Verificar proporciones originales de estratos para muestrear y balancear
prop_original <- train %>%
  count(estrato) %>%
  mutate(prop = n / sum(n))

print(prop_original)

# Resampleo estratificado (25% manteniendo proporciones)
set.seed(123)  # Para reproducibilidad
train <- train %>%
  group_by(estrato) %>%
  slice_sample(prop = 0.45) %>%  # 25% por estrato
  ungroup()

# Verificar nuevas proporciones
prop_resampled <- train %>%
  count(estrato) %>%
  mutate(prop = n / sum(n))

print(prop_resampled)
# -------------------------------------------------------------------------
# 1. Modelo XGBoost (versión reducida de prueba)
# -------------------------------------------------------------------------

set.seed(123)

# Especificación del modelo
xgb_spec <- parsnip::boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(), 
  sample_size = tune(),
  mtry = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Asegúrate de que 'train' no sea un objeto sf (convertirlo después)
train_df <- train %>% 
  select(-lon, -lat)  # Elimina coordenadas para la receta

# Receta SIN lon/lat (usa solo variables predictoras)
receta <- recipe(price ~ ., data = train_df) %>%
  step_rm(precio_m2, precio_m2_sc) %>%
  step_tokenize(title, description) %>%
  step_stopwords(title, description) %>%
  step_tokenfilter(title, description, max_tokens = 100) %>%
  step_tfidf(title, description) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

workflow_xgb <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(receta)

# Crear folds espaciales (usando el objeto sf original)
sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)
block_folds <- spatial_block_cv(sf_train, v = 5)

# Calcular el número total de predictores después del preprocesamiento
num_predictors <- ncol(juice(prep(receta))) - 1  # Resta 1 por la variable respuesta

# Modificar el grid para usar valores enteros
#grid_xgb <- expand.grid(
#  trees = c(150),
#  tree_depth = c(4, 6),
#  learn_rate = c(0.03, 0.07),
#  loss_reduction = c(0, 0.5),
#  sample_size = c(0.75),
#  mtry = floor(c(0.3, 0.4, 0.5) * num_predictors)  # Convierte a enteros
#) %>%
#  distinct()  # Elimina posibles duplicados al redondear

grid_xgb <- expand.grid(
  trees = c(150),                          # Fijo
  tree_depth = c(4, 6),                    # 2 valores → Mantener
  learn_rate = c(0.05),                    # Reducir a 1 valor intermedio (ej: 0.05)
  loss_reduction = c(0),                   # Reducir a 1 valor (0 es común)
  sample_size = c(0.75),                   # Fijo
  mtry = floor(c(0.4) * num_predictors)    # Reducir a 1 valor intermedio (ej: 0.4)
) %>%
  distinct()

# Validación cruzada espacial con solo 1 fold
#sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)
#set.seed(1111)
#block_folds <- spatial_block_cv(sf_train, v = 2)


# Resuelve conflictos primero
conflicts_prefer(yardstick::mae)
conflicts_prefer(yardstick::rmse)
conflicts_prefer(yardstick::rsq)

tune_res_xgb <- tune_grid(
  workflow_xgb,
  resamples = block_folds,
  grid = grid_xgb,
  metrics = metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq),
  control = control_grid(save_pred = TRUE, verbose = TRUE)
)

# Revisar si hay errores
#show_notes(tune_res_xgb)


# Selección de mejores hiperparámetros
best_xgb <- tune::select_best(tune_res_xgb, metric = "rmse")

# Entrenamiento final
final_xgb <- finalize_workflow(workflow_xgb, best_xgb)
final_fit <- fit(final_xgb, data = train)  # Sin select()


# Acceder a los mejores hiperparámetros
params <- best_xgb

# Reemplazar puntos por guiones bajos
trees <- gsub("\\.", "_", as.character(params$trees))
tree_depth <- gsub("\\.", "_", as.character(params$tree_depth))
learn_rate <- gsub("\\.", "_", as.character(params$learn_rate))
loss_reduction <- gsub("\\.", "_", as.character(params$loss_reduction))
sample_size <- gsub("\\.", "_", as.character(params$sample_size))
mtry <- gsub("\\.", "_", as.character(params$mtry))

# Construir nombre del archivo
name <- paste0(
  "XGBoost_trees_", trees,
  "depth", tree_depth,
  "lr", learn_rate,
  "gamma", loss_reduction,
  "sample", sample_size,
  "mtry", mtry,
  ".csv"
)

# Asegúrate de que test tenga las columnas requeridas (aunque sea con NA)
test_fixed <- test %>%
  mutate(
    precio_m2 = ifelse("precio_m2" %in% names(.), precio_m2, NA_real_),
    precio_m2_sc = ifelse("precio_m2_sc" %in% names(.), precio_m2_sc, NA_real_)
  )

# Predicción
predicted_prices <- augment(final_fit, new_data = test_fixed)

submission <- test %>%
  select(property_id) %>%
  mutate(price = predicted_prices$.pred)

# Guardar archivo en la carpeta deseada
write.csv(submission, file.path(dir$models, name), row.names = FALSE)