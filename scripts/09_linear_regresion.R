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


# Verificar proporciones originales
prop_original <- train %>%
  count(estrato) %>%
  mutate(prop = n / sum(n))

print(prop_original)

set.seed(123)  # Para reproducibilidad



# -------------------------------------------------------------------------
# 1. Regresion lineal
# -------------------------------------------------------------------------

set.seed(123)


# Asegúrate de que 'train' no sea un objeto sf (convertirlo después)
train_df <- train %>% 
  select(-lon, -lat, -property_id)  # Elimina coordenadas para la receta

# Receta SIN lon/lat (usa solo variables predictoras)
receta <- recipe(price ~ ., data = train_df) %>%
  step_rm(precio_m2, precio_m2_sc) %>%
  step_tokenize(title, description) %>%
  step_stopwords(title, description) %>%
  step_tokenfilter(title, description, max_tokens = 100) %>%
  step_tfidf(title, description) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Definir especificación del modelo de regresión lineal
lm_spec <- linear_reg() %>%
  set_engine("lm")

# Crear workflow con la receta y modelo lineal
workflow_lm <- workflow() %>%
  add_model(lm_spec) %>%
  add_recipe(receta)

# Entrenamiento final (no necesitas validación cruzada aquí)
final_fit <- fit(workflow_lm, data = train)

test_fixed <- test %>%
  mutate(
    precio_m2 = ifelse("precio_m2" %in% names(.), precio_m2, NA_real_),
    precio_m2_sc = ifelse("precio_m2_sc" %in% names(.), precio_m2_sc, NA_real_),
    across(is.character, ~ifelse(is.na(.), "missing", .)),
    across(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))
  )


# Predicción sobre test (asegúrate de que las columnas coincidan)
predicted_prices <- augment(final_fit, new_data = test_fixed)

# Generar nombre del archivo
name <- "LinearRegression.csv"

# Crear archivo de envío
submission <- test %>%
  select(property_id) %>%
  mutate(price = predicted_prices$.pred)

write.csv(submission, file.path(dir$models, name), row.names = FALSE)

