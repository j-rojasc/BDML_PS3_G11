# =========================================================
# 0. Workspace configuration
# =========================================================

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

train <- read.csv(file.path(dir$raw, "train.csv"))
test <- read.csv(file.path(dir$raw, "test.csv"))

dim(train) # check dimensions
table(train$operation_type) # check operation type - all venta

train %>%
  count(property_type) # check types of properties

constant_vars <- c('city', 'operation_type')
train <- train %>% dplyr:: select(-constant_vars)
dim(train)

colSums(sapply(train, is.na)) > 0

vis_dat(train) # check missing values

train %>% 
  count(rooms) # find mode for rooms

train %>% 
  count(bathrooms) # find mode for bathrooms

