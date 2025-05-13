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

median_totalsur <- median(train$surface_total, na.rm = T) # calculate median for surfaces
median_coveredsur <- median(train$surface_covered, na.rm = T)

# input missing data

train <- train %>% 
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_total = replace_na(surface_total, median_totalsur),
         surface_covered = replace_na(surface_covered, median_coveredsur))

colSums(sapply(train, is.na)) > 0
vis_dat(train)

# check distribution of numeric variables

stargazer(train, type = 'text')
# valores raros en surface covered 2 m2, 0 bedrooms. Limpieza de estos datos según descripción.

# calculate price per m2

train <- train %>% 
  mutate(precio_m2 = round(price / surface_total, 0)) %>% 
  mutate(precio_m2 = precio_m2 / 1000000)

stargazer(train['precio_m2'], type = 'text') # valores mínimos y máximos irreales

hist(train$precio_m2)

# work with outliers

p1 <- train %>% 
  ggplot(aes(y = precio_m2)) +
  geom_boxplot(fill = 'darkblue', alpha = 0.4) +
  labs(title = 'Muestra Completa',
       y = 'Precio por metro cuadrado (millones)', x = '') +
  theme_minimal()

perc1 <- unname(round(quantile(train$precio_m2, probs = c(0.01)), 2))
up <- round(mean(train$precio_m2) + 2*sd(train$precio_m2))

p2 <- train %>% 
  filter(between(precio_m2, perc1, up)) %>% 
  ggplot(aes(y = precio_m2)) +
  geom_boxplot(fill = 'darkblue', alpha = 0.4) +
  labs(title = 'Muestra Filtrada',
       y = 'Precio por metro cuadrado (millones)', x = '') +
  theme_minimal()
grid.arrange(p1, p2, ncol = 2)

train <- train %>% 
  filter(between(precio_m2, perc1, up))

plot_price <- ggplot(train, aes(x = log10(price))) +
  geom_histogram(binwidth = 0.05, fill = 'darkblue', alpha = 0.4) +
  scale_x_continuous(labels = function(x) scales::dollar(10^x, accuracy = 1),
                     name = 'Valor de venta (pesos reales, escala log)') +
  labs(y = 'Cantidad') +
  theme_minimal()
ggplotly(plot_price)

# create a base map

leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = train$lon,
             lat = train$lat)


