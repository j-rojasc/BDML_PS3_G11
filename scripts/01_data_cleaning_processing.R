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

# =========================================================
# 1. Exploring and cleaning data (NAs, outliers)
# =========================================================

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

# =========================================================
# 2. Mapping train data
# =========================================================

leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = train$lon,
             lat = train$lat)

limites <- getbb('Bogota Colombia')
train <- train %>% 
  filter(between(lon, limites[1, 'min'], limites[1, 'max']) &
           between(lat, limites[2, 'min'], limites[2, 'max'])
         )

train <- train %>% 
  mutate(precio_m2_sc = ((precio_por_mt2 - min(precio_por_mt2)) / 
                           (max(precio_por_mt2) - min(precio_por_mt2))))

train <- train %>% 
  mutate(color = case_when(property_type == 'Apartamento' ~ 'red',
                           property_type == 'Casa' ~ 'blue'))

html <- paste0("<b>Precio:</b> ",
               scales::dollar(train$price),
               "<br> <b>Area:</b> ",
               as.integer(train$surface_total), " mt2",
               "<br> <b>Tipo de immueble:</b> ",
               train$property_type,
               "<br> <b>Numero de alcobas:</b> ",
               as.integer(train$bedrooms),
               "<br> <b>Numero de baños:</b> ",
               as.integer(train$bathrooms))

lat_central <- mean(train$lat)
lon_central <- mean(train$lon)

leaflet() %>% 
  addTiles() %>% 
  setView(lng = lon_central, lat = lat_central, zoom = 11) %>% 
  addCircles(lng = train$lon,
             lat = train$lat,
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             radius = train$precio_m2_sc*10,
             popup = html)

# transform data to sf
sf_train <- st_as_sf(train, coords = c('lon', 'lat'), crs = 4326)

# =========================================================
# 4. Adding data of localidades
# =========================================================

localidades <- st_read(file.path(dir$raw, "poligonos-localidades.geojson"))
localidades <- st_transform(localidades, 4626)

ggplot() +
  geom_sf(data = localidades %>% 
            filter(!(Nombre.de.la.localidad %in% c('SUMAPAZ',
                                                   'USME',
                                                   'CIUDAD BOLIVAR'))), 
          color = 'orange') +
  geom_sf(data = sf_train, aes(color = precio_m2), shape = 15, size = 0.3) +
  theme_minimal()
# hay varias localidades con cero o pocas observaciones!!

# =========================================================
# 5. Extracting spatial data from OSM
# =========================================================

# check categories of available spatial data
osmdata::available_features()
leisure <- osmdata::available_tags('leisure')
print(leisure, n=33)

# extract info of parks
parques <- opq(bbox = getbb('Bogota Colombia')) %>% 
  add_osm_feature(key = 'leisure', value = 'park')

# transform parks data into sf
parques_sf <- osmdata_sf(parques)

# select polygons and save them
parques_geom <- parques_sf$osm_polygons %>% 
  dplyr::select(osm_id, name)
parques_geom <- st_as_sf(parques_sf$osm_polygons)

# calculate each park's centroid (queremos trabajar con centroides u otra cosa?)
centroides <- st_centroid(parques_geom, byid = T)
centroides <- centroides %>% 
  mutate(x = st_coordinates(centroides)[, 'X']) %>% 
  mutate(y = st_coordinates(centroides)[, 'Y'])

centroides_sf <- st_as_sf(centroides, coords = c('x', 'y'), crs = 4326)

# calculate distances between each property and nearest park
dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
dim(dist_matrix)

# find min distance to any park for each property
dist_min <- apply(dist_matrix, 1, min)
train <- train %>%
  mutate(distancia_parque = dist_min)

# check distribution 
plot_parks <- ggplot(train, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a un parque en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a los parques') +
  theme_minimal()
ggplotly(plot_parks)

# ===============================================================
# 6. Checking the relationship between price and parks
# ===============================================================

# distance to parks
price_parks <- ggplot(train %>% sample_n(1000), aes(x = distancia_parque,
                                                    y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a un parque en metros (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre la proximidad a un parque y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_parks)

# park area
posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
areas <- st_area(parques_geom)
train <- train %>% 
  mutate(area_parque = as.numeric(areas[posicion]))

price_aparks <- ggplot(train %>% sample_n(1000), aes(x = area_parque,
                                                    y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Área del parque más cercano (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre área de un parque y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_aparks)


## saveRDS(train, file.path(dir$processed, paste0("train_clean", ".rds")), row.names = F)




