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
# 1. Exploring and cleaning train data (NAs, outliers)
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
# 2. Exploring and cleaning test data (NAs, outliers)
# =========================================================

dim(test) # check dimensions
table(test$operation_type) # check operation type - all venta

test %>%
  count(property_type) # check types of properties

test <- test %>% dplyr:: select(-constant_vars)
dim(test)

colSums(sapply(test, is.na)) > 0

vis_dat(test) # check missing values

test %>% 
  count(rooms) # find mode for rooms

test %>% 
  count(bathrooms) # find mode for bathrooms

median_totalsur_test <- median(test$surface_total, na.rm = T) # calculate median for surfaces
median_coveredsur_test <- median(test$surface_covered, na.rm = T)

# input missing data

test <- test %>% 
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_total = replace_na(surface_total, median_totalsur_test),
         surface_covered = replace_na(surface_covered, median_coveredsur_test))

colSums(sapply(test, is.na)) > 0
vis_dat(test)

# check distribution of numeric variables
stargazer(test, type = 'text') # valores raros en surface total 15 m2 y 108,800 m2, 0 bedrooms.

# =========================================================
# 3. Mapping train data
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
  mutate(precio_m2_sc = ((precio_m2 - min(precio_m2)) / 
                           (max(precio_m2) - min(precio_m2))))

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
# 4. Mapping test data
# =========================================================

leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = test$lon,
             lat = test$lat)

test <- test %>% 
  filter(between(lon, limites[1, 'min'], limites[1, 'max']) &
           between(lat, limites[2, 'min'], limites[2, 'max'])
  )

test <- test %>% 
  mutate(color = case_when(property_type == 'Apartamento' ~ 'red',
                           property_type == 'Casa' ~ 'blue'))

html_test <- paste0("<br> <b>Area:</b> ",
               as.integer(train$surface_total), " mt2",
               "<br> <b>Tipo de immueble:</b> ",
               train$property_type,
               "<br> <b>Numero de alcobas:</b> ",
               as.integer(train$bedrooms),
               "<br> <b>Numero de baños:</b> ",
               as.integer(train$bathrooms))

lat_central_test <- mean(test$lat)
lon_central_test <- mean(test$lon)

leaflet() %>% 
  addTiles() %>% 
  setView(lng = lon_central_test, lat = lat_central_test, zoom = 11) %>% 
  addCircles(lng = test$lon,
             lat = test$lat,
             col = test$color,
             fillOpacity = 1,
             opacity = 1,
             popup = html_test)

# transform data to sf
sf_test <- st_as_sf(test, coords = c('lon', 'lat'), crs = 4326)

# =========================================================
# 5. Adding data of localidades
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

ggplot() +
  geom_sf(data = localidades, color = 'orange') +
  geom_sf(data = sf_test, color = 'blue', shape = 15, size = 0.3) +
  theme_minimal()

# =========================================================
# 6. Extracting spatial data from OSM
# =========================================================

# ===================== parks =============================

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

# =================== public transport ====================

ptransport <- osmdata::available_tags('public_transport')
print(ptransport)

# extract info
estaciones <- opq(bbox = getbb('Bogota Colombia')) %>%
add_osm_feature(key = 'public_transport', value = 'station')

# transform data into sf
estaciones_sf <- osmdata_sf(estaciones)

# select polygons and save them
estaciones_geom <- estaciones_sf$osm_polygons %>%
  dplyr::select(osm_id, name)
estaciones_geom <- st_as_sf(estaciones_sf$osm_polygons)

# calculate each station's centroid (queremos trabajar con centroides u otra cosa?)
centroides_pt <- st_centroid(estaciones_geom, byid = T)
centroides_pt <- centroides_pt %>% 
  mutate(x = st_coordinates(centroides_pt)[, 'X']) %>% 
  mutate(y = st_coordinates(centroides_pt)[, 'Y'])

centroides_pt_sf <- st_as_sf(centroides_pt, coords = c('x', 'y'), crs = 4326)

# calculate distances between each property and nearest station
dist_matrix_pt <- st_distance(x = sf_train, y = centroides_pt_sf)
dim(dist_matrix_pt)

# find min distance to any station for each property
dist_min_pt <- apply(dist_matrix_pt, 1, min)
train <- train %>%
  mutate(distancia_estaciones = dist_min_pt)

# check distribution 
plot_estaciones <- ggplot(train, aes(x = distancia_estaciones)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a una estación de transporte público en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a las estaciones') +
  theme_minimal()
ggplotly(plot_estaciones)

# =================== malls ========================

shop <- osmdata::available_tags('shop')

# extract info
malls <- opq(bbox = getbb('Bogota Colombia')) %>% 
  add_osm_feature(key = 'shop', value = 'mall')

# transform malls data into sf
malls_sf <- osmdata_sf(malls)

# select polygons and save them
malls_geom <- malls_sf$osm_polygons %>% 
  dplyr::select(osm_id, name)
malls_geom <- st_as_sf(malls_sf$osm_polygons)

# check all geoms are valid
invalids <- st_is_valid(malls_geom, reason = T)
table(invalids)
which(!st_is_valid(malls_geom))
malls_geom <- st_make_valid(malls_geom)


# calculate each malls' centroid (queremos trabajar con centroides u otra cosa?)
centroides_mall <- st_centroid(malls_geom, byid = T)
centroides_mall <- centroides_mall %>% 
  mutate(x = st_coordinates(centroides_mall)[, 'X']) %>% 
  mutate(y = st_coordinates(centroides_mall)[, 'Y'])

centroides_mall_sf <- st_as_sf(centroides_mall, coords = c('x', 'y'), crs = 4326)

# calculate distances between each property and nearest park
dist_matrix_mall <- st_distance(x = sf_train, y = centroides_mall_sf)
dim(dist_matrix_mall)

# find min distance to any park for each property
dist_min_mall <- apply(dist_matrix_mall, 1, min)
train <- train %>%
  mutate(distancia_mall = dist_min_mall)

# check distribution 
plot_malls <- ggplot(train, aes(x = distancia_mall)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a un mall en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a los malls') +
  theme_minimal()
ggplotly(plot_malls)

# ==================== universities ============================

# amenity <- osmdata::available_tags('amenity')
# print(amenity)
# 
# # extract info of parks
# unis <- opq(bbox = getbb('Bogota Colombia')) %>% 
#   add_osm_feature(key = 'leisure', value = 'park')
# 
# # transform parks data into sf
# parques_sf <- osmdata_sf(parques)
# 
# # select polygons and save them
# parques_geom <- parques_sf$osm_polygons %>% 
#   dplyr::select(osm_id, name)
# parques_geom <- st_as_sf(parques_sf$osm_polygons)
# 
# # calculate each park's centroid (queremos trabajar con centroides u otra cosa?)
# centroides <- st_centroid(parques_geom, byid = T)
# centroides <- centroides %>% 
#   mutate(x = st_coordinates(centroides)[, 'X']) %>% 
#   mutate(y = st_coordinates(centroides)[, 'Y'])
# 
# centroides_sf <- st_as_sf(centroides, coords = c('x', 'y'), crs = 4326)
# 
# # calculate distances between each property and nearest park
# dist_matrix <- st_distance(x = sf_train, y = centroides_sf)
# dim(dist_matrix)
# 
# # find min distance to any park for each property
# dist_min <- apply(dist_matrix, 1, min)
# train <- train %>%
#   mutate(distancia_parque = dist_min)
# 
# # check distribution 
# plot_parks <- ggplot(train, aes(x = distancia_parque)) +
#   geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
#   labs(x = 'Distancia mínima a un parque en metros',
#        y = 'Cantidad',
#        title = 'Distribución de la distancia a los parques') +
#   theme_minimal()
# ggplotly(plot_parks)

# ===============================================================
# 7. Checking the relationship between price and parks
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

# ===============================================================
# 8. Checking the relationship between price and stations
# ===============================================================

# distance to public transport stations
price_stations <- ggplot(train %>% sample_n(1000), aes(x = distancia_estaciones,
                                                    y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a una estación de transporte público en metros (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre la proximidad a una estación y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_stations)

# ===============================================================
# 9. Checking the relationship between price and malls
# ===============================================================

# distance
price_malls <- ggplot(train %>% sample_n(1000), aes(x = distancia_mall,
                                                       y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a un mall en metros (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre la proximidad a un mall y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_malls)

# area
posicion_mall <- apply(dist_matrix_mall, 1, function(x) which(min(x) == x))

areas1 <- st_area(malls_geom)
train <- train %>% 
  mutate(area_mall = as.numeric(areas1[posicion_mall]))


price_amalls <- ggplot(train %>% sample_n(1000), aes(x = area_mall,
                                                     y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Área del mall más cercano (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre área de un mall y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_amalls)

## saveRDS(train, file.path(dir$processed, paste0("train_clean", ".rds")), row.names = F)




