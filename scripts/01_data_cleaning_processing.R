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
dir$models <- file.path(dir$root, "stores", "models")
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

# check operation and property types
table(train$operation_type)

train %>%
  count(property_type)

# select only variables that are not constant
constant_vars <- c('city', 'operation_type')
train <- train %>% dplyr:: select(-constant_vars)

# check columns with missing values
colSums(sapply(train, is.na)) > 0

vis_dat(train)
missing_train <- vis_miss(train)
# ggsave(filename = file.path(dir$views, 'vis_miss_plot_train.png'),
#        plot = missing_train,
#        width = 8, 
#        height = 6, 
#        dpi = 300)

train %>% filter(is.na(title)) %>% count() # 22 missing titles

train %>% filter(is.na(description)) %>% count() # 9 missing descriptions

# find mode and median to replace missing data
train %>% 
  count(rooms)

train %>% 
  count(bathrooms)

median_totalsur <- median(train$surface_total, na.rm = T)
median_coveredsur <- median(train$surface_covered, na.rm = T)

# input missing data with modes and medians
train <- train %>% 
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_total = replace_na(surface_total, median_totalsur),
         surface_covered = replace_na(surface_covered, median_coveredsur))

# check descriptive statistics of numeric variables
# stargazer(train, type = 'latex', out = file.path(dir$views, 'destats_train.tex'))

# create a new column to look for properties with commercial usage (0 bedrooms)
train <- train %>%
  mutate(
    usage_type = case_when(
      str_detect(tolower(description),
                 'bodega|oficina|local|negocio') ~ 'commercial',
      TRUE ~ 'residential'))

train <- train %>% 
  mutate(
    usage_type = case_when(
      property_id %in% c('5e728b7e8963db83de6b24f0', 
                         '49679ddbae871d957d44fc47', 
                         '1021686694a0b7d514752220',
                         'fcf646927a1a711a4624db0e') ~ 'residential',
      TRUE ~ usage_type
    )
  )

train <- train %>% 
  mutate(
    usage_type = case_when(
      property_id %in% c('7414965b658996300406cbc8',
                         'bd396a0122feddffe741c259',
                         '5ed18f05976f694207732c46',
                         'e61f9417246e8663bc061bde',
                         '1dfffcad24a86f44298a6a4c',
                         '257a6e95ec86faacc0645adf',
                         '03d6a954744e72f7928ac456',
                         'a9d74d8a46fb12f0bcc79a58',
                         'fe0682be1fbc31b7c5580afa',
                         '91c2760388ad1e3aa437b9e6',
                         '5fdd57730a451a2e31177600'
                         ) ~ 'commercial',
      TRUE ~ usage_type
    )
  )

#Dictionary to convert words to numbers
word_to_number <- c(
  "una" = 1, "un" = 1, "uno" = 1,
  "dos" = 2, "tres" = 3, "cuatro" = 4,
  "cinco" = 5, "seis" = 6, "siete" = 7,
  "ocho" = 8, "nueve" = 9, "diez" = 10,
  "once" = 11, "doce" = 12, "trece" = 13,
  "catorce" = 14, "quince" = 15
)

# Function Words to number
to_number <- function(x) {
  x <- tolower(x)
  ifelse(x %in% names(word_to_number), word_to_number[x], as.numeric(x))
}

# Pattern for words and number
number_pattern <- paste(c("\\d+", names(word_to_number)), collapse = "|")
regex_bedrooms <- paste0("(", number_pattern, ")\\s*(alcoba|habitación|cuarto)s?")

# extract number of bedrooms from descriptions
train <- train %>%
  mutate(
    extract_raw = case_when(
      usage_type == "residential" & bedrooms == 0 ~ str_extract(description, regex_bedrooms),
      TRUE ~ NA_character_
    ),
    bedrooms_extracted = str_extract(extract_raw, number_pattern),
    bedrooms_extracted = as.numeric(to_number(bedrooms_extracted))
  ) %>% select(-extract_raw)

# change bedrooms if a valid number was extracted
train <- train %>% 
  mutate(
    bedrooms = if_else(
      bedrooms == 0 & usage_type == 'residential' & !is.na(bedrooms_extracted),
      bedrooms_extracted,
      bedrooms
    )
  ) %>% select(-bedrooms_extracted)

# Extract number of parking spaces from descriptions

train <- train %>%
  mutate(
    extract_raw = case_when(
      usage_type == "residential"  ~ str_extract(description, "\\d+\\s*(parqueadero|garaje)s?"),
      TRUE ~ NA_character_
    ),
    parking_spaces_extracted = str_extract(extract_raw, "\\d+"),
    parking_spaces_extracted = as.numeric(to_number(parking_spaces_extracted))
  ) %>% select(-extract_raw)

# change parking_spaces if a valid number was extracted
train <- train %>% 
  mutate(
    parking_spaces = if_else(
       usage_type == 'residential' & !is.na(parking_spaces_extracted) & 
        parking_spaces_extracted <= 5, #Why imposed that there are no home with more than 5 parking spaces?
      parking_spaces_extracted,
      0
    )
  ) %>% select(-parking_spaces_extracted)

# Extract from the title the specific type of property
train <- train %>%
  mutate(
    property_type_extracted = case_when(
      str_detect(tolower(title), 'apartamento|apto') ~ 'Apartamento',
      str_detect(tolower(title), 'casa') ~ 'Casa',
      str_detect(tolower(title), 'apartaestudio|aparta estudio') ~ 'Apartaestudio',
      str_detect(tolower(title), 'ph|penthouse') ~ 'PH',
      str_detect(tolower(title), 'duplex') ~ 'Duplex',
      TRUE ~ property_type
    )
  )


# Extract if the property has vigilance, common areas, is new, has gym, humidity zones or deposits
train <- train %>%
  mutate(
    vigilance = ifelse(str_detect(tolower(description), 'vigilancia|vigilante'), 1, 0),
    common_areas = ifelse(str_detect(tolower(description), 'zonas comunes|zonas sociales|bbq|salones comunales|salones sociales'), 1, 0),
    gym = ifelse(str_detect(tolower(description), 'gimnasio|gim|gym'), 1, 0),
    humidity_zones = ifelse(str_detect(tolower(description), 'zonas humedas|piscina|jacuzzi'), 1, 0),
    deposit = ifelse(str_detect(tolower(description), 'deposito|depósito|cuarto util'), 1, 0)
  )

# calculate price per m2
train <- train %>% 
  mutate(precio_m2 = round(price / surface_total, 0)) %>% 
  mutate(precio_m2 = precio_m2 / 1000000)

# stargazer(train['precio_m2'], type = 'latex', 
#           out = file.path(dir$views, 'destats_pricem2.tex'))

hist(train$precio_m2)

# work with outliers
p1 <- train %>% 
  ggplot(aes(y = precio_m2)) +
  geom_boxplot(fill = 'darkblue', alpha = 0.4) +
  labs(title = 'Muestra Completa',
       y = 'Precio por metro cuadrado (millones)', x = '') +
  theme_minimal()

# ggsave(filename = file.path(dir$views, 'boxplot_pricem2_complete.png'),
#        plot = p1,
#        width = 8, 
#        height = 6, 
#        dpi = 300)

perc1 <- unname(round(quantile(train$precio_m2, probs = c(0.01)), 2))
up <- round(mean(train$precio_m2) + 2*sd(train$precio_m2))

p2 <- train %>% 
  filter(between(precio_m2, perc1, up)) %>% 
  ggplot(aes(y = precio_m2)) +
  geom_boxplot(fill = 'darkblue', alpha = 0.4) +
  labs(title = 'Muestra Filtrada',
       y = 'Precio por metro cuadrado (millones)', x = '') +
  theme_minimal()

# ggsave(filename = file.path(dir$views, 'boxplot_pricem2_filtered.png'),
#        plot = p2,
#        width = 8, 
#        height = 6, 
#        dpi = 300)

p3 <- grid.arrange(p1, p2, ncol = 2)

# ggsave(filename = file.path(dir$views, 'boxplots_pricem2.png'),
#        plot = p3,
#        width = 8, 
#        height = 6, 
#        dpi = 300)

train <- train %>% 
  filter(between(precio_m2, perc1, up))

plot_price <- ggplot(train, aes(x = log10(price))) +
  geom_histogram(binwidth = 0.05, fill = 'darkblue', alpha = 0.4) +
  scale_x_continuous(labels = function(x) scales::dollar(10^x, accuracy = 1),
                     name = 'Valor de venta (pesos reales, escala log)') +
  labs(y = 'Cantidad') +
  theme_minimal()
ggplotly(plot_price)

# ggsave(filename = file.path(dir$views, 'histogram_price.png'),
#        plot = plot_price,
#        width = 8, 
#        height = 6, 
#        dpi = 300)

# =========================================================
# 2. Exploring and cleaning test data (NAs, outliers)
# =========================================================

# check operation and property types
table(test$operation_type)

test %>%
  count(property_type)

# select only vars that are not constant
test <- test %>% dplyr:: select(-constant_vars)

#check for columns with missing values
colSums(sapply(test, is.na)) > 0

vis_dat(test)
missing_test <- vis_miss(test)
# ggsave(filename = file.path(dir$views, 'vis_miss_plot_test.png'),
#        plot = missing_test,
#        width = 8, 
#        height = 6, 
#        dpi = 300)

test %>% filter(is.na(title)) %>% count() # 6 missing titles

test %>% filter(is.na(description)) %>% count() # 2 missing descriptions

# find mode and median to replace missing data
test %>% 
  count(rooms)

test %>% 
  count(bathrooms)

median_totalsur_test <- median(test$surface_total, na.rm = T)
median_coveredsur_test <- median(test$surface_covered, na.rm = T)

# input missing data with modes and medians
test <- test %>% 
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_total = replace_na(surface_total, median_totalsur_test),
         surface_covered = replace_na(surface_covered, median_coveredsur_test))

# check descriptive statistics of numeric variables
# stargazer(test, type = 'latex', out = file.path(dir$views,
#                                                 'destats_test.tex'))

# create a new column to look for properties with commercial usage (0 bedrooms)
test <- test %>%
  mutate(
    usage_type = case_when(
      str_detect(tolower(description),
                 'bodega|oficina|local|negocio') ~ 'commercial',
      TRUE ~ 'residential'))

# extract number of bedrooms from descriptions
test <- test %>% 
  mutate(
    bedrooms_extracted = case_when(
      usage_type == 'residential' & bedrooms == 0 ~ str_extract(description, 
                                                                '\\d+\\s*(alcoba|habitacion|cuarto)s?'),
      TRUE ~ NA_character_
    ),
    bedrooms_extracted = as.numeric(str_extract(bedrooms_extracted, '\\d+')))

# change bedrooms if a valid number was extracted
test <- test %>% 
  mutate(
    bedrooms = if_else(
      bedrooms == 0 & usage_type == 'residential' & !is.na(bedrooms_extracted),
      bedrooms_extracted,
      bedrooms
    )
  )

# extract number of parking spaces from descriptions
test <- test %>%
  mutate(
    parking_spaces_extracted = case_when(
      usage_type == "residential"  ~ str_extract(description, "\\d+\\s*(parqueadero|garaje)s?"),
      TRUE ~ NA_character_
    ),
    parking_spaces_extracted = as.numeric(str_extract(parking_spaces_extracted, "\\d+"))
  )

# change parking_spaces if a valid number was extracted
test <- test %>% 
  mutate(
    parking_spaces = if_else(
      usage_type == 'residential' & !is.na(parking_spaces_extracted) & 
        parking_spaces_extracted <= 5, #Why imposed that there are no home with more than 5 parking spaces?
      parking_spaces_extracted,
      0
    )
  ) %>% select(-parking_spaces_extracted)

# Extract from the title the specific type of property
test <- test %>%
  mutate(
    property_type_extracted = case_when(
      str_detect(tolower(title), 'apartamento|apto') ~ 'Apartamento',
      str_detect(tolower(title), 'casa') ~ 'Casa',
      str_detect(tolower(title), 'apartaestudio|aparta estudio') ~ 'Apartaestudio',
      str_detect(tolower(title), 'ph|penthouse') ~ 'PH',
      str_detect(tolower(title), 'duplex') ~ 'Duplex',
      TRUE ~ property_type
    )
  )

# Extract if the property has vigilance, common areas, is new, has gym, humidity zones or deposits
test <- test %>%
  mutate(
    vigilance = ifelse(str_detect(tolower(description), 'vigilancia|vigilante'), 1, 0),
    common_areas = ifelse(str_detect(tolower(description), 'zonas comunes|zonas sociales|bbq|salones comunales|salones sociales|terraza'), 1, 0),
    gym = ifelse(str_detect(tolower(description), 'gimnasio|gim|gym'), 1, 0),
    humidity_zones = ifelse(str_detect(tolower(description), 'zonas humedas|piscina|jacuzzi'), 1, 0),
    deposit = ifelse(str_detect(tolower(description), 'deposito|depósito|cuarto util'), 1, 0)
  )

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

map_train <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = lon_central, lat = lat_central, zoom = 11) %>% 
  addCircles(lng = train$lon,
             lat = train$lat,
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             radius = train$precio_m2_sc*10,
             popup = html)

# saveWidget(map_train, file = file.path(dir$views, "interactive_map_train.html"))

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

map_test <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = lon_central_test, lat = lat_central_test, zoom = 13) %>% 
  addCircles(lng = test$lon,
             lat = test$lat,
             col = test$color,
             fillOpacity = 1,
             opacity = 1,
             popup = html_test)

# saveWidget(map_test, file = file.path(dir$views, "interactive_map_test.html"))

# transform data to sf
sf_test <- st_as_sf(test, coords = c('lon', 'lat'), crs = 4326)

# =========================================================
# 5. Adding data of localidades
# =========================================================

localidades <- st_read(file.path(dir$raw, "poligonos-localidades.geojson"))
localidades <- st_transform(localidades, 4326)

# Map without filter chapinero

localidades_train <- ggplot() +
  geom_sf(data = localidades %>% 
            filter(!(Nombre.de.la.localidad %in% c('SUMAPAZ',
                                                   'USME',
                                                   'CIUDAD BOLIVAR'))), 
          color = 'orange') +
  geom_sf(data = sf_train, aes(color = precio_m2), shape = 15, size = 0.3) +
  theme_minimal()


# ggsave(filename = file.path(dir$views, "mapa_localidades_train.png"),
#        plot = localidades_train,
#        width = 8,
#        height = 6,
#        dpi = 300)

# Filter out our locality to predict Chapinero
sf_train <- st_join(sf_train, localidades, join = st_within) 
sf_train <- sf_train %>%
  filter(Nombre.de.la.localidad != 'CHAPINERO')

localidades_train_filter <- ggplot() +
  geom_sf(data = localidades %>% 
            filter(!(Nombre.de.la.localidad %in% c('SUMAPAZ',
                                                   'USME',
                                                   'CIUDAD BOLIVAR'))), 
          color = 'orange') +
  geom_sf(data = sf_train, aes(color = precio_m2), shape = 15, size = 0.3) +
  theme_minimal()

ggsave(filename = file.path(dir$views, "mapa_localidades_train_filter.png"),
       plot = localidades_train_filter,
       width = 8,
       height = 6,
       dpi = 300)

localidades_test <- ggplot() +
  geom_sf(data = localidades %>% 
            filter(!(Nombre.de.la.localidad %in% c('SUMAPAZ',
                                                   'USME',
                                                   'CIUDAD BOLIVAR'))), 
          color = 'orange') +
  geom_sf(data = sf_test, color = 'blue', shape = 15, size = 0.3) +
  theme_minimal()

# ggsave(filename = file.path(dir$views, "mapa_localidades_test.png"),
#        plot = localidades_test,
#        width = 8,
#        height = 6,
#        dpi = 300)


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

# same process but now on test

# calculate distances between each property and nearest park
dist_matrix_test <- st_distance(x = sf_test, y = centroides_sf)
dim(dist_matrix_test)

# find min distance to any park for each property
dist_min_test <- apply(dist_matrix_test, 1, min)
test <- test %>%
  mutate(distancia_parque = dist_min_test)

# check distribution 
plot_parks_test <- ggplot(test, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a un parque en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a los parques') +
  theme_minimal()
ggplotly(plot_parks_test)

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

# same process but now on test

# calculate distances between each property and nearest station
dist_matrix_pt_test <- st_distance(x = sf_test, y = centroides_pt_sf)
dim(dist_matrix_pt_test)

# find min distance to any station for each property
dist_min_pt_test <- apply(dist_matrix_pt_test, 1, min)
test <- test %>%
  mutate(distancia_estaciones = dist_min_pt_test)

# check distribution 
plot_estaciones_test <- ggplot(test, aes(x = distancia_estaciones)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a una estación de transporte público en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a las estaciones') +
  theme_minimal()
ggplotly(plot_estaciones_test)

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

# same process but now on test

# calculate distances between each property and nearest park
dist_matrix_mall_test <- st_distance(x = sf_test, y = centroides_mall_sf)
dim(dist_matrix_mall_test)

# find min distance to any park for each property
dist_min_mall_test <- apply(dist_matrix_mall_test, 1, min)
test <- test %>%
  mutate(distancia_mall = dist_min_mall_test)

# check distribution 
plot_malls_test <- ggplot(test, aes(x = distancia_mall)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a un mall en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a los malls') +
  theme_minimal()
ggplotly(plot_malls_test)

# ==================== universities ============================

amenity <- osmdata::available_tags('amenity')
print(amenity, n=137)

# extract info of universities
unis <- opq(bbox = getbb('Bogota Colombia')) %>%
  add_osm_feature(key = 'amenity', value = 'university') 

# transform data into sf
unis_sf <- osmdata_sf(unis)

# select polygons and save them
unis_geom <- unis_sf$osm_polygons %>%
  dplyr::select(osm_id, name)
unis_geom <- st_as_sf(unis_sf$osm_polygons)

# calculate each centroid (queremos trabajar con centroides u otra cosa?)
centroides_unis <- st_centroid(unis_geom, byid = T)
centroides_unis <- centroides_unis %>%
  mutate(x = st_coordinates(centroides_unis)[, 'X']) %>%
  mutate(y = st_coordinates(centroides_unis)[, 'Y'])

centroides_uni_sf <- st_as_sf(centroides_unis, coords = c('x', 'y'), crs = 4326)

# calculate distances between each property and nearest university
dist_matrix_unis <- st_distance(x = sf_train, y = centroides_uni_sf)
dim(dist_matrix_unis)

# find min distance to any park for each property
dist_min_unis <- apply(dist_matrix_unis, 1, min)
train <- train %>%
  mutate(distancia_unis = dist_min_unis)

# check distribution
plot_unis <- ggplot(train, aes(x = distancia_unis)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a una universidad en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a las universidades') +
  theme_minimal()
ggplotly(plot_unis)

# same process but now on test

# calculate distances between each property and nearest university
dist_matrix_unis_test <- st_distance(x = sf_test, y = centroides_uni_sf)
dim(dist_matrix_unis_test)

# find min distance to any park for each property
dist_min_unis_test <- apply(dist_matrix_unis_test, 1, min)
test <- test %>%
  mutate(distancia_unis = dist_min_unis_test)

# check distribution
plot_unis_test <- ggplot(test, aes(x = distancia_unis)) +
  geom_histogram(bins = 50, fill = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a una universidad en metros',
       y = 'Cantidad',
       title = 'Distribución de la distancia a las universidades') +
  theme_minimal()
ggplotly(plot_unis_test)

# =========================================================
# 7. Adding spatial data from Alcaldía Mayor de Bogotá
# =========================================================

# Uploading stratification data
estratos <- st_read(file.path(dir$raw, "ManzanaEstratificacion.shp"))
st_crs(estratos) <- 4686
estratos <- st_make_valid(estratos)
sf_train <- st_transform(sf_train, 4686)

# Joining sf_train and stratification data
sf_train$estrato <- st_join(sf_train, estratos)$ESTRATO

#Turning stratum 0 to NA
sf_train$estrato[sf_train$estrato==0] <- NA

#The next code is going to set new values for NA based on close neighboors

# Values with and without defined data
con_estrato <- sf_train %>% filter(!is.na(estrato))
sin_estrato <- sf_train %>% filter(is.na(estrato))

# Extracting their coordinates
coords_con <- st_coordinates(con_estrato)
coords_sin <- st_coordinates(sin_estrato)

nn <- get.knnx(coords_con, coords_sin, k = 3)

# Getting neighboors data
vecinos_estratos <- apply(nn$nn.index, 1, function(idx) {
  vecinos <- con_estrato$estrato[idx]
  # Most frequent value
  names(sort(table(vecinos), decreasing = TRUE))[1]
})

# Setting new values
sf_train$estrato[is.na(sf_train$estrato)] <- vecinos_estratos

# Adding the same column for train
train$estrato <- sf_train$estrato

# Same process but now on test

estratos <- st_read(file.path(dir$raw, "ManzanaEstratificacion.shp"))
st_crs(estratos) <- 4686
estratos <- st_make_valid(estratos)

sf_test <- st_transform(sf_test, 4686)

sf_test$estrato <- st_join(sf_test, estratos)$ESTRATO

sf_test$estrato[sf_test$estrato == 0] <- NA

con_estrato <- sf_test %>% filter(!is.na(estrato))
sin_estrato <- sf_test %>% filter(is.na(estrato))

coords_con <- st_coordinates(con_estrato)
coords_sin <- st_coordinates(sin_estrato)

nn <- get.knnx(coords_con, coords_sin, k = 3)

vecinos_estratos <- apply(nn$nn.index, 1, function(idx) {
  vecinos <- con_estrato$estrato[idx]
  names(sort(table(vecinos), decreasing = TRUE))[1]
})

sf_test$estrato[is.na(sf_test$estrato)] <- vecinos_estratos

test$estrato <- sf_test$estrato


# Plotting
ggplot() +
  geom_sf(data = estratos, aes(fill = as.factor(ESTRATO)), color = NA) +
  scale_fill_brewer(palette = "YlOrRd", name = "Estrato") +
  theme_minimal() +
  labs(title = "Distribución de estratos socioeconómicos en Bogotá") +
  theme(legend.position = "right")


# ===============================================================
# 8. Checking the relationship between price and parks
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

# same on test
posicion_test <- apply(dist_matrix_test, 1, function(x) which(min(x) == x))
areas_test <- st_area(parques_geom)
test <- test %>%
  mutate(area_parque = as.numeric(areas_test[posicion_test]))

# ===============================================================
# 9. Checking the relationship between price and stations
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
# 10. Checking the relationship between price and malls
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

# same on test
posicion_mall_test <- apply(dist_matrix_mall_test, 1, function(x) which(min(x) == x))

areas1_test <- st_area(malls_geom)
test <- test %>%
  mutate(area_mall = as.numeric(areas1_test[posicion_mall_test]))

# ===============================================================
# 11. Checking the relationship between price and universities
# ===============================================================

# distance
price_unis <- ggplot(train %>% sample_n(1000), aes(x = distancia_unis,
                                                       y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Distancia mínima a una universidad en metros (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre la proximidad a una universidad y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_unis)

# area
posicion_unis <- apply(dist_matrix_unis, 1, function(x) which(min(x) == x))

areas1 <- st_area(unis_geom)
train <- train %>%
  mutate(area_unis = as.numeric(areas1[posicion_unis]))


price_unis <- ggplot(train %>% sample_n(1000), aes(x = area_unis,
                                                     y = price)) +
  geom_point(col = 'darkblue', alpha = 0.4) +
  labs(x = 'Área de la universidad más cercano (log-scale)',
       y = 'Valor de venta (log-scale)',
       title = 'Relación entre área de una universidad y el precio del inmueble') +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_minimal()
ggplotly(price_unis)

# same on test
posicion_unis_test <- apply(dist_matrix_unis_test, 1, function(x) which(min(x) == x))

areas2_test <- st_area(unis_geom)
test <- test %>%
  mutate(area_unis = as.numeric(areas2_test[posicion_unis_test]))


saveRDS(train, file.path(dir$processed, paste0("train_clean", ".rds")))
saveRDS(test, file.path(dir$processed, paste0("test_clean", ".rds")))
