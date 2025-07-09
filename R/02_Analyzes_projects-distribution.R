### Analyzes ###


# Projects distribution in the world --------------------------------------

# Sum countries' projects
library(dplyr)
country_count <- df %>% count(Country)
country_count <- country_count %>%
  rename(region = Country)
country_count <- country_count[1:35,]
View(country_count)

# Ploting the world map
library(ggplot2)
library(wordcloud)
library(maps)

mapdata = map_data("world")
str(mapdata)
View(mapdata)

# identifying the countries names in mapdatas
mapdataNames <- as.factor(mapdata$region)
levels(mapdataNames)

# joing the objects
country_count <- country_count %>%
  mutate(region = as.character(region))

mapdata = left_join(mapdata, country_count, by="region")
View(mapdata)



##Establishing climate zones
# Carregar pacotes necessários
#install.packages("terra")
#install.packages(c("sf", "rnaturalearth", "dplyr"))
#install.packages("rnaturalearthdata")
library(terra)
library(sf)
library(rnaturalearthdata)
library(dplyr)
library(viridis)
library(ggpattern)
library(scales)

# Transformar o 'mapdata' em um objeto 'sf' para cálculo do centroide
centroides <- st_as_sf(mapdata, coords = c("long", "lat"), crs = 4326, agr = "constant")

# Extrair as coordenadas dos centroides (longitude e latitude)
centroid_coords <- st_coordinates(centroides)

# Adicionar as coordenadas do centroide ao 'mapdata'
mapdata <- mapdata %>%
  mutate(centroid_lon = centroid_coords[, 1],  # Longitude do centroide
         centroid_lat = centroid_coords[, 2])  # Latitude do centroide

# Classificação das zonas climáticas com base na latitude do centroide
mapdata <- mapdata %>%
  mutate(climate_zone = case_when(
    centroid_lat >= -23.5 & centroid_lat <= 23.5 ~ "tropical",  # Zona tropical
    (centroid_lat > 23.5 & centroid_lat <= 40) | (centroid_lat < -23.5 & centroid_lat >= -40) ~ "subtropical",  # Zona subtropical
    (centroid_lat > 40 & centroid_lat <= 60) | (centroid_lat < -40 & centroid_lat >= -60) ~ "temperate",  # Zona temperada
    TRUE ~ "cold"  # Zona fria
  ))
names(mapdata)
View(mapdata)

# Adding a value 'n' according to climate zone
mapdata <- mapdata %>%
  mutate(n = case_when(
    !is.na(n) & n != 0 ~ n,  # if n is not NA or 0, keep the value 'n'
    is.na(n) & climate_zone %in% c("tropical", "subtropical") ~ 0L,  # if n is not NA and climate zone is tropical or subtropical, put 0
    is.na(n) & climate_zone %in% c("temperate", "cold") ~ NA_integer_,  # if n is NA or the climate zone is temperate or cold, put NA
    TRUE ~ n  # Keep value 'n' if it is any of the above options
  ))

#Ploting

# Separating data
map_na <- mapdata %>% filter(is.na(n))
map_not_na <- mapdata %>% filter(!is.na(n))

# Verifying color vector
if (!exists("cores")) {
  cores <- viridis::viridis(length(unique(na.omit(mapdata$n))))
}

# Graphic
ggplot() +
  # Countries with NA - gray
  geom_polygon(data = map_na,
               aes(x = long, y = lat, group = group),
               fill = alpha("gray", 0.05),
               color = "black", linewidth = 0.2) +

  # Countries with n different from NA
  geom_polygon(data = map_not_na,
               aes(x = long, y = lat, group = group, fill = factor(n)),
               color = "black", linewidth = 0.2) +

  # Coutrines with NA - striped pattern
  ggpattern::geom_polygon_pattern(data = map_na,
                                  aes(x = long, y = lat, group = group),
                                  color = "black", linewidth = 0.2,
                                  pattern = "stripe",
                                  pattern_fill = "gray",
                                  pattern_density = 0.1,
                                  pattern_spacing = 0.02) +

  scale_fill_manual(name = "Projects",
                    values = cores,
                    guide = guide_legend(title.position = "top")) +
  coord_quickmap() +
  theme_void() +
  theme(legend.title = element_text(hjust = 0.5),
        legend.box.just = "center")

ggsave("Outputs/Figures/mapCorrect5.png",height = 12,width = 8, units = "in",dpi = 300)
