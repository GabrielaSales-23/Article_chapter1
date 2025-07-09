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

#mapdata1 = mapdata %>% filter(n >1)

#Ploting 
# mapdata %>%
#   mutate(n = if_else(is.na(n), 0L, n)) %>%
#   ggplot(aes(x=long, y=lat, group=group,fill=n))+
#   geom_polygon(color="black",linewidth=0.2)+
#   scale_fill_gradient(name= "Projects",
#                       low="#FFFFFF", high = "#FF0000",
#                       limits=c(0,10),
#                       guide = guide_colourbar(title.position = "top"))+
#   
#   coord_quickmap()+
#   theme_void()+
#   theme(legend.title.align = 0.5,
#         legend.box.just = "center")
# ggsave("Figures/map.png",height = 12,width = 8, units = "in",dpi = 300)

library(viridis)
library(ggpattern)
library(scales)

##Establishing climate zones
# Carregar pacotes necessários
#install.packages("terra")
#install.packages(c("sf", "rnaturalearth", "dplyr"))
#install.packages("rnaturalearthdata")
library(terra)
library(sf)
library(rnaturalearthdata)
library(dplyr)

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

# Atribuindo valor de 'n' conforme a zona climática
mapdata <- mapdata %>%
  mutate(n = case_when(
    !is.na(n) & n != 0 ~ n,  # Se n não for NA e não for 0, mantém o valor de 'n'
    is.na(n) & climate_zone %in% c("tropical", "subtropical") ~ 0L,  # Se n for NA e a zona for tropical ou subtropical, atribui 0
    is.na(n) & climate_zone %in% c("temperate", "cold") ~ NA_integer_,  # Se n for NA e a zona for temperada ou fria, atribui NA
    TRUE ~ n  # Mantém o valor de 'n' caso não se encaixe em nenhuma condição acima
  ))

#Ploting

# Separando dados
map_na <- mapdata %>% filter(is.na(n))
map_not_na <- mapdata %>% filter(!is.na(n))

# Verificando o vetor de cores
if (!exists("cores")) {
  cores <- viridis::viridis(length(unique(na.omit(mapdata$n))))
}

# Gráfico
ggplot() +
  # Países com NA - fundo cinza
  geom_polygon(data = map_na,
               aes(x = long, y = lat, group = group),
               fill = alpha("gray", 0.05),
               color = "black", linewidth = 0.2) +
  
  # Países com dados
  geom_polygon(data = map_not_na,
               aes(x = long, y = lat, group = group, fill = factor(n)),
               color = "black", linewidth = 0.2) +
  
  # Países com NA - padrão listrado
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

# # Acumulation curve -------------------------------------------------------
# 
# colnames(df)
# New_data <- data.frame(df[,c("Year","Country")])
# plot(New_data)
# 
# library(dplyr)
# library(tidyr)
# 
# New_data_resumo <- New_data %>%
#   group_by(Country, Year) %>%
#   summarise(n_projects = n(), .groups = "drop")
# 
# New_data_acumulado <- New_data %>%
#   group_by(Country, Year) %>%
#   summarise(n_projects = n(), .groups = "drop") %>%
#   arrange(Country, Year) %>%
#   group_by(Country) %>%
#   mutate(cumulative_projects = cumsum(n_projects))
# 
# 
# library(ggplot2)
# 
# ggplot(New_data_acumulado, aes(x = Year, y = cumulative_projects, color = Country, group = Country)) +
#   geom_line(linewidth = 1) +
#   geom_point() +
#   labs(
#     x = "Ano",
#     y = "Projetos acumulados",
#     title = "Número acumulado de projetos por país ao longo do tempo"
#   ) +
#   theme_classic()