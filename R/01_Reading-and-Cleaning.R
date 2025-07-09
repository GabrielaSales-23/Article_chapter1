## Reading ##

data <- read.csv("Data/Processed/Projects_info_V06.csv", sep = ";", fileEncoding = "latin1")

View(data)

## Cleaning ##

library(dplyr)
head(data)

df <- data %>% filter(Filter.suitability.=="Yes")  
head(df)
nrow(df)  #114 projects fit the criteria

df <- df %>% select(-Carimbo.de.data.hora, -Endereço.de.e.mail, -If.no..why., -Filter.suitability.)
colnames(df)

# Verification of false double levels

df$Country <- as.factor(df$Country)
levels(df$Country)
library(stringr)
df <- df %>%
  mutate(Country = if_else(str_detect(Country, "(?i)laos|lao"), "Laos", Country))
df <- df %>%
  mutate(Country = if_else(str_detect(Country, "Republic of Panama"), "Panama", Country))
df <- df %>%
  mutate(Country = if_else(str_detect(Country, "Paraguay"), "Paraguay", Country))
df <- df %>%
  mutate(Country = if_else(str_detect(Country, "Senegal"), "Senegal", Country))
df <- df %>%
  mutate(Country = if_else(str_detect(Country, "Tanzania"), "Tanzania", Country))
df <- df %>%
  mutate(Country = if_else(str_detect(Country, "United"), "USA", Country))

df$Continent <- as.factor(df$Continent)
levels(df$Continent)
df <- df %>%
  mutate(Continent = if_else(str_detect(Continent, "(?i)africa"), "Africa", Continent))
df <- df %>%
  mutate(Continent = if_else(str_detect(Continent, "(?i)South"), "South America", Continent))

df$Climate <- as.factor(df$Climate)
levels(df$Climate)
df <- df %>%
  mutate(Climate = if_else(str_detect(Climate, "(?i)Tropical mo"), "Tropical monsoon", Climate))
df <- df %>%
  mutate(Climate = if_else(str_detect(Climate, "(?i)Tropical se"), "Tropical, Semi-arid", Climate))
df <- df %>%
  mutate(Climate = if_else(str_detect(Climate, "(?i)Tropical "), "Tropical", Climate))

# 
# rows_with_x <- which(df$Climate == "Tropical and Subtropical Grasslands, Savannas and Shrublands")
# df[72,]
# df <- df %>%
#   mutate(Climate = if_else(str_detect(Climate, "(?i)Tropical and Subtropical Grasslands, Savannas and Shrublands"), "Tropical", Climate))

df <- df %>% 
  mutate(Funding = if_else(str_detect(Funding, "(?i)Carbon credits under VCS, Carbon credits under CCB"), "VCS and CCB", Funding))
df <- df %>% 
  mutate(Funding = if_else(str_detect(Funding, "(?i)Carbon credits under VCS"), "VCS",  Funding))
df <- df %>% 
  mutate(Funding = if_else(str_detect(Funding, "(?i)Carbon credits under CCB"), "VCS and CCB",  Funding))
df <- df %>%
    mutate(across(c(Country, Continent, Biome, Climate, Land_owner, Biomass, Year, Profit_owner, Funding, Plant_origin, Social_actions, Biodiversity_actions, Biodiversity_variables, Success_by_biod, Biodiversity_monitoring, Post_land_use, Restoration_type), as.factor))


df <- df %>%
  mutate(Restoration_type = if_else(str_detect(Restoration_type, "Commercial"), "Commercial reforestation", Restoration_type))
