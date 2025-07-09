
# Separating a scientific name by lines -----------------------------------

library(tidyr)
#sp_df <- data.frame(species_names = df_C$Species_name) #transforming the column in a data frame
sp_df <- df %>%
  separate_rows(Species_name, sep = ",\\s*") ## putting each specie in one line
View(sp_df)
library(openxlsx)
write.xlsx(sp_df, file = "Data/Processed/species_name_original_data.xlsx")


# Removing duplicates -----------------------------------------------------
sp_df %>%
  count(across(everything())) %>%
  filter(n > 1)

sp_df <- sp_df %>%
  distinct()

# Cleaning species name ----------------------------------------------
install.packages("remotes")
remotes::install_github("brunobrr/bdc")

library(bdc)
query_names <- bdc_query_names_taxadb(
  sci_name            = sp_df$Species_name,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = FALSE, # should parallel processing be used?
  ncores              = 2, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)

teste <- query_names %>%  filter(is.na(scientificName))
View(teste)

names(query_names)[1]<- "Species_name2"
nrow(sp_df)
sp_df_GBIF <- cbind(sp_df, query_names[, c("taxonID", "scientificName", "Species_name2")])
nrow(sp_df_GBIF)


all(sp_df_GBIF$Species_name == sp_df_GBIF$Species_name2)

sp_df_GBIF <- sp_df_GBIF %>%
  mutate(Species_name = ifelse(is.na(taxonID), Species_name, scientificName)) %>%
  select(-scientificName, -taxonID, -Species_name2)
View(sp_df_GBIF)

#write.table(sp_df_GBIF, file = "Outputs/Tables/Corrected_species_names_bdc.txt")

sp_df_GBIF<- read.table("Outputs/Tables/Corrected_species_names_bdc.txt", sep = "")
View(sp_df_GBIF)
sum(is.na(sp_df_GBIF$Species_name))

##Counting
sp <- sp_df_GBIF %>% count(Species_name)
nrow(sp)
View(sp)

library(openxlsx)
# write.xlsx(sp, file = "Outputs/Tables/Species_counting_bdc.xlsx")


# Number of species by project --------------------------------------------
library(tidyr)
species_number <- sp_df_GBIF %>% count(ID)
nrow(species_number)


Monoculture <- species_number %>% filter(n == "1")
Monoculture$culture_system <- "Monoculture"
View(Monoculture)
Two_species <- species_number %>% filter(n == "2")
Two_species$culture_system <- "Two species"
View(Duas)
Mixed_planting <- species_number %>% filter(n > 2)
Mixed_planting$culture_system <- "Mixed planting"
View(Mixed_planting)

Culture_system <- rbind(Monoculture, Two_species, Mixed_planting)
Number <- Culture_system %>% count(culture_system)
View(Number)
names(Number)[1] <- "Planting_system"
df <- left_join(df, Culture_system, by="ID")


# Frequency of planting system --------------------------------------------
ggplot(Number, aes(x = Planting_system, y = n, fill = Planting_system)) +
  geom_bar(stat = "identity") +
  labs(x = "Planting system", y = "Frequency", fill = "Planting system") +
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_blank())
ggsave("Outputs/Figures/exploration/PlantingSystem.png",height = 12,width = 8, units = "in",dpi = 300)

sp_df_GBIF <- left_join(sp_df_GBIF, Culture_system, by="ID")
Mono <- sp_df_GBIF %>% filter(culture_system == "Monoculture")
View(Mono)
TwoS <- sp_df_GBIF %>% filter(culture_system == "Two species")
View(TwoS)
Mix <- sp_df_GBIF %>% filter(culture_system == "Mixed planting")
View(Mix)
NumberMix <- Mix %>% count(Species_name)
View(NumberMix)

CasuarinaEquisetifolia  <- sp_df_GBIF %>% filter(Species_name == "Casuarina equisetifolia")
View(CasuarinaEquisetifolia)
