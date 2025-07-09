
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

# #using a global packge
# # install.packages("taxize")
# library(taxize)
# sources <- gnr_datasources()
# View(sources)
# data_base <- sources$id[sources$title == 'GBIF Backbone Taxonomy']
# 
# # #testing with a small number of species names
# # small_test_list <- head(species_list, 10)
# # results <- gnr_resolve(sci = small_test_list, data_source_ids = 11)
# # View(results)
# 
# # #trying to select only one match for supplied name
# # resolve_names <- function(names_list, data_source_ids) {
# #   resolved_results <- gnr_resolve(sci = names_list, data_source_ids = data_source_ids)
# #   
# #   resolved_results <- resolved_results %>%
# #     group_by(user_supplied_name) %>%
# #     arrange(desc(score)) %>%
# #     slice(1) # Seleciona a melhor correspondência, ajustável conforme necessário
# #   
# #   return(resolved_results)
# # }
# # resolved_names <- resolve_names(small_test_list, 11)
# # print(resolved_names)
# 
# #trying to apply for the hole data frame
# species_list <- sp_df_C$Species_name
# resolved_names <- resolve_names(species_list, 11)
# View(resolved_names)
# 
# #putting the correct names on the original table
# names(resolved_names)[2]<- "Species_name"
# sp_df_C_GBIF <- left_join(sp_df_C, resolved_names[,2:3], by="Species_name", relationship =
#                             "many-to-many")
# View(sp_df_C_GBIF)
# 
# sp_df_C_GBIF <- sp_df_C %>%
#   left_join(resolved_names, by = "Species_name")
# sp_df_C_GBIF <- sp_df_C_GBIF %>%
#   mutate(Species_name = ifelse(is.na(matched_name), Species_name, matched_name)) %>%
#   select(-matched_name, -data_source_title, -score, -user_supplied_name)
# 
# write.table(sp_df_C_GBIF, file = "Outputs/Tables/Corrected_species_names.txt")
# 
# sp_df_C_GBIF<- read.table("Outputs/Tables/Corrected_species_names.txt", sep = "")
# 
# ## Counting the number of each specie frequency
# sp <- sp_df_C_GBIF %>% count(Species_name)
# View(sp)
# library(openxlsx)
# write.xlsx(sp, file = "Outputs/Tables/Species_counting.xlsx")


# #Graphic for presentation
# library(tm)
# library(wordcloud)
# 
# ## Species planted
# corpus <- Corpus(VectorSource(sp_df_C_GBIF$Species_name))
# 
# # Pre-processing 
# corpus <- tm_map(corpus, content_transformer(tolower))  # Converter para minúsculas
# corpus <- tm_map(corpus, removePunctuation)            # Remover pontuação
# corpus <- tm_map(corpus, removeNumbers)                # Remover números
# corpus <- tm_map(corpus, removeWords, stopwords("english"))  # Remover stopwords em inglês
# 
# 
# 
# ## bionomial analysis of planted species
# # Criar um vetor de palavras
# palavras <- unlist(strsplit(sapply(corpus, as.character), " "))
# # Identificar bigramas
# bigramas <- table(toupper(paste(head(palavras, -1), tail(palavras, -1))))
# # Visualizar os bigramas
# head(sort(bigramas, decreasing = TRUE), 15)
# 
# # Converter bigramas para um formato adequado para wordcloud
# palavras_bigramas <- names(bigramas)
# freq_bigramas <- as.numeric(bigramas)
# pf <- data.frame(palavras_bigramas, freq_bigramas) # visualize data in a table
# pf2 <- pf %>%  filter(pf$freq_bigramas > 2)
# View(pf2)
# 
# # Criar nuvem de palavras com bigramas
# wordcloud(words = palavras_bigramas, freq = freq_bigramas, min.freq = 4, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))







# # cleaning species names -----------------------------------------------
# sp_df$Species_name <- sub("^((\\w+\\s+\\w+)).*", "\\1", sp_df$Species_name)
# 
# sp_df$Species_name <- sapply(sp_df$Species_name, function(nome) {
#   # Divide o nome em palavras
#   palavras <- unlist(strsplit(nome, " "))
#   # Se houver ao menos duas palavras, mantém as duas primeiras e ajusta a segunda para minúsculas
#   if (length(palavras) >= 2) {
#     palavras[2] <- tolower(palavras[2])
#     # Junta novamente as duas primeiras palavras
#     paste(palavras[1], palavras[2])
#   } else {
#     # Se houver apenas uma palavra, retorna como está
#     palavras[1]
#   }
# })
# 
# sum(is.na(sp_df$Species_name))

# bdc ---------------------------------------------------------------------
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
