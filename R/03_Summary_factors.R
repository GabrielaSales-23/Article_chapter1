# Frequency of Biomes and Climate zone ------------------------------------
library(ggplot2)

biomes <- df %>% count(Biome)
names(biomes)[1] <- "Biome"
View(biomes)
biomes <- subset(biomes, n>2)
print(biomes)

ggplot(biomes, aes(x = Biome, y = n, fill = Biome)) +
  geom_bar(stat = "identity") +
  labs(x = "Biome", y = "Frequency") +
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  coord_flip()
ggsave("Outputs/Figures/exploration/Biome.png",height = 12,width = 8, units = "in",dpi = 300)


Climate <- df %>% count(Climate)
names(Climate)[1] <- "Zone"
View(Climate)
print(Climate)

ggplot(Climate, aes(x = Zone, y = n, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(x = "Climate", y = "Frequency") +
  theme_minimal()+
  theme(axis.text.x = element_blank())
ggsave("Outputs/Figures/exploration/ClimateZone.png",height = 12,width = 8, units = "in",dpi = 300)



# Frequency of Restoratio type --------------------------------------------

Restoration <- df %>% count(Restoration_type)
names(Restoration)[1] <- "Type"
View(Restoration)
print(Restoration)

library(viridis)
library(tidyverse)
Restoration <- Restoration |> mutate(Type = fct_reorder(Type, n, .desc = TRUE))
ggplot(Restoration, aes(x = Type, y = n, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(x = "Restoration Type", y = "Frequency") +
  scale_fill_viridis_d()+
  theme_minimal()+
  theme(axis.text.x = element_blank())
ggsave("Outputs/Figures/exploration/RstoratioType.png",height = 12,width = 8, units = "in",dpi = 300)


# Frequency of crediting system -------------------------------------------

CS <- df %>% count(Funding)
print(CS)
ggplot(CS, aes(x = Funding, y = n, fill = Funding)) +
  geom_bar(stat = "identity") +
  labs(x = "Crediting system", y = "Frequency") +
  theme_minimal()+
  theme(axis.text.x = element_blank())


# Frequency of social and biodiversity actions ----------------------------

Social <- df %>%  count(df$Social_actions)
print(Social)
Biodiversity <- df %>% count(df$Biodiversity_actions)
print(Biodiversity)

count_actions <- table(df$Social_actions, df$Biodiversity_actions)

count_df_actions <- as.data.frame(count_actions)
colnames(count_df_actions) <- c("Social actions", "Biodiversity actions", "n")


# Frequency of Biodiversity monitoring ------------------------------------

CCB <- df %>% filter(Funding == "VCS and CCB")
Monitoring <- CCB %>% count(CCB$Biodiversity_monitoring)
print(Monitoring)


# Frequency of Post Land Use ----------------------------------------------
Land_use <- df %>% count(df$Post_land_use)
print(Land_use)

# Frequency of each Land owner --------------------------------------------
Land_owner <- df %>% count(df$Land_owner)
print(Land_owner)



# Frequency of each Credit owner ------------------------------------------
Credit_owner <- df %>% count(df$Profit_owner)
print(Credit_owner)
View(Credit_owner)


# Frequency of each Plant origin ------------------------------------------
Origin <- df %>% count(df$Plant_origin)
View(Origin)
