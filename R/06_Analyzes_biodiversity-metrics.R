# Biodiversity variables -----------------------------------------------
CCB <- df |> filter(Funding == "VCS and CCB")

df_bio <- df %>%
  separate_rows(Biodiversity_variables, sep = ",\\s*") ## putting each specie in one line
View(df_bio)
CCB <- df_bio |> filter(Funding == "VCS and CCB")
View(CCB)
bio_var <- df_bio %>% count(df_bio$Biodiversity_variables)
colnames(bio_var)[1] <- "Biodiversity_variables"
print(bio_var)
View(bio_var)
library(openxlsx)
write.xlsx(bio_var, file = "Outputs/Tables/Biodiversity_variables_counting.xlsx")


bio_var_funding <- df_bio %>% count(df_bio$Biodiversity_variables, df_bio$Funding)
View(bio_var_funding)
