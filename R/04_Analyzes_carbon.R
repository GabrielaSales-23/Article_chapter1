View(df)


# Exploring data ----------------------------------------------------------

## Carbon removing vs Area

df$Area <- trimws(df$Area) # removing spaces
df$Carbon <- trimws(df$Carbon)
df_C <- df %>% filter(Area != "Uninformed")
df_C <- df_C %>% filter(Area != ">50.000")
df_C$Area <- gsub(",", "", df_C$Area) #removing "," as hundreds separator
df_C$Carbon <- gsub(",", "", df_C$Carbon)
df_C$Duration <- gsub("2035", "20", df_C$Duration) #correction in manual data error
df_C$Year <- gsub("204","2004", df_C$Year)
df_C <- df_C %>%
  mutate(across(c(Area, Carbon, Duration), as.numeric))
df_C <- df_C %>%
  mutate(across(c(Year), as.factor))

View(df_C)
options(scipen = 999) # view numbers in a complete way (not scientific notation)
plot(df_C$Carbon)
plot(Carbon~Area, data=df_C)
hist(df_C$Carbon)
hist(df_C$Area)

# excluding the highest number to better visualize the data distribution
teste <- df_C %>% filter(df_C$Carbon < 100000)
teste <- df_C %>% filter(df_C$Area < 100000)
plot(teste$Carbon)
hist(teste$Carbon, nclass = 10)# Assimetric distribution
hist(teste$Area, nclass = 20)
plot(Carbon~Area, data=teste)

# testing log transformation
df_C$area_log <- log(df_C$Area)
df_C$carbon_log <- log(df_C$Carbon)
plot(log(df_C$Carbon))
hist(log(df_C$Carbon))
hist(log(df_C$Area))
plot(log(Carbon)~log(Area), data = df_C, xlab = "Area", ylab = "Carbon removal")

## Avarage
median(df_C$Area)
max(df_C$Area)
min(df_C$Area)
median(df_C$Carbon)
max(df_C$Carbon)
min(df_C$Carbon)
mean(df_C$Carbon/df_C$Area)

## Carbon removing vs Duration
plot(Duration~Area, data=df_C)
plot(Carbon~Duration, data = df_C)
plot(log(df_C$Carbon)~log(df_C$Duration))


# Data transformations ----------------------------------------------------

df_C$area_log <- log(df_C$Area)
df_C$carbon_log <- log(df_C$Carbon)
df_C$Duration_log <- log(df_C$Duration)


# Correlation test --------------------------------------------------------

# Quantitative variables
cor(df_C[, sapply(df_C, is.numeric)])
cor(df_C[, sapply(df_C, is.numeric)])

str(df_C)

# Qualitative variables
df_C_qual <- df_C[,c("Biome","Climate","Biomass","Funding","Plant_origin","Social_actions","Biodiversity_actions","Restoration_type")]
qualitativas <- sapply(df_C_qual, is.factor)

# Obter todas as combinações de variáveis qualitativas
combinacoes <- combn(names(df_C_qual)[qualitativas], 2, simplify = FALSE)

# Aplicar o teste de qui-quadrado para cada par e armazenar os resultados
resultados_todos <- lapply(combinacoes, function(par) {
  tabela <- table(df_C_qual[[par[1]]], df_C[[par[2]]])
  teste <- chisq.test(tabela)

  # Retornar os resultados (incluindo todos os testes)
  return(data.frame(Variavel1 = par[1],
                    Variavel2 = par[2],
                    X_squared = round(teste$statistic, 3),  # Valor do chi-quadrado
                    P_value = round(teste$p.value, 3)))    # Valor p
})

# Combinar os resultados em um único data.frame
tabela_resultados <- do.call(rbind, resultados_todos)
View(tabela_resultados)

# Carregar pacotes necessários
library(gridExtra)
library(knitr)

# Exibir a tabela de resultados com knitr::kable ou gridExtra
kable(tabela_resultados, caption = "Resultados de Todos os Testes de Qui-Quadrado")
View(tabela_resultados)

library(openxlsx)
write.xlsx(tabela_resultados, file = "Outputs/Tables/Correlation_test.xlsx")

# Modeling Carbon credits -------------------------------------------------

##Trying a total model with only carbon as dependent variable
library(lme4)
library(MuMIn)
library(car)

Realglobal_m <- glm(carbon_log ~ area_log+Duration_log+Funding+Restoration_type+Biodiversity_actions, data = df_C, family = gaussian,  na.action = na.fail)
modelosT <- dredge(Realglobal_m)
modelosT
write.xlsx(modelosT, file = "Data/Processed/Model_selection.xlsx")

library(car)
vif(Realglobal_m)

better_model <- glm(carbon_log ~ area_log+Funding, data = df_C, family = gaussian,  na.action = na.fail)
anova(better_model)
summary(better_model)
sum(resid(better_model)^2)
plot(better_model)

vif(better_model)

#install.packages("relaimpo")
library(relaimpo)
# Calcular a importância relativa das variáveis
importancia <- calc.relimp(better_model, type = "lmg", rela = TRUE)
# Exibir os resultados
print(importancia)


##Ploting
library(visreg)
library(ggplot2)
library(cowplot)

par(mfrow = c(1, 2))
# Criar gráficos individuais com visreg e ggplot2
grafico_a <- visreg(better_model, "area_log", xlab = "Area (log)", ylab = "Carbon (log)", gg = TRUE) +
  theme_classic() +  # Aplicar um tema com fundo branco e contorno
  theme(axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA),  # Contorno preto
        panel.grid = element_blank())  # Remover a grade

grafico_b <- visreg(better_model, "Funding", xlab = "Cedit System", ylab = " ", gg = TRUE)+
  theme_classic() +  # Aplicar um tema com fundo branco e contorno
  theme(axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA),  # Contorno preto
        panel.grid = element_blank())  # Remover a grade

grafico <- plot_grid(grafico_a, grafico_b, ncol = 2, labels = c("a", "b"))
grafico
x11()

save_plot("Figures/Fig2.Carbon_model.png", grafico)
save

# # ## Trying a total model with carbon/area dependent variable
# df_C$Carbon.area <- df_C$Carbon/df_C$Area
# global_m <- glm(Carbon.area ~ Biome+Funding+Plant_origin+Biomass+Restoration_type+Post_land_use, data = df_C, family = gaussian,  na.action = na.fail)
# #install.packages("MuMIn")
# library(MuMIn)
# modelos <- dredge(global_m)
# modelos
#
# #### result: weakest model



# Comparing the two credit systems ----------------------------------------


## Native and Exotic plants vs crediting system
#contingency table
count_data_po <- table(df_C$Plant_origin, df_C$Funding)
count_df_po <- as.data.frame(count_data_po)
colnames(count_df_po) <- c("Plant", "Creditsystem", "n")
View(count_df_po)

#Testing
fisher.test(df_C$Plant_origin, df_C$Funding)

# The proportion of CCB projects using only native species is greater than VCS projects

##Biomass
#contingency table
count_data_bm <- table(df_C$Biomass, df_C$Funding)
count_df_bm <- as.data.frame(count_data_bm)
colnames(count_df_bm) <- c("Biomass", "Creditsystem", "n")
View(count_df_bm)

#Testing
fisher.test(df_C$Biomass, df_C$Funding)

## joings the graphics 3 and 4
library(ggplot2)
library(viridis)
library(patchwork)

# First graphic
cores1 <- c("#440154FF","#482677FF","#287D8EFF")
plot1 <- ggplot(count_df_po, aes(x = n, y = Creditsystem, fill = Plant)) +
  geom_bar(stat = "identity") +
  labs(y = "Credit system",   fill = "Species origin") +
  scale_fill_manual(values = cores1) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.75, 0.8)
  )


# Second graphic
library(viridis)
cores2 <- tail(viridis(14),5)
plot2 <- ggplot(count_df_bm, aes(x = n, y = Creditsystem, fill = Biomass)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of projects", y = "Credit system",
       fill = "Biomass") +
  scale_fill_manual(values = cores2) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.7, 0.8)
  )

# Combining graphics side by side
combined_plots <- plot1 + plot2 +
  plot_layout(nrow = 2, axis_titles = 'collect') +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(size = 18, face = "bold"),
    plot.tag.position = c(0.04,0.85)# tamanho e estilo da tag
  )
ggsave("Outputs/Figures/Variables_vs_Credit_System.png",height = 10,width = 12, units = "in",dpi = 300)

## Area of the projects
#Testing
summary(aov(df_C$area_log ~ df_C$Funding))

#Ploting
ggplot(df_C, aes(x = seq_along(area_log), y = Area, color = Funding)) +
  geom_point() +
  labs(x = "Índice", y = "Area",
       color = "Credit system") +
  theme_minimal()

ggplot(df_C, aes(x = Funding, y = area_log)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(x = "Credit system", y = "Area (log)") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(), # remove grade principal
    panel.grid.minor = element_blank()
  )


ggsave("Outputs/Figures/Area_Credit_System.png",height = 10,width = 12, units = "in",dpi = 300)



##Number of plant species used vs Credit system
hist(log(df_C$species_number.x))
boxplot(df_C$species_number.x~df_C$Funding)
t.test(df_C$species_number.x~df_C$Funding)
#no big difference.

library(plotly)
data_summary <- df_C %>%
  group_by(Biomass, Funding, Plant_origin) %>%
  summarise(carbon_log_mean = mean(carbon_log, na.rm = TRUE), .groups = "drop")
ggplot(data_summary, aes(x = Plant_origin, y = Funding, fill = carbon_log_mean)) +
  geom_tile(color = "white") +
  facet_wrap(~Biomass) +
  scale_fill_viridis_c(option = "viridis", name = "Carbon mean (log)") +
  theme_minimal() +
  labs(title = "Distribuição de Projetos por Biomass, Funding e Plant Origin",
       x = "Plant origin",
       y = "Funding")
