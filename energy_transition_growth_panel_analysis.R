###############################################################################
# Titre du projet : Impact de la transition énergétique sur la croissance
# Auteur : Théotime Turolla
# Encadrant : Jalal El Ouardighi
# Date : Février 2025
# Description : Analyse économétrique en panel de données mondiales sur énergie et PIB
###############################################################################


##### 1. Chargement des packages #####
# Installer les packages
install.packages("readxl")      
install.packages("plm")         
install.packages("dplyr")       
install.packages("tidyr")
install.packages("zoo")
install.packages("moments")
install.packages("ggplot2")
install.packages("car")

# Charger les librairies
library(tidyr)
library(readxl)
library(plm)
library(dplyr)
library(zoo)
library(moments)
library(ggplot2)
library(car)


##### 2. Importation et mise en forme des données brutes #####

# Importation du fichier Excel contenant les indicateurs mondiaux
data <- read_excel("C:/Users/theot/OneDrive/Documents/MGE2/Data/DataJalal/P_Data_Extract_From_World_Development_Indicators.xlsx")


# Aperçu rapide du contenu
head(data)
View(data)

# Passage au format long : transformation des colonnes "années" en variable Year
data_long <- data %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),        # Sélectionne uniquement les colonnes représentant des années
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year),             # Convertit Year en numérique
    Value = na_if(Value, ".."),          # Remplace ".." par NA
    Value = as.numeric(Value)            # Convertit Value en numérique
  )




# Vérifications rapides après transformation
sum(is.na(data_long$Value))              # Nombre de NA dans Value
glimpse(data_long)                       # Structure des données
data_long %>% filter(!is.na(Value)) %>% head(10)                # Observations non manquantes
data_long %>% summarise(across(everything(), ~ sum(is.na(.))))  # Nombre de NA par variable
View(data_long)                          # Affichage dans le viewer

# Renommage explicite des colonnes pour clarté
colnames(data_long) <- c("Country_Name", "Country_Code", "Series_Name", "Series_Code", "Year", "Value")
colnames(data_long)

# Création d’un objet panel à partir du format long
data_panel <- pdata.frame(data_long, index = c("Country_Name", "Year"))

# Vérification des doublons sur couple (Pays, Année)
table(duplicated(data_long[, c("Country_Name", "Year")]))




##### 3. Passage au format large (wide) pour exploitation par variable #####

# Transformation des lignes de séries en colonnes (1 colonne par variable)
data_wide <- data_long %>%
  pivot_wider(names_from = Series_Name, values_from = Value)

# Vérification des doublons persistants
data_long %>%
  dplyr::group_by(Country_Name, Country_Code, Series_Code, Year, Series_Name) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::filter(n > 1)                          # Affiche les doublons éventuels




##### 4. Format large + nettoyage des doublons + interpolation #####

# Passage définitif au format wide (1 ligne par pays/année, 1 colonne par indicateur)
data_wide <- data_long %>%
  group_by(Country_Name, Year, Series_Name) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Series_Name, values_from = Value)

# Vérification des doublons sur (pays, année)
table(duplicated(data_wide[, c("Country_Name", "Year")]))

# Exploration des cas où plusieurs valeurs coexistent pour un même pays/année
data_wide %>%
  group_by(Country_Name, Year) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  head(10)





##### 5. Interpolation et traitement des valeurs manquantes #####

# Interpolation linéaire pays par pays
data_wide_clean <- data_wide %>%
  group_by(Country_Name) %>%
  mutate(across(where(is.numeric), ~ na.approx(., na.rm = FALSE))) %>%
  ungroup()

# Nombre total de valeurs manquantes restantes
sum(is.na(data_wide_clean))

# Identifier les colonnes valides (au moins une valeur non manquante)
valid_cols <- data_wide_clean %>%
  summarise(across(where(is.numeric), ~ sum(!is.na(.)), .names = "valid_{.col}")) %>%
  pivot_longer(cols = everything(), names_to = "col_name", values_to = "valid_count") %>%
  filter(valid_count > 0) %>%
  pull(col_name) %>%
  gsub("valid_", "", .)

# Remplissage des valeurs manquantes avec forward/backward fill (na.locf)
data_wide_clean <- data_wide_clean %>%
  group_by(Country_Name) %>%
  mutate(across(all_of(valid_cols), ~ na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE))) %>%
  ungroup()

# Suppression des colonnes vides ou parasites
data_wide_clean <- data_wide_clean %>%
  select(where(~ sum(!is.na(.)) > 0)) %>%
  select(-matches("^NA$"))

# Vérification du type et structure
str(data_wide_clean)





##### 6. Finalisation du panel + gestion des problèmes de données #####

# Création du panel (structure plm) – une seule fois suffit
data_panel <- pdata.frame(data_wide_clean, index = c("Country_Name", "Year"), drop.index = TRUE, row.names = FALSE)

# Identifier les colonnes avec uniquement des valeurs NA, Inf ou NaN
problem_cols <- names(data_wide_clean)[sapply(data_wide_clean, function(x) any(is.infinite(x) | is.nan(x) | all(is.na(x))))]
problem_cols  # À vérifier manuellement si besoin

# Vérifier la proportion de NA par variable dans les pays
data_wide_clean %>%
  group_by(Country_Name) %>%
  summarise(across(where(is.numeric), ~ sum(is.na(.)) / n()), .groups = "drop") %>%
  arrange(desc(`Adjusted savings: carbon dioxide damage (% of GNI)`)) %>%
  head(10)

# Pour chaque variable : si toute une colonne est NA, on la laisse ; sinon, LOCF double
data_wide_clean <- data_wide_clean %>%
  group_by(Country_Name) %>%
  mutate(across(where(is.numeric), ~ if (all(is.na(.))) . else na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE))) %>%
  ungroup()

# Nombre total de NA restants
sum(is.na(data_wide_clean))

# Mise à jour du panel avec les données nettoyées
data_panel <- pdata.frame(data_wide_clean, index = c("Country_Name", "Year"), drop.index = TRUE, row.names = FALSE)
stopifnot(inherits(data_panel, "pdata.frame"))  # Vérification de validité du panel

# Précaution supplémentaire : remplacer les pays manquants par "Unknown"
data_wide_clean <- data_wide_clean %>%
  mutate(Country_Name = ifelse(is.na(Country_Name), "Unknown", Country_Name))




##### 7. Statistiques sur la structure du panel #####

# Nombre de pays et d’années
num_countries <- length(unique(data_panel$Country_Name))
num_years <- length(unique(data_panel$Year))

cat("Nombre de pays : ", num_countries, "\n")
cat("Nombre d’années : ", num_years, "\n")

# Réorganisation propre des index
data_panel <- pdata.frame(data_wide_clean, 
                          index = c("Country_Name", "Year"), 
                          drop.index = FALSE, row.names = TRUE)

# Résumé du panel
pdim(data_panel)

# Vérification de l'équilibre du panel
balanced_panel <- table(data_panel$Country_Name, data_panel$Year)
summary(balanced_panel)  # Permet de voir si toutes les années sont présentes pour chaque pays

# Affichage des couples (pays, année) avec valeurs manquantes dans une variable clé
missing_data <- which(is.na(data_panel$`Adjusted.savings..carbon.dioxide.damage....of.GNI.`))
missing_years <- data_panel[missing_data, c("Country_Name", "Year")]
head(missing_years)



##### 8. Statistiques descriptives globales #####

# Calcul des statistiques descriptives pour chaque variable numérique
descriptive_stats <- data_wide_clean %>%
  summarise(across(where(is.numeric), 
                   list(
                     n = ~ sum(!is.na(.)),  # Nombre d'observations non manquantes
                     Mean = ~ mean(., na.rm = TRUE),
                     SD = ~ sd(., na.rm = TRUE),
                     Median = ~ median(., na.rm = TRUE),
                     Trimmed = ~ mean(., trim = 0.1, na.rm = TRUE),  # Moyenne coupée (10%)
                     MAD = ~ mad(., na.rm = TRUE),  # Médiane des écarts absolus
                     Min = ~ min(., na.rm = TRUE),
                     Max = ~ max(., na.rm = TRUE),
                     Range = ~ max(., na.rm = TRUE) - min(., na.rm = TRUE),  # Étendue
                     Skew = ~ skewness(., na.rm = TRUE),  # Asymétrie
                     Kurtosis = ~ kurtosis(., na.rm = TRUE)  # Aplatissement
                   ), .names = "{col}_{fn}"))  # Nommage explicite des stats

# Affichage et export des résultats
print(descriptive_stats)
write.csv(descriptive_stats, "descriptive_stats.csv", row.names = FALSE)




##### 9. Visualisation sur un sous-échantillon de pays représentatifs #####

# Liste des pays sélectionnés
selected_countries <- c("China", "France", "Germany", "India", "United States", 
                        "South Africa", "Nigeria", "Canada", "Mexico", "Australia", 
                        "Brazil", "Argentina")

# Création du sous-échantillon à partir du panel final
data_subset <- data_panel %>%
  filter(Country_Name %in% selected_countries)

# Vérification rapide
table(data_subset$Country_Name)  # Nb d'observations par pays
table(data_subset$Year)          # Nb d'années par pays




##### 10. Graphiques temporels par indicateur #####

# Courbe 1 : Croissance du PIB
ggplot(data_subset, aes(x = Year, y = GDP.growth..annual..., group = Country_Name, color = Country_Name)) +
  geom_line(size = 1) +
  labs(title = "Yearly GDP Growth (Annual %)",
       x = "Year",
       y = "GDP Growth (%)",
       color = "Country") +
  theme_minimal()

# Courbe 2 : Électricité renouvelable (hors hydro)
ggplot(data_subset, aes(x = Year, 
                        y = Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh., 
                        color = Country_Name, 
                        group = Country_Name)) +
  geom_line(size = 1) +
  labs(title = "Electricity Production from Renewables (Excl. Hydro)",
       x = "Year",
       y = "Electricity Production (kWh)",
       color = "Country") +
  theme_minimal()

# Courbe 3 : Intensité énergétique
ggplot(data_subset, aes(x = Year, y = Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP., group = Country_Name, color = Country_Name)) +
  geom_line(size = 1) +
  labs(title = "Energy Intensity Level (MJ/$2017 PPP GDP)",
       x = "Year",
       y = "Energy Intensity",
       color = "Country") +
  theme_minimal()

# Courbe 4 : Consommation d'énergie fossile
ggplot(data_subset, aes(x = Year, y = Fossil.fuel.energy.consumption....of.total., group = Country_Name, color = Country_Name)) +
  geom_line(size = 1) +
  labs(title = "Fossil Fuel Energy Consumption (% of Total)",
       x = "Year",
       y = "Consumption (%)",
       color = "Country") +
  theme_minimal()



##### 11. Vérification et traitement des données manquantes #####

# Aperçu des données avant estimation
View(data_panel)
colSums(is.na(data_panel))  # Nombre de valeurs manquantes par colonne

# Nombre de modalités distinctes par variable
sapply(data_panel, function(x) length(unique(x[!is.na(x)])))



##### 12. Pré-traitement des données pour estimation économétrique #####

# Étape 1 : Supprimer les lignes où la variable dépendante est manquante
data_panel <- data_panel %>%
  filter(!is.na(GDP.growth..annual...))

# Étape 2 : Interpolation linéaire pour les variables à évolution temporelle
vars_interp <- c("Adjusted.savings..carbon.dioxide.damage....of.GNI.",
                 "Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP.",
                 "Fossil.fuel.energy.consumption....of.total.")

data_panel <- data_panel %>%
  group_by(Country_Name) %>%
  mutate(across(all_of(vars_interp), ~ na.approx(., na.rm = FALSE))) %>%
  ungroup()

# Étape 3 : Imputation par médiane pour l’électricité renouvelable
data_panel <- data_panel %>%
  group_by(Country_Name) %>%
  mutate(Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh. = 
           ifelse(is.na(Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh.), 
                  median(Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh., na.rm = TRUE), 
                  Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh.)) %>%
  ungroup()

# Étape 4 : Double interpolation + LOCF pour lisser et combler les NA résiduels
data_panel <- data_panel %>%
  group_by(Country_Name) %>%
  mutate(across(where(is.numeric), ~ na.approx(na.approx(., na.rm = FALSE), na.rm = FALSE))) %>%
  mutate(across(where(is.numeric), ~ na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Étape 5 : Correction extrême pour colonnes entièrement manquantes dans certains pays
data_panel <- data_panel %>%
  group_by(Country_Name) %>%
  mutate(across(where(is.numeric), ~ if (all(is.na(.))) . else na.locf(na.locf(., na.rm = FALSE), fromLast = TRUE))) %>%
  ungroup()

# Vérification finale
colSums(is.na(data_panel))

# Sauvegarde du dataset propre
write.csv(data_panel, "data_panel_cleaned.csv", row.names = FALSE)





##### 13. Estimation économétrique de base #####

# Modèle MCO (Moindres Carrés Ordinaires)
model_ols <- lm(GDP.growth..annual... ~ 
                  Adjusted.savings..carbon.dioxide.damage....of.GNI. + 
                  Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh. +
                  Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                  Fossil.fuel.energy.consumption....of.total., 
                data = data_panel)

summary(model_ols)

# Modèle à effets fixes (within estimator)
model_fe <- plm(GDP.growth..annual... ~ 
                  Adjusted.savings..carbon.dioxide.damage....of.GNI. + 
                  Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh. +
                  Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                  Fossil.fuel.energy.consumption....of.total., 
                data = data_panel, 
                model = "within")

summary(model_fe)





##### 14. Estimation des modèles économétriques #####

# Modèle à effets fixes (WITHIN)
model_fe <- plm(GDP.growth..annual... ~ 
                  Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                  Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh. +
                  Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                  Fossil.fuel.energy.consumption....of.total.,
                data = data_panel,
                model = "within")

summary(model_fe)

# Vérification : variables constantes dans le temps ?
data_panel %>%
  group_by(Country_Name) %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE)) %>%
  summarise(across(everything(), ~ sum(. == 0, na.rm = TRUE)))

# Vérification de la multicolinéarité
library(car)
vif(lm(GDP.growth..annual... ~ 
         Adjusted.savings..carbon.dioxide.damage....of.GNI. +
         Electricity.production.from.renewable.sources..excluding.hydroelectric..kWh. +
         Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
         Fossil.fuel.energy.consumption....of.total.,
       data = data_panel))

# Modèle à effets fixes sans variable colinéaire
model_fe <- plm(GDP.growth..annual... ~ 
                  Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                  Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                  Fossil.fuel.energy.consumption....of.total.,
                data = data_panel,
                model = "within")

summary(model_fe)

# Modèle à effets aléatoires (RANDOM)
model_re <- plm(GDP.growth..annual... ~ 
                  Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                  Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                  Fossil.fuel.energy.consumption....of.total.,
                data = data_panel,
                model = "random")

summary(model_re)

# Test de Hausman : choix entre FE et RE
phtest(model_fe, model_re)

# Modèle Between (inter-pays)
model_between <- plm(GDP.growth..annual... ~ 
                       Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                       Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                       Fossil.fuel.energy.consumption....of.total.,
                     data = data_panel,
                     model = "between")

summary(model_between)

# Modèle Pooled OLS
model_pooled <- plm(GDP.growth..annual... ~ 
                      Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                      Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                      Fossil.fuel.energy.consumption....of.total.,
                    data = data_panel,
                    model = "pooling")

summary(model_pooled)

# Modèle GLS (correction autocorrélation AR1)
library(nlme)

data_panel$Country_Name <- as.character(data_panel$Country_Name)
data_panel$Year <- as.numeric(as.character(data_panel$Year))

model_gls <- gls(GDP.growth..annual... ~ 
                   Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                   Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                   Fossil.fuel.energy.consumption....of.total.,
                 data = data_panel,
                 correlation = corAR1(form = ~Year | Country_Name))

summary(model_gls)

# Modèle à effets fixes avec effets temporels
model_fe_time <- plm(GDP.growth..annual... ~ 
                       Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                       Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                       Fossil.fuel.energy.consumption....of.total. +
                       factor(Year),
                     data = data_panel,
                     model = "within")

summary(model_fe_time)

# Modèle GMM dynamique
library(pgmm)

model_gmm <- pgmm(GDP.growth..annual... ~ lag(GDP.growth..annual..., 1) +
                    Adjusted.savings..carbon.dioxide.damage....of.GNI. +
                    Energy.intensity.level.of.primary.energy..MJ..2017.PPP.GDP. +
                    Fossil.fuel.energy.consumption....of.total. |
                    lag(GDP.growth..annual..., 2:3),
                  data = data_panel,
                  effect = "individual",
                  model = "twosteps")

summary(model_gmm)
