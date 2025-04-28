# Impact de la transition énergétique sur la croissance économique

Ce projet évalue l'impact de la transition énergétique (émissions de CO2, intensité énergétique, part des énergies fossiles, production d’électricité renouvelable) sur la croissance du PIB via des modèles économétriques en panel.

## Contenu du dépôt

- `energy_transition_growth_panel_analysis.R` : script complet (nettoyage, interpolation, estimation OLS/FE/RE/GLS/GMM)
- `Impact_Transition_Energetique_Croissance_Panel.pdf` : rapport d’analyse détaillé
- `data/P_Data_Extract_From_World_Development_Indicators.xlsx` : données de la Banque mondiale (indicateurs pays-années, 1990-2023)

## Méthodologie

- **Modèles estimés** : MCO, effets fixes (FE), effets aléatoires (RE), GLS (AR1), GMM dynamique
- **Tests réalisés** : Hausman, Breusch-Pagan, Wooldridge, Dickey-Fuller, multicolinéarité (VIF)
- **Prétraitement** : imputation (interpolation linéaire, LOCF, médiane pays), transformation panel `pdata.frame`

## Objectif

Identifier l’impact des politiques énergétiques sur la croissance, et valider économétriquement la pertinence d’un modèle à effets fixes avec erreurs robustes.

## Auteurs

- Théotime Turolla  
- Encadrant : El Ouardighi Jalal  
- Université de Strasbourg, 2025