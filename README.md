# **Beyond Carbon: how biodiversity is considered in carbon market restoration projects**

This repository contains all data, R scripts, and outputs from the statistical analyses used to investigate the role of biodiversity in environmental restoration projects submitted to one of the largest carbon certification systems: the Verified Carbon Standard (VCS) Program by the Verra organization.

I assessed whether biodiversity has been considered during restoration planning, whether there is a monitoring plan for biodiversity, and whether the metrics used to measure biodiversity are appropriate.

This is the first chapter of my doctoral thesis developed in the Graduate Program in Ecology and Evolution.

# **Repository structure**

All type of material is organized in individual folders properly named and described bellow:

## **Data folder**
Data folder contain two sub folders:

1. Raw: data in the way it was collected from the projects description
- 02_Verra-registry_2024-01-03

2. Processed:data in a table format suitable for statistical analysis
- Projects_info_V06.csv: table with all information useful for statistical analyses were maintained here and cleaned. This is the file ran in the scripts 
- Projects_info.csv: table with all information useful for statistical analyses but without a basic cleaning
- Projects_info.xlsx: table with all information useful for statistical analyses but without a basic cleaning in the xlsx format
- Species_name_original_data.xlsx: table obtained thought script 05 analyses aiming that each line correspond to only one planted species, allowing further standardization of planted species name.  

## **R folder**

The R folder contains all scripts made in R Studio software 4.3.2.

The scripts are numbered in the best order to running, since there are dependence relationship between some scripts.

- 01_Reading-and-Cleaning.R: view, clean, and standardize the data.
- 02_Analyzes_projects-distribution.R: summary the distribution of restoration projects analyzed and generation of a distribution map. 
- 03_Summary_factors.R: summary of projects information, such as: climate zone, biome, certification system, type of restoration, class of land-owner, plant origin, biodiversity metrics.
- 04_Analyzes_carbon.R: statistical analyzes to investigate the drivers of the estimated carbon removed of avoided. The script also performs a comparison analyses between the projects certification systems. 
- 05_Analyzes_planted-species.R: standardization, correction, and summary of planted species names.
- 06_Analyzes_biodiversity-metrics.R:standardization, correction, and summary of planted species names. 

## **Outputs folder**

This folder contains tables and figures resulting from statistical analyses or summary procedures.

