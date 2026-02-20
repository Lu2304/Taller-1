rm(list = ls())

#Instalación de paquetes y cargue de librerías

if (!require("pacman")) install.packages("pacman")

# Cargar y/o instalar todos los paquetes de una vez
pacman::p_load(rio, tidyverse, skimr, stargazer, dplyr, tidyr, 
               corrplot, rvest, flextable, officer, boot, ggplot2,webshot2, gt, modelsummary)

# Paths 
path_input  <- "Input"
path_raw <- file.path(path_input, "Raw")
path_cleaned  <- file.path(path_input, "Cleaned")

path_output  <- "Output"
path_figures <- file.path(path_output, "Figures")
path_tables  <- file.path(path_output, "Tables")

dir.create(path_raw,     recursive = TRUE, showWarnings = FALSE)
dir.create(path_cleaned, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables,  recursive = TRUE, showWarnings = FALSE)

# Inspeccionar los datos
source(file.path("Code", "01_inspecting.R"))