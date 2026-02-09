rm(list = ls())

# Bucle para que R busque los paquetes requridos y si no está instalado, instalarlo y después cargarlo.
pkgs <- c("rio", "tidyverse", "skimr", "stargazer", "dplyr", "tidyr", 
          "corrplot", "pacman", "rvest", "flextable", "officer")

for (i in pkgs) {
  if(!requireNamespace(i)) {
    install.packages(i)
  }
  library(i, character.only = TRUE)
}

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