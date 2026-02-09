rm(list = ls())

# Bucle para que R busque los paquetes requridos y si no está instalado, instalarlo y después cargarlo.
pkgs <- c("rio", "tidyverse", "skimr", "stargazer", "dplyr", "tidyr", "corrplot", "pacman", "rvest")

for (i in pkgs) {
  if(!requireNamespace(i)) {
    install.packages(i)
  }
  library(i, character.only = TRUE)
}

# Inspeccionar los datos
source("01_inspecting.R")