rm(list = ls())

# Bucle para que R busque los paquetes requridos y si no está instalado, instalarlo y después cargarlo.
pkgs <- c("rio", "tidyverse")

for (i in pkgs) {
  if(!requireNamespace(i)) {
    install.packages(i)
  }
  library(i, character.only = TRUE)
}

# Importar datos (primer conjunto)
GEIH_BOG_01 <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
GEIH_BOG_01 <- as_tibble(GEIH_BOG_01)

# Inspeccionar los datos
source("01_inspecting.R")