rm(list = ls())

pkgs = c("tidyverse", "httr", "readr", "fs", "here")

for (i in pkgs) {
  if(!requireNamespace(i)) {
    install.packages(i)
  }
  library(i, character.only = TRUE)
}

