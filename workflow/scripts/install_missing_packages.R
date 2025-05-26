##### 20230705 Workflow sequence analysis : 0 - install_missing_packages #####
###
##### Description de la fonction #####

# Description de la fonction : Installe les packages nécessaire à la bonne éxécution du workflow sequence analysis

# Entrées : 
# packages : Liste des packages à vérifier et installer pour le Workflow

# Sortie :

# Liste des packages à vérifier et installer pour le Workflow
packages <- c("hdd","readr", "readxl", "dplyr","tidyr","TraMineR",
              "dynamicTreeCut","WeightedCluster","ParBayesianOptimization",#Pour linux : sudo apt install cmake
              "fpc","tibble","janitor","seqhandbook","lubridate","writexl",
              "igraph","targets")  
# Installer les packages manquants
install_missing_packages <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
  else {
    library(package, character.only = TRUE)
  }
}

# Appliquer la fonction sur la liste de packages
lapply(packages, install_missing_packages)
