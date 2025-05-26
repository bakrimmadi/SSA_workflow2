##### 20240503 Workflow sequence analysis : 1 bis - imputation_missing_long #####
###

##### Description de la fonction #####

# Description de la fonction : Complète des données long en ajoutant les suivis manquants

# Entrées : 
# bdd : base de données au format long avec des suivis manquants
# var_id : nom de la variable ID
# var_suivi : nom de la variable suivi
# var_rep : nom de la variable réponse

# Sortie :
# bdd complete

log <- file(snakemake@log[[1]], open = "wt")
sink(log)
sink(log, type="message")

###

#####                      LIBRARY                        #####


###
# Installer les packages manquants
install_missing_packages <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = "http://cran.univ-lyon1.fr/")
    library(package, character.only = TRUE)
  }
  else {
    library(package, character.only = TRUE)
  }
}
install_missing_packages("data.table")
install_missing_packages("hdd")
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(hdd)
library(readxl)

#####                     Paramètres                       #####

imputation_missing_long <- function(bdd,
                                    var_ID,
                                    var_suivi,
                                    var_rep) {
Tab_Univ <- bdd %>%
  dplyr::mutate_if(is.character,factor) %>%
  dplyr::select(var_ID,var_suivi,var_rep)


Tab_univ_max_Delai <-Tab_Univ %>%
  dplyr::group_by(!!sym(var_ID)) %>%
  dplyr::summarise(Delai_max = max(get(var_suivi)))

Tab_Univ_complete <- Tab_Univ %>%
  dplyr::mutate(!!sym(var_suivi) := factor(!!sym(var_suivi), levels = min(!!sym(var_suivi)):max(!!sym(var_suivi)))) %>%
  tidyr::complete(!!sym(var_ID), !!sym(var_suivi)) %>%
  dplyr::left_join(Tab_univ_max_Delai, by = var_ID)

Tab_Univ_complete_finale <- Tab_Univ_complete %>%
  dplyr::mutate(!!sym(var_suivi) := as.numeric(!!sym(var_suivi))-1) %>%
  dplyr::filter(!!sym(var_suivi)<=Delai_max) %>%
  dplyr::arrange(!!sym(var_ID),!!sym(var_suivi)) %>%
  tidyr::fill(!!sym(var_rep)) %>%
  dplyr::select(-Delai_max)
}
##############
###### BLOC Import ######
##############
#Chemin du csv
chemin_bdd <- snakemake@input[[1]]

if(file.exists(chemin_bdd)==FALSE){
  stop("File path doesn't exist")
}
if (str_detect(chemin_bdd,".csv")){#extension
  # Détection automatique du séparateur
  separateur <- hdd::guess_delim(chemin_bdd)
  # Import du fichier CSV avec le séparateur détecté
  bdd0 <- readr::read_delim(chemin_bdd, delim = separateur) %>%
    data.frame()
  # bdd0<-read.csv2(chemin_bdd,stringsAsFactors = FALSE) %>%
  #   data.frame()
}
if (str_detect(chemin_bdd,".xls|.xlsx")){
  bdd0<-read_excel(chemin_bdd)%>%
    data.frame()
}
if (!str_detect(chemin_bdd,".csv|.xls|.xlsx")){
  stop("Select a xls/xlsx or csv file")  
}
if(is.null(bdd0)){
  stop("No data")
}
bdd_f <- imputation_missing_long(bdd = bdd0,
                  var_ID = "Id",
                  var_suivi = "Delai",
                  var_rep = "Gaz")
write.csv2(bdd_f,snakemake@output[[1]], row.names = FALSE, na="")
