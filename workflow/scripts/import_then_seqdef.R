##### 20230705 Workflow sequence analysis : 1 - import_then_seqdef #####
###

##### Description de la fonction #####

# Description de la fonction : Importe des données au format excel ou csv et 
  #les transforme en données au format séquentielles

# Entrées : 
  # chemin_bdd : Chemin du fichier avec le nom de la base de données et son extension
  # bdd_format : Format de la bdd (Long ou wide)         
  # var_id : nom de la variable 
  # var_rep : nom(s) de la ou des variables réponses
  # labels : vecteur contenant les labels des modalités de la variables réponses

# Sortie :
  # liste contenant 2 bdd:
    # bdd au format wide 
    # bdd au format seqdef


###### Snakemake log######
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
install_missing_packages("hdd")
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(hdd)
library(readxl)
library(TraMineR)


#####                     Paramètres                       #####

import_before_seqdef <- function(chemin_bdd=NULL,
                       bdd_format=c("Long","Wide"),
                       var_id=NULL,
                       var_rep=NULL){
  '%ni%' <- Negate('%in%') #Inverse de %in%
  #Arguments
  bdd_format <- match.arg(bdd_format)
  #chemin_bdd<-str_to_lower(chemin_bdd)#Pb univ voir file exist
  ##############
  ###### BLOC Import ######
  ##############
  
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
  ##############
  ###### BLOC VERIF paramètres fonctions ######
  ##############
  if(is.numeric(var_id)) {
    var_id <- colnames(bdd0)[var_id]
  }
  
  if(is.numeric(var_rep)) {
    var_rep <- colnames(bdd0)[var_rep]
  }

  if(is.null(var_id)){
    stop("No identifiant variable")
  }
  if(all(var_id %ni% colnames(bdd0))){
    stop("No identifiant variable in the data")
  }
  if(is.null(var_rep)){
    stop("Missing var_rep")
  }
  if(all(var_rep %ni% colnames(bdd0))){
    stop("No var_rep name in the data")
  }
  if(any(var_rep %ni% colnames(bdd0))){
    temp <- var_rep[var_rep %ni% colnames(bdd0)]
    warning(paste0("var_rep contains variables that are not present in the data : ", paste0(temp, collapse = " & "), "\nremoval of additional variables"))
    var_rep <- var_rep[var_rep %in% colnames(bdd0)]
  }
  if(bdd_format %ni% c("Long","Wide")){
    warning("bdd_format must be in ('Long','Wide'), by default : bdd_format = 'Long' if lenght of var_rep=1 else bdd_format = 'Wide'")
    if(length(var_rep)==1){
      bdd_format<-"Long"
    }
    else{bdd_format<-"Wide"}
  }
  if(bdd_format=="Long" & length(var_rep)>1){
    stop("Format Long implies a character vector of length 1 for var_rep")
  }
  if(bdd_format=="Wide" & length(var_rep)<=2){
    stop("Format Wide implies a character vector of length 2 or more for var_rep")
  }

  ###### Format Long ###### 
  if (bdd_format=="Long"){
    bdd <- bdd0 %>%
      dplyr::select(var_id, var_rep) %>%
      dplyr::mutate_if(is.integer, ~as.numeric(.)) %>%
      dplyr::mutate_if(is.character, ~as.factor(.))%>%
      dplyr::group_by(across(var_id))%>%
      dplyr::mutate(temps = dplyr::row_number()) %>%
      tidyr::spread(temps,var_rep) %>%
      dplyr::arrange(across(var_id))%>%
      ungroup()
    colnames(bdd)<-c(var_id,paste0("Time",colnames(bdd[str_detect(colnames(bdd),"[0-9]")])))
  }
  ###### Format Wide ###### 
  if (bdd_format=="Wide"){
    bdd <- bdd0 %>%
      dplyr::select(var_id, var_rep) %>%
      dplyr::mutate_if(is.integer, ~as.numeric(.)) %>%
      dplyr::mutate_if(is.character, ~as.factor(.))
    colnames(bdd)<-c(var_id,paste0("Time_",1:(length(bdd)-1)))
  }
  ##### Data final avant conversion au format seq #####
  bdd %>% 
    dplyr::select(-var_id)


  ###### Valeurs retournées######
  # return(list(bdd=bdd, seq=seq))
  
}
###### Création d'un objet sequence ######
convert_seqdef <- function(bdd, labels = NULL) {
  if (is.null(labels)) {
    seqdef(bdd)
  }
  if (!is.null(labels)) {
    seqdef(bdd, states = labels)
  }
}
###### Utilisation des fonctions avec Snakemake ######
#### Transformation de la liste des variables réponses en vecteur
var_rep_config <- str_split(snakemake@config[["var_rep"]], ",")[[1]] %>%
  str_trim()
#### Import de la table selon les infos du fichier de config
bdd_seq <-
  import_before_seqdef(
    chemin_bdd=snakemake@input[[1]],
    bdd_format=snakemake@config[["bdd_format"]],
    var_id=snakemake@config[["var_id"]],
    var_rep=var_rep_config
  )
#### Transformation de la liste des labels en vecteur
labels_config <- str_split(snakemake@config[["labels"]], ",")[[1]] %>%
  str_trim()
#### Conversion au format seqdef selon les infos du fichier de config
seq <- convert_seqdef(bdd = bdd_seq,
                      labels = labels_config)
# Write seqdef object as RDS
saveRDS(seq, file = snakemake@output[[1]])