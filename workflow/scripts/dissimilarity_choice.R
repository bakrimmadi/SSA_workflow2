##### 20230705 Workflow sequence analysis : 2 - dissimilarity_choice #####
###

##### Description de la fonction #####

# Description de la fonction 
#Choisit la distance adequate selon les criteres choisit par l'utilisateur

# Entrees  : 
  # expert : Utilisateur expert? Non : 0 , Oui : 1
  # dissimilarite_expert : Vecteur de dissimilarité saisi par l'expert
  # fusion_dissimilarites : Fusionner les dissimilarités entrées par l'expert et celles issues des critères? Non : 0 , Oui : 1
  # sequencement : Interet du sequencement TRUE/FALSE
  # timing : Interet du timing TRUE/FALSE
  # duree_etats : Interet de la duree des etats distincts TRUE/FALSE
  # sequencement_perturbation : Interet de la sensibilité aux petites perturbations TRUE/FALSE
  # timing_etatsRares : Interet particulier pour les etats rares TRUE/FALSE

# Sortie : 
  # dissimilarite : Vecteur contentant le nom des dissimilarites retenues

###### Snakemake log######
log <- file(snakemake@log[[1]], open = "wt")
sink(log)
sink(log, type="message")

###

#####                      LIBRARY                        #####


###

library(dplyr)
library(stringr)
library(tidyr)

#####                     Paramètres                       #####

dissimilarity_choice <- function(expert,
                                 dissimilarite_expert,
                                 fusion_dissimilarites,
                                 sequencement = FALSE,
                                 timing = FALSE,
                                 duree_etats = FALSE,
                                 sequencement_perturbation = FALSE,
                                 timing_etatsRares = FALSE) {
  

  ##############
  ###### BLOC VERIF paramètres fonctions ######
  ##############
  if (sequencement_perturbation & sequencement == FALSE) {
    warning("sequencement_perturbation is TRUE so sequencement is forced to TRUE")
    sequencement <- TRUE
  }
  if (timing_etatsRares & timing == FALSE) {
    warning("timing_etatsRares is TRUE so timing is forced to TRUE")
    timing <- TRUE
  }
  #Arguments
  choix_principaux <- c(sequencement, timing, duree_etats)
  #Critère principal : Aucun
  if (sum(choix_principaux) == 0) {
    dissimilarite <- c("OM", "LCS", "CHI2", "EUCLID")
  }
  #Critère principal : Multiple
  if (sum(choix_principaux) > 1) {
    dissimilarite <- c("OMstran", "OMspell", "SVRspell")
  }
  #Critère principal : Unique
  if (sum(choix_principaux) == 1) {
    #Critère principal : Sequencement
    if (sequencement & sequencement_perturbation) {
      dissimilarite <- c("SVRspell", "OMspell")
    }
    if (sequencement & sequencement_perturbation == FALSE) {
      dissimilarite <- c("OMstran", "OMspell")
    }
    #Critère principal : Timing
    if (timing & timing_etatsRares) {
      dissimilarite <- c("CHI2")
    }
    if (timing & timing_etatsRares == FALSE) {
      dissimilarite <- c("HAM", "CHI2", "EUCLID")
    }
    #Critère principal : Duree
    if (duree_etats == TRUE) {
      dissimilarite <- c("OM", "LCS", "CHI2", "EUCLID","OMspell")
    }
  }
  #Profil expert
  if (expert==1){
    cat("Valeurs du vecteur de dissimilarité :", dissimilarite, "\n")
    changer_dissimilarite <- all(dissimilarite_expert %in% c("LCS", "CHI2", "EUCLID", "OM", 
                                                              "OMstran", "OMspell", "SVRspell"))
    if (changer_dissimilarite==TRUE){

        dissimilarite_new0 <- dissimilarite_expert
        
        if (length(dissimilarite_expert) > 1){
        dissimilarite_new <- str_trim(dissimilarite_new0)
        }
        if (length(dissimilarite_expert) == 1){
          dissimilarite_new <- str_trim(str_split_1(dissimilarite_new0,","))
        }
      
      if (fusion_dissimilarites==1){
        dissimilarite_fusion <- unique(c(dissimilarite,dissimilarite_new))
        dissimilarite <- dissimilarite_fusion
        cat("Vecteur de dissimilarité final :", dissimilarite, "\n")
      }
        else{
          dissimilarite <- dissimilarite_new
          cat("Vecteur de dissimilarité final :", dissimilarite, "\n")
        }
    }
    
  }
  ###### Valeurs retournées######
  return(dissimilarite)
}

###### Utilisation des fonctions avec Snakemake ######
# On met tous les critères dans un dataframe pour les manipuler
choice_criteria0 <- data.frame(sequencement = snakemake@params[["sequencement"]],
                                         timing = snakemake@params[["timing"]],
                                         duree_etats = snakemake@params[["duree_etats"]],
                                         sequencement_perturbation = snakemake@params[["sequencement_perturbation"]],
                                           timing_etatsRares = snakemake@params[["timing_etatsRares"]])

#Transformation des critères au format logical
choice_criteria <- choice_criteria0 %>%
  dplyr::mutate_all(~trimws(str_to_upper(.))) %>%
  dplyr::mutate_all(as.logical) %>%
  dplyr::mutate_if(is.na,~FALSE)

dissimilarite0 <- dissimilarity_choice(expert = snakemake@params[["expert"]],
                                       dissimilarite_expert = snakemake@params[["dissimilarite_expert"]],
                                       fusion_dissimilarites = snakemake@params[["fusion_dissimilarites"]],
                                       sequencement = choice_criteria$sequencement,
                                       timing = choice_criteria$timing,
                                       duree_etats = choice_criteria$duree_etats,
                                       sequencement_perturbation = choice_criteria$sequencement_perturbation,
                                       timing_etatsRares = choice_criteria$timing_etatsRares)

#Export du vecteur de dissimilarités retenues
dissimilarite <- data.frame(dissimilarite = dissimilarite0)

write.csv2(dissimilarite,snakemake@output[[1]], row.names = FALSE, na="")