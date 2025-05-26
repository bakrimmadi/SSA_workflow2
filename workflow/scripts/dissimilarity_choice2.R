##### 20240506 Workflow sequence analysis : 2 - dissimilarity_choice #####
###

##### Description de la fonction #####

# Description de la fonction 
#Choisit la distance adequate selon les criteres choisit par l'utilisateur

# Entrees  : 
# sequencement : Interet du sequencement TRUE/FALSE
# timing : Interet du timing TRUE/FALSE
# duree_etats : Interet de la duree des etats distincts TRUE/FALSE
# sequencement_perturbation : Interet de la sensibilité aux petites perturbations TRUE/FALSE
# timing_etatsRares : Interet particulier pour les etats rares TRUE/FALSE

# Sortie : 
# dissimilarite : Vecteur contentant le nom des dissimilarites retenues



###

#####                      LIBRARY                        #####


###

library(dplyr)
library(stringr)
library(tidyr)

#####                     Paramètres                       #####

dissimilarity_choice <- function(sequencement = FALSE,
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
  oui_non <- c("Oui","Non")
  expert <- menu(oui_non,title = "Êtes-vous un profil expert? :")
  #Profil expert
  if (expert==1){
    cat("Valeurs du vecteur de dissimilarité :", dissimilarite, "\n")
    changer_dissimilarite <- menu(oui_non,title = "Voulez-vous changer ce vecteur de dissimilarité à tester? :")
    if (changer_dissimilarite==1){
      # Boucle pour demander à l'utilisateur de saisir le vecteur jusqu'à ce qu'il soit valide
      dissimilarite_new <-" character(0)"
      
      while (!(all(dissimilarite_new %in% c(
          "LCS",
          "CHI2",
          "EUCLID",
          "OM",
          "OMstran",
          "OMspell",
          "SVRspell")
          ))
      ) {
        dissimilarite_new0 <- readline("Entrez un vecteur avec les dissimilarités de votre choix séparé par une virgule (ex : LCS, OM, OMspell) :")
        dissimilarite_new <- str_trim(str_split_1(dissimilarite_new0,","))
      }
      dissimilarite <- dissimilarite_new
      cat("Vecteur de dissimilarité final :", dissimilarite, "\n")
      
    }
    
  }
  ###### Valeurs retournées######
  return(dissimilarite)
}

##### Tests fonctions ######
# test<-dissimilarity_choice(sequencement = TRUE,
#                            timing = TRUE,
#                            duree_etats = FALSE,
#                            sequencement_perturbation = FALSE,
#                            timing_etatsRares = TRUE)
