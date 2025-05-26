##### 20240306 Workflow sequence analysis : 3 - params_dissimilarity_choice #####
###

##### Description de la fonction #####
# Fonction : params_dissimilarity --> Choix des plages de valeurs pour chaque hyperparamètres de chaque dissimilarité

# Entrées  
#     dissimilarite : Vecteur contentant le nom des dissimilarites retenues
#     expert : Utilisateur expert? Non : 0 , Oui : 1
#     paramètre seqsubm pour dissimilarité : OM  Valeurs entières entre 0 et 4, ex : "c(0L,4L)"
#     paramètre seqsubm pour dissimilarité : OMstran Valeurs entières entre 0 et 4, ex : "c(0L,4L)"
#     paramètre transindel pour dissimilarité : OMstran Valeurs entières entre 0 et 2, ex : "c(0L,2L)"
#     paramètre otto pour dissimilarité : OMstran , ex : "c(0.001,1)"
#     paramètre seqsubm pour dissimilarité : OMspell Valeurs entières entre 0 et 4, ex : "c(0L,4L)"
#     paramètre expcost pour dissimilarité : OMspell, ex : "c(0,3)"
#     paramètre a pour dissimilarité : SVRspell, ex : "c(0,3)"
#     paramètre b pour dissimilarité : SVRspell, ex : "c(0,3)"
#     paramètre tpow pour dissimilarité : SVRspell, ex : "c(0,3)"


# Sortie 
#     bounds : liste contenant les plages de valeurs des hyperparamètres à faire varier pour chaque dissimilarité
###

###### Snakemake log######
log <- file(snakemake@log[[1]], open = "wt")
sink(log)
sink(log, type="message")
#####                      LIBRARY                        #####

library(dplyr)
library(stringr)
library(tidyr)
###

#####                     Paramètres                       #####
params_dissimilarity_choice <- function(dissimilarite,
                                        expert,
                                        bounds_OM_seqsubm,
                                        bounds_OMstran_seqsubm,bounds_OMstran_transindel,bounds_OMstran_otto,
                                        bounds_OMspell_seqsubm,bounds_OMspell_expcost,
                                        bounds_SVRspell_a,bounds_SVRspell_b,bounds_SVRspell_tpow) {
  if (all(
    dissimilarite %in% c(
      "LCS",
      "CHI2",
      "EUCLID",
      "OM",
      #HAM ne fonctionne pas (uniquement pour les seq de même taille de base)
      "OMstran",
      "OMspell",
      "SVRspell"
    )
  ) == FALSE) {
    stop(
      "All elements of the dissimilarity vector must be in
        LCS, CHI2, EUCLID, OM, OMstran, OMspell, SVRspell"
    )
  }

  
  if (expert==1) {
    #Bounds
    bounds_LCS <- list()
    bounds_CHI2 <- list()
    bounds_EUCLID <- list()
    
    if ("OM" %in% dissimilarite) {
    #bounds_OM
      bounds_OM <- list(seqsubm = c(0L, 4L))
      cat("Valeurs par defaut pour les arguments de OM :", paste(c("seqsubm ="),bounds_OM), "\n")
      
      changer_defaut <- any(str_detect(bounds_OM_seqsubm, "[0-9]"))
      
      if (changer_defaut == TRUE){
        if (str_detect(bounds_OM_seqsubm, "[0-9]")){
          bounds_OM[["seqsubm"]] <-
            eval(parse(text = bounds_OM_seqsubm))
        }
        
      }
      
    }
    else{
      bounds_OM <- list(seqsubm = c(0L, 4L))
    }
    #bounds_OMstran
    if ("OMstran" %in% dissimilarite) {
      #bounds_OMstran
      bounds_OMstran <- list(
        seqsubm = c(0L, 4L),
        transindel = c(0L, 2L),
        otto = c(0.0001, 1)
      )
      cat("Valeurs par defaut pour les arguments de OMstran :", paste(c("seqsubm =","transindel =","otto ="),bounds_OMstran), "\n")
      
      changer_defaut <- any(str_detect(c(bounds_OMstran_seqsubm,bounds_OMstran_transindel,bounds_OMstran_otto), "[0-9]"))
      
      if (changer_defaut == TRUE){
        if (str_detect(bounds_OMstran_seqsubm, "[0-9]")){
          bounds_OMstran[["seqsubm"]] <-
            eval(parse(text = bounds_OMstran_seqsubm))
        }
        if (str_detect(bounds_OMstran_transindel, "[0-9]")){
          bounds_OMstran[["transindel"]] <-
            eval(parse(text = bounds_OMstran_transindel))
        }
        if (str_detect(bounds_OMstran_otto, "[0-9]")){
          bounds_OMstran[["otto"]] <-
            eval(parse(text = bounds_OMstran_otto))
        }
        
      } 
    }
    else{
      bounds_OMstran <- list(
        seqsubm = c(0L, 4L),
        transindel = c(0L, 2L),
        otto = c(0.0001, 1)
      )
    }

    #bounds_OMspell
    if ("OMspell" %in% dissimilarite) {
      #bounds_OMspell
      bounds_OMspell <- list(seqsubm = c(0L, 4L),
                             expcost = c(0, 3))
      cat("Valeurs par defaut pour les arguments de OMspell :", paste(c("seqsubm =","expcost ="),bounds_OMspell), "\n")
      
      changer_defaut <- any(str_detect(c(bounds_OMspell_seqsubm,bounds_OMspell_expcost), "[0-9]"))
      
      if (changer_defaut == TRUE){
        if (str_detect(bounds_OMspell_seqsubm, "[0-9]")){
          bounds_OMspell[["seqsubm"]] <-
            eval(parse(text = bounds_OMspell_seqsubm))
        }
        if (str_detect(bounds_OMspell_expcost, "[0-9]")){
          bounds_OMspell[["expcost"]] <-
            eval(parse(text = bounds_OMspell_expcost))
        }
        
      }
      
    }
    else{
      bounds_OMspell <- list(seqsubm = c(0L, 4L),
                             expcost = c(0, 3))
    }
    #bounds_SVRspell
    if ("SVRspell" %in% dissimilarite) {
      #bounds_SVRspell par défaut
      bounds_SVRspell <- list(
        a = c(0, 3),
        b = c(0, 3),
        tpow = c(0, 3))
      cat("Valeurs par defaut pour les arguments de SVRspell :", paste(c("a =","b =","tpow ="),bounds_SVRspell), "\n")
      
      changer_defaut <- any(str_detect(c(bounds_SVRspell_a,bounds_SVRspell_b,bounds_SVRspell_tpow), "[0-9]"))
      
      if (changer_defaut == TRUE){
        if (str_detect(bounds_SVRspell_a, "[0-9]")){
          bounds_SVRspell[["a"]] <-
          eval(parse(text = bounds_SVRspell_a))
        }
        if (str_detect(bounds_SVRspell_b, "[0-9]")){
          bounds_SVRspell[["b"]] <-
            eval(parse(text = bounds_SVRspell_b))
        }
        if (str_detect(bounds_SVRspell_tpow, "[0-9]")){
          bounds_SVRspell[["tpow"]] <-
            eval(parse(text = bounds_SVRspell_tpow))
        }
        
      }
    }
    else{
      bounds_SVRspell <- list(
        a = c(0, 3),
        b = c(0, 3),
        tpow = c(0, 3))
    }

    # Utiliser les paramètres pour le reste du programme
    cat("Utilisation des paramètres :\n")
    cat("LCS :", bounds_LCS, "\n")
    cat("CHI2 :", bounds_CHI2, "\n")
    cat("EUCLID :", bounds_EUCLID, "\n")
    cat("OM :", paste(c("seqsubm ="),bounds_OM), "\n")
    cat("OMstran :", paste(c("seqsubm =","transindel =","otto ="),bounds_OMstran), "\n")
    cat("OMspell :", paste(c("seqsubm =","expcost ="),bounds_OMspell), "\n")
    cat("SVRspell :", paste(c("a =","b =","tpow ="),bounds_SVRspell), "\n")
  } else {
    # Paramètres par défaut pour un profil non expert
    bounds_LCS <- list()
    bounds_CHI2 <- list()
    bounds_EUCLID <- list()
    bounds_OM <- list(seqsubm = c(0L, 4L))
    bounds_OMstran <- list(
      seqsubm = c(0L, 4L),
      transindel = c(0L, 2L),
      otto = c(0.0001, 1)
    )
    bounds_OMspell <- list(seqsubm = c(0L, 4L),
                           expcost = c(0, 3))
    bounds_SVRspell <- list(
      a = c(0, 3),
      b = c(0, 3),
      tpow = c(0, 3))
    
      cat("Paramètres par défaut pour un profil non expert :\n")
      cat("LCS :", bounds_LCS, "\n")
      cat("CHI2 :", bounds_CHI2, "\n")
      cat("EUCLID :", bounds_EUCLID, "\n")
      cat("OM :", paste(c("seqsubm ="),bounds_OM), "\n")
      cat("OMstran :", paste(c("seqsubm =","transindel =","otto ="),bounds_OMstran), "\n")
      cat("OMspell :", paste(c("seqsubm =","expcost ="),bounds_OMspell), "\n")
      cat("SVRspell :", paste(c("a =","b =","tpow ="),bounds_SVRspell), "\n")
  }
  
  bounds <- list(LCS = bounds_LCS,
                 CHI2 = bounds_CHI2,
                 EUCLID = bounds_EUCLID,
                 OM = bounds_OM,
                 OMstran = bounds_OMstran,
                 OMspell = bounds_OMspell,
                 SVRspell = bounds_SVRspell)
}
###### Utilisation des fonctions avec Snakemake ######
dissimilarite0 <- read.csv2(snakemake@input[[1]])
dissimilarite <- dissimilarite0$dissimilarite 

bounds <- params_dissimilarity_choice(dissimilarite = dissimilarite,
                                      expert = snakemake@params[["expert"]],
                                      bounds_OM_seqsubm = snakemake@params[["bounds_OM_seqsubm"]],
                                      bounds_OMstran_seqsubm = snakemake@params[["bounds_OMstran_seqsubm"]],
                                      bounds_OMstran_transindel = snakemake@params[["bounds_OMstran_transindel"]],
                                      bounds_OMstran_otto = snakemake@params[["bounds_OMstran_otto"]],
                                      bounds_OMspell_seqsubm = snakemake@params[["bounds_OMspell_seqsubm"]],
                                      bounds_OMspell_expcost = snakemake@params[["bounds_OMspell_expcost"]],
                                      bounds_SVRspell_a = snakemake@params[["bounds_SVRspell_a"]],
                                      bounds_SVRspell_b = snakemake@params[["bounds_SVRspell_b"]],
                                      bounds_SVRspell_tpow = snakemake@params[["bounds_SVRspell_tpow"]])
# Write list object as RDS
saveRDS(bounds, file = snakemake@output[[1]])