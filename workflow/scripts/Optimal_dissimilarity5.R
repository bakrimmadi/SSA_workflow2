##### 20230910 Workflow sequence analysis : 4 - optimal_dissimilarity #####
###

##### Description de la fonction #####
# Fonction : optimal_dissimilarity --> Compare les traitements selon les dissimilarités en optimisant le choix des hyperparamètres par bayesian optimisation

# Entrées
#     bdd : Base de donnes au format séquentiel
#     dissimilarite : Vecteur contentant le nom des dissimilarites retenues
#     bounds : liste contenant les plages de valeurs des hyperparamètres à faire varier pour chaque dissimilarité

# Sortie
#     seq_diss_best : matrice de dissimilarité calculé à partir des meilleurs hyperparamètres
#     best_params_diss : dataframe contenant les meilleurs hyperparamètres pour la dissimilarité retenue
###

###### Snakemake log######
log <- file(snakemake@log[[1]], open = "wt")
sink(log)
sink(log, type="message")

#####                      LIBRARY                        #####
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
install_missing_packages("seqhandbook")
###
library(dynamicTreeCut)
library(WeightedCluster)
library(ParBayesianOptimization)#Pour linux : sudo apt install cmake
library(fpc)
library(tibble)
library(janitor)
library(seqhandbook)
#####                     Paramètres                       #####
optimal_dissimilarity <- function(bdd,
                                  dissimilarite,
                                  bounds,
                                  min_cluster_size = NULL) {
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
  ###### hyperparameter bounds : HAM ######
  # bounds_HAM <- list(seqsubm = c(0L, 4L)) #Ne fonctionne pas
  ###### hyperparameter bounds : LCS ######
  bounds_LCS <- bounds[["LCS"]]
  ###### hyperparameter bounds : CHI2 ######
  bounds_CHI2 <- bounds[["CHI2"]]#list(k = c(2L, 20L))
  ###### hyperparameter bounds : EUCLID ######
  bounds_EUCLID <- bounds[["EUCLID"]]
  ###### hyperparameter bounds : OM ######
  bounds_OM <- bounds[["OM"]]
  ###### hyperparameter bounds : OMstran ######
  bounds_OMstran <- bounds[["OMstran"]]
  ###### hyperparameter bounds : OMspell ######
  bounds_OMspell <- bounds[["OMspell"]]
  ###### hyperparameter bounds : SVRspell ######
  bounds_SVRspell <- bounds[["SVRspell"]]

scoringFunction <-
  function(dissimilarite=diss, seqsubm, expcost,
           a, b, tpow,
           transindel,otto) {
    
    if (dissimilarite == "OMspell" |
        dissimilarite == "OM") {
      if (seqsubm == 0) {
        couts <- seqsubm(bdd, method = "CONSTANT")
      }
      else if (seqsubm == 1) {
        couts <- seqsubm(bdd, method = "INDELS")
      }
      else if (seqsubm == 2) {
        couts <- seqsubm(bdd, method = "FUTURE")
      }
      else if (seqsubm == 3) {
        couts <- seqsubm(bdd, method = "TRATE")
      }
      else if (seqsubm == 4) {
        couts <- seqsubm(bdd, method = "INDELSLOG")
      }
      if (dissimilarite == "OMspell") {
        seq.diss <-
          seqdist(bdd,
                  method = dissimilarite,
                  sm = couts,
                  expcost = expcost)
      }
      if (dissimilarite == "OM") {
        seq.diss <-
          seqdist(bdd,
                  method = dissimilarite,
                  sm = couts)
      }
      # else if (seqsubm == 5) {couts <- seqsubm(seq, method = "FEATURES")} #Ne fonctionne pas
    }
    else if (dissimilarite == "LCS" | 
             dissimilarite == "EUCLID" |
             dissimilarite == "CHI2") {
      ###### Choix de la méthode pour la matrice de dissimilarité ######
      #Matrice de distance
      seq.diss <-
        seqdist(bdd,
                method = dissimilarite)
    }
    else if (dissimilarite == "SVRspell") {
      ###### Choix de la méthode pour la matrice de dissimilarité ######
      #Matrice de distance
      seq.diss <-
        seqdist(
          bdd,
          method = dissimilarite,
          a = a,
          b = b,
          tpow = tpow
        )
    }
    else if (dissimilarite == "OMstran") {
      if (seqsubm == 0) {
        couts <- seqsubm(bdd, method = "CONSTANT")
      }
      else if (seqsubm == 1) {
        couts <- seqsubm(bdd, method = "INDELS")
      }
      else if (seqsubm == 2) {
        couts <- seqsubm(bdd, method = "FUTURE")
      }
      else if (seqsubm == 3) {
        couts <- seqsubm(bdd, method = "TRATE")
      }
      else if (seqsubm == 4) {
        couts <- seqsubm(bdd, method = "INDELSLOG")
      }
      # else if (seqsubm == 5) {couts <- seqsubm(seq, method = "FEATURES")} #Ne fonctionne pas
      if (transindel == 0) {
        transmethod = "constant"
      }
      else if (transindel == 1) {
        transmethod = "subcost"
      }
      else if (transindel == 2) {
        transmethod = "prob"
      }

      ###### Choix de la méthode pour la matrice de dissimilarité ######
      #Matrice de distance
      seq.diss <-
        seqdist(
          bdd,
          method = dissimilarite,
          sm = couts,
          transindel = transmethod,
          otto = otto
        )
    }
    #Hierarchical clustering
    seq.dist_diss <- hclust(as.dist(seq.diss), method = "ward.D2")
    #Dynamic cut tree
    # Appliquer les règles si min_cluster_size est non spécifié
    if (is.null(min_cluster_size)) {
      n <- nrow(bdd)
      min_cluster_size <- dplyr::case_when(
        n < 100    ~ 10,
        n < 1000   ~ 30,
        TRUE       ~ 100
      )
    }
    # Clustering dynamique
    seq.part_dynamic_base <- data.frame(
      Cluster = cutreeDynamic(
        seq.dist_diss,
        distM = seq.diss,
        minClusterSize = min_cluster_size,
        respectSmallClusters = TRUE
      )
    )
    ###Reclassement des valeurs qui n'ont pas réussi à être assignés à un cluster
    #ASW par cluster
    clustqual_base <- wcClusterQuality(seq.diss, seq.part_dynamic_base$Cluster)
    table_ASW_cluster <- seq.part_dynamic_base %>%
      dplyr::left_join(data.frame(clustqual_base$ASW) %>%
                         tibble::rownames_to_column("Cluster") %>%
                         dplyr::mutate(Cluster=as.numeric(Cluster)),
                       by="Cluster") 
    #Cluster ayant l'ASW minimal
    Cluster_min <- table_ASW_cluster %>%
      dplyr::filter(Cluster!=0) %>%
      dplyr::filter(ASW == min(table_ASW_cluster$ASW[table_ASW_cluster$Cluster!=0])) %>%
      dplyr::mutate(Cluster=as.numeric(Cluster)) %>%
      dplyr::select(Cluster) %>%
      unique()
    
    seq.part_dynamic0 <- table_ASW_cluster %>%
      dplyr::mutate(Cluster2=paste("Classe", Cluster),
                    Cluster_reclasse=replace(Cluster,Cluster==0,Cluster_min),
                    Cluster_reclasse2=paste("Classe", Cluster_reclasse))
    
    #Vecteurs avec les label des clusters
    seq.part_dynamic_NR <- factor(seq.part_dynamic0$Cluster2)
    seq.part_dynamic <- factor(seq.part_dynamic0$Cluster_reclasse2) 
    # #Stability measurement
    # The vector of cluster stabilities.
    # Values close to 1 indicate stable clusters
    if (length(seq.diss) <= 1000000) {
      cboot.hclust <-
        clusterboot(
          as.dist(seq.diss),
          B = 100,
          clustermethod = hclustCBI,
          method = "ward.D2",
          k = nlevels(seq.part_dynamic),
          bootmethod = "boot",
          seed = 15555,
          count = TRUE
        )
    }
    else {
      cboot.hclust <- data.frame(bootmean = 0,
                                 msg = "clusterboot not used",
                                 size = length(seq.diss))
    }
    
    
    #Alggorithme PAM medoid
    #pamward_diss <- wcKMedoids(seq.diss, k = nlevels(seq.part_dynamic), initialclust = seq.dist_diss)
    if (nlevels(seq.part_dynamic) == 1) {
      clustqual <-list(stats=c("ASW"=0,"HC"=0,"ASWw"=0,"HG"=0,"CH"=0))
      Score_euclid = 1
    }
    else{
      #Qualité du clustering : ASW
      clustqual <- wcClusterQuality(seq.diss, seq.part_dynamic)
      Score_euclid = max(sqrt(
        clustqual$stats["ASW"]^2 + mean(cboot.hclust$bootmean)^2 + (1 - clustqual$stats["HC"])^2))
    }
    Score_old = max(2 * clustqual$stats["ASW"] + mean(cboot.hclust$bootmean) - clustqual$stats["HC"])
    if ("Classe 0" %in% seq.part_dynamic_NR){
      Score_euclid = Score_euclid-0.5
      Score_old = Score_old-0.5
    }
    return(
      list(
        # Score = max(pamward_diss$stats["ASW"]-pamward_diss$stats["HC"]),
        # Score_ASW = max(pamward_diss$stats["ASW"]),
        # Score_HC = max(pamward_diss$stats["HC"])
        Score_old = Score_old,
        Score = Score_euclid,
        Score_ASW = max(clustqual$stats["ASW"]),
        Score_HC = max(clustqual$stats["HC"]),
        Score_bootmean = mean(cboot.hclust$bootmean),
        nb_cluster = nlevels(seq.part_dynamic)
      )
    )
  }

#### Bayesian Optimization for each dissimilarity ####
set.seed(15555)
for (diss in dissimilarite) {
  if (length(get(paste0("bounds_", diss))) == 0) {
    scoringFunction_result <-
      do.call("scoringFunction",
              list(dissimilarite=diss))
    assign(
      paste0("best_param_", diss),
      data.frame(
        dissimilarite = diss,
        Epoch = NA,
        Iteration = NA,
        Score = scoringFunction_result$Score,
        Score_old = scoringFunction_result$Score_old,
        Score_ASW = scoringFunction_result$Score_ASW,
        Score_HC = scoringFunction_result$Score_HC,
        Score_bootmean = scoringFunction_result$Score_bootmean,
        nb_cluster = scoringFunction_result$nb_cluster
      )
    )
    
  }
  else {
    tNoPar <- system.time(assign( 
      paste0("optObj_", diss),
      bayesOpt(
        FUN = scoringFunction,
        bounds = get(paste0("bounds_", diss)),
        initPoints = 4,
        iters.n = 20,
        iters.k = 1
      )
    ))
    optObj <- get(paste0("optObj_", diss))
    scoreSummary <- optObj[["scoreSummary"]]
    
    # Vérification de la variance de Score
    if (!"Score" %in% names(scoreSummary)) {
      stop("Colonne 'Score' manquante dans scoreSummary.")
    }
    
    if (var(scoreSummary$Score) == 0) {
      warning("Score constant : pas de variance. Jointure avec scoreSummary désactivée.")
      best_param <- getBestPars(optObj) %>%
        as.data.frame() %>%
        dplyr::mutate(dissimilarite = diss,min_cluster_size_used = min_cluster_size)
    } else {
      best_param <- getBestPars(optObj) %>%
        as.data.frame() %>%
        dplyr::mutate(dissimilarite = diss,min_cluster_size_used = min_cluster_size) %>%
        dplyr::inner_join(scoreSummary, by = intersect(names(.), names(scoreSummary))) %>%
        dplyr::select(
          -gpUtility,
          -acqOptimum,
          -inBounds,
          -Elapsed,
          -errorMessage
        )
    }
    
    assign(paste0("best_param_", diss), best_param)
    
  }
}
#### Best parameters ####
best_param_list <- mget(ls(pattern = "best_param_.*"))

if (length(best_param_list) >= 1) {
  best_param_diss0 <-
    dplyr::bind_rows(best_param_list)  #     purrr::reduce(full_join) %>%
  best_param_diss1 <- best_param_diss0 %>%
    dplyr::filter(Score == max(Score, na.rm = TRUE)) %>%
    dplyr::arrange(nb_cluster) %>%
    dplyr::slice(1) #Au cas où il y ait des scores identiques, on prend celui avec le moins de clusters générés
}
# else if (length(best_param_list) == 1) {
#   best_param_diss0 <- best_param_list %>%
#     as_tibble()
# }
else {
  stop("Error best params is empty")
}

###### Spécificités de chaque dissimilarité ######
#On refait tourner avec les meilleurs paramètres pour stocker les résultats
if (best_param_diss1$dissimilarite == "OMspell" |
    best_param_diss1$dissimilarite == "OM" #| best_param_diss1$dissimilarite == "HAM"
) {
  best_param_diss <- best_param_diss1 %>%
    dplyr::mutate(seqsubm = factor(
      seqsubm,
      levels = 0:4,
      labels = c("CONSTANT", "INDELS", "FUTURE",
                 "TRATE", "INDELSLOG")
    ))
  
  seq_diss_best <- seqdist(
    bdd,
    method = best_param_diss$dissimilarite,
    sm = seqsubm(bdd, method = best_param_diss$seqsubm),
    expcost = best_param_diss$expcost
  )
}
else if (best_param_diss1$dissimilarite == "LCS" |
         best_param_diss1$dissimilarite == "EUCLID" |
         best_param_diss1$dissimilarite == "CHI2") {
  best_param_diss <- best_param_diss1
  
  seq_diss_best <- seqdist(bdd,
                           method = best_param_diss$dissimilarite)
}
else if (best_param_diss1$dissimilarite == "SVRspell") {
  best_param_diss <- best_param_diss1
  
  seq_diss_best <- seqdist(bdd,
                           method = best_param_diss$dissimilarite,
                           a = best_param_diss$a,
                           b = best_param_diss$b,
                           tpow = best_param_diss$tpow)
}
else if (best_param_diss1$dissimilarite == "OMstran") {
  best_param_diss <- best_param_diss1 %>%
    dplyr::mutate(
      seqsubm = factor(
        seqsubm,
        levels = 0:4,
        labels = c("CONSTANT", "INDELS", "FUTURE",
                   "TRATE", "INDELSLOG")
      ),
      transindel = factor(
        transindel,
        levels = 0:2,
        labels = c("constant", "subcost", "prob")
      )
    )
  
  seq_diss_best <- seqdist(
    bdd,
    method = best_param_diss$dissimilarite,
    sm = seqsubm(bdd, method = best_param_diss$seqsubm),
    transindel = best_param_diss$transindel,
    otto = best_param_diss$otto
  )
}
return(list(seq_diss_best = seq_diss_best,
            best_param_diss = best_param_diss))
}

###### Utilisation des fonctions avec Snakemake ######
seq <- readRDS(snakemake@input[[1]])

dissimilarite0 <- read.csv2(snakemake@input[[2]])
dissimilarite <- dissimilarite0$dissimilarite

bounds <- readRDS(snakemake@input[[3]])
#### Choix de la meilleure dissimilarité ####
optimal_test <- optimal_dissimilarity(seq,
                                      dissimilarite = dissimilarite,
                                      bounds = bounds,
                                      min_cluster_size = snakemake@params[["min_cluster_size"]])
# Write matrix object as RDS
saveRDS(optimal_test$seq_diss_best, file = snakemake@output[[1]])
#Export du vecteur de la meilleure dissimilarité avec ses paramètres
write.csv2(optimal_test$best_param_diss,snakemake@output[[2]], row.names = FALSE, na="")
