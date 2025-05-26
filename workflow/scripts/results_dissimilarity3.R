##### 20230910 Workflow sequence analysis : 5 - results_dissimilarity #####
###

##### Description de la fonction #####
# Fonction : results_dissimilarity --> Calcul et export des résultats pour chaque matrice de dissimilarité

# Entrées
#     bdd : Base de donnes au format séquentiel
#     matrice_seqdist : matrice ou liste de matrices de dissimilarités
#     best_params_diss : dataframe contenant les paramètres pour chaque matrice de dissimilarité utilisée
#     path_result : chemin indiquant où exporter les résultats
# Sortie
#     Création d'un dossier contenant : bdd avec variable cluster, dissimilarité et paramètres retenues, matrice de transitionheatmap, iplot, fplot, top 10 des fréquences
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

library(TraMineR)
library(dynamicTreeCut)
library(dplyr)
library(tibble)
library(lubridate)
library(writexl)
library(seqhandbook)
library(WeightedCluster)
library(janitor)
#####                     Paramètres                       #####
results_dissimilarity <- function(bdd,
                                  matrice_seqdist = NULL,
                                  best_param_diss = NULL,
                                  path_result = NULL,
                                  width = 7,
                                  height = 7) {
  if (is.list(matrice_seqdist) == FALSE) {
    matrice_seqdist <- list(matrice_seqdist)
  }
  for (i in length(matrice_seqdist)) {
    seq.dist_diss_best <-
      hclust(as.dist(matrice_seqdist[[i]]), method = "ward.D2")
    #Dynamic cut tree
    # if (nrow(bdd) <= 100){
    #   min_size <- 10}
    # else {
    #   min_size <- ceiling(nrow(bdd)*0.05)}
    # seq.part_dynamic_diss_best <- cutreeDynamic(seq.dist_diss_best,
    #                                             distM = matrice_seqdist[[i]],
    #                                             minClusterSize = min_size)
    # seq.part_dynamic_diss_best <- factor(seq.part_dynamic_diss_best,
    #                                      labels = paste("classe", 1:nlevels(factor(seq.part_dynamic_diss_best)),
    #                                                     sep = "."))
    if (nrow(bdd) < 100) {
      seq.part_dynamic_base <-data.frame(Cluster =cutreeDynamic(seq.dist_diss_best, 
                                                                distM = matrice_seqdist[[i]], 
                                                                minClusterSize = 10,
                                                                respectSmallClusters=TRUE))
    }
    if (nrow(bdd) >= 100 & nrow(bdd) < 1000) {
      seq.part_dynamic_base <-data.frame(Cluster =cutreeDynamic(seq.dist_diss_best, 
                                                                distM = matrice_seqdist[[i]], 
                                                                minClusterSize = 30,
                                                                respectSmallClusters=TRUE))
    }
    if (nrow(bdd) >= 1000) {
      seq.part_dynamic_base <-data.frame(Cluster =cutreeDynamic(seq.dist_diss_best, 
                                                                distM = matrice_seqdist[[i]], 
                                                                minClusterSize = 100,
                                                                respectSmallClusters=TRUE))
    }
    
    ###Reclassement des valeurs qui n'ont pas réussi à être assignés à un cluster
    #ASW par cluster
    clustqual_base <- wcClusterQuality(matrice_seqdist[[i]], seq.part_dynamic_base$Cluster)
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
    seq.part_dynamic_diss_best_NR <- factor(seq.part_dynamic0$Cluster2)
    seq.part_dynamic_diss_best <- factor(seq.part_dynamic0$Cluster_reclasse2) 
    #### Results to stock ####
    #Bdd with cluster variable
    Bdd_cluster <- cbind(bdd, seq.part_dynamic_diss_best)
    
    #Transition matrix
    seq.trate <- seqtrate(bdd) %>%
      data.frame() %>%
      rownames_to_column()
    
    # Creating a folder to store results
    dir.create(path_result, showWarnings = FALSE)
    #Export of tables
    if (is.null(best_param_diss)) {
      list.xls <- list(bdd_cluster = Bdd_cluster,
                       transition_matrix = seq.trate)
    }
    else {
      list.xls <- list(
        bdd_cluster = Bdd_cluster,
        best_dissimilarity = best_param_diss[i,] %>%
          remove_empty("cols"),
        transition_matrix = seq.trate
      )
    }
    ###### Export excel #####
    write_xlsx(list.xls,
               path = paste0(path_result,"bdd_cluster_dissimilarity_", i, ".xlsx"))
    ###### Export plots ######
    
    # Boucle pour exporter des PDF avec différentes tailles de page
    tryCatch({
      # Exporte le PDF avec la taille de page actuelle
      pdf(
        paste0(path_result, "results_plots_", i, ".pdf"),
        width = width,
        height = height
      )
      ###### Heatmap ######
      seq_heatmap(bdd,
                  seq.dist_diss_best)
      ###### Heatmap par clusters sans reclassement ######
      if ("Classe 0" %in% seq.part_dynamic_diss_best_NR){
        seqIplot(bdd, 
                 group = seq.part_dynamic_diss_best_NR, 
                 space = 0, 
                 border = NA, 
                 yaxis = FALSE,
                 cex.legend=0.8)
      }
      ###### Heatmap par clusters avec reclassement si necessaire######
      seqIplot(bdd, 
               group = seq.part_dynamic_diss_best, 
               space = 0, 
               border = NA, 
               yaxis = FALSE,
               cex.legend=0.8)
      
      ###### Top 10 most frequent sequences ######
      seqfplot(
        bdd,
        weighted = FALSE,
        border = NA,
        main = "Top 10 most frequent sequences",
        with.legend = "right",
        cex.legend = 0.8
      )
      dev.off()
      
    }, error = function(e) {
      pdf(
        paste0(path_result, "results_plots_", i, ".pdf"),
        width = 10,
        height = 20
      )
      ###### Heatmap ######
      seq_heatmap(bdd,
                  seq.dist_diss_best)
      ###### Heatmap par clusters sans reclassement ######
      if ("Classe 0" %in% seq.part_dynamic_diss_best_NR){
        seqIplot(bdd, 
                 group = seq.part_dynamic_diss_best_NR, 
                 space = 0, 
                 border = NA, 
                 yaxis = FALSE,
                 cex.legend=0.8)
      }
      ###### Heatmap par clusters ######
      seqIplot(bdd, 
               group = seq.part_dynamic_diss_best, 
               space = 0, 
               border = NA, 
               yaxis = FALSE,
               cex.legend=0.8)
      
      ###### Top 10 most frequent sequences ######
      seqfplot(
        bdd,
        weighted = FALSE,
        border = NA,
        main = "Top 10 most frequent sequences",
        with.legend = "right",
        cex.legend = 0.8
      )
      dev.off()

    })
  }
}
###### Utilisation des fonctions avec Snakemake ######
seq <- readRDS(snakemake@input[[1]])

matrice_seqdist <- readRDS(snakemake@input[[2]])

best_param_diss <- read.csv2(snakemake@input[[3]])


###### Export des résultats ######
result_test <- results_dissimilarity(
  bdd = seq,
  matrice_seqdist = matrice_seqdist,
  best_param_diss = best_param_diss,
  path_result = paste0(snakemake@params[["path_result"]],"/"),
  width = 7,
  height = 7
)
