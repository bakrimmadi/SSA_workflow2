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
    
    # Vérifier la condition pour les fichiers _NR
    export_NR <- "Classe 0" %in% seq.part_dynamic_diss_best_NR
    
    # 🔹 Définir les graphiques à exporter
    plots <- list(
      "seqfplot_top10" = function() seqfplot(bdd, weighted = FALSE, border = NA, main = "Top 10 most frequent sequences",
                                             with.legend = "right", cex.legend = 0.8),
      
      "seq_heatmap" = function() seq_heatmap(bdd, seq.dist_diss_best),
      
      "seqIplot" = function() seqIplot(bdd, group = seq.part_dynamic_diss_best, space = 0, border = NA, yaxis = FALSE, cex.legend = 0.8),
      
      "seqdplot" = function() seqdplot(bdd, group = seq.part_dynamic_diss_best, space = 0, border = NA, yaxis = FALSE, cex.legend = 0.8),
      
      "seqfplot" = function() seqfplot(bdd, group = seq.part_dynamic_diss_best, weighted = FALSE, border = NA, 
                                       with.legend = "right", cex.legend = 0.8),
      "seqmtplot" = function() seqmtplot(bdd, group = seq.part_dynamic_diss_best, weighted = FALSE, border = NA,
                                         with.legend = "right", cex.legend = 0.8),
      "seqrplot" = function() seqrplot(bdd, group = seq.part_dynamic_diss_best, dist.matrix = as.dist(matrice_seqdist[[i]]), criterion = "dist"),
      "seqHtplot" = function() seqHtplot(bdd, group = seq.part_dynamic_diss_best, weighted = FALSE, border = NA,
                                         with.legend = "right", cex.legend = 0.8)
    )
    
    # 🔹 Ajouter les graphiques _NR si la condition est vérifiée
    if (export_NR) {
      plots <- c(plots, list(
        "seqIplot_NR" = function() seqIplot(bdd, group = seq.part_dynamic_diss_best_NR, space = 0, border = NA, yaxis = FALSE, cex.legend = 0.8),
        
        "seqdplot_NR" = function() seqdplot(bdd, group = seq.part_dynamic_diss_best_NR, space = 0, border = NA, yaxis = FALSE, cex.legend = 0.8),
        
        "seqfplot_NR" = function() seqfplot(bdd, group = seq.part_dynamic_diss_best_NR, weighted = FALSE, border = NA, 
                                            with.legend = "right", cex.legend = 0.8),
        "seqmtplot_NR" = function() seqmtplot(bdd, group = seq.part_dynamic_diss_best_NR, weighted = FALSE, border = NA,
                                              with.legend = "right", cex.legend = 0.8),
        "seqrplot_NR" = function() seqrplot(bdd, group = seq.part_dynamic_diss_best_NR, dist.matrix = as.dist(matrice_seqdist[[i]]), criterion = "dist"),
        "seqHtplot_NR" = function() seqHtplot(bdd, group = seq.part_dynamic_diss_best_NR, weighted = FALSE, border = NA,
                                              with.legend = "right", cex.legend = 0.8)
      ))
    }
    
    # Boucle pour exporter des PDF avec différentes tailles de page
    tryCatch({
      # Exporte le PDF avec la taille de page actuelle
      pdf(
        paste0(path_result, "results_plots_", i, ".pdf"),
        width = width,
        height = height
      )
      for (plot_name in names(plots)) {
        plots[[plot_name]]()  # 🔹 Exécuter la fonction graphique
        Sys.sleep(1)  # 🔹 Laisser le temps au rendu
      }
      
      dev.off()
      
    }, error = function(e) {
      pdf(
        paste0(path_result, "results_plots_", i, ".pdf"),
        width = 10,
        height = 20
      )    
      for (plot_name in names(plots)) {
        plots[[plot_name]]()  # 🔹 Exécuter la fonction graphique
        Sys.sleep(1)  # 🔹 Laisser le temps au rendu
      }
      
    })
    
    # 🔹 Exportation des graphiques en PNG
    for (plot_name in names(plots)) {
      png(paste0(path_result, plot_name, ".png"), width = 700, height = 500, res = 100)
      plots[[plot_name]]()  # Exécuter la fonction graphique
      dev.off()  # Sauvegarder l'image
    }
  }
  return(list(bdd_cluster = Bdd_cluster))
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

bddcluster <-result_test$bdd_cluster %>%
  data.frame() %>%
  dplyr::rename(Cluster=seq.part_dynamic_diss_best)%>%
  dplyr::mutate(across(everything(), ~as.character(.))) %>%
  dplyr::mutate(across(everything(), ~na_if(., "%")))

write.csv2(bddcluster,snakemake@output[[1]], row.names = FALSE, na="")