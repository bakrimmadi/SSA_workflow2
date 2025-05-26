##### 20230913 Workflow sequence analysis #####
###
rm(list = ls())
#### Chargement des fonctions necessaires ####
source("5 - Script/Import_then_seqdef.R")
#source("5 - Script/scoringFunction.R")
source("5 - Script/dissimilarity_choice.R")
source("5 - Script/params_dissimilarity.R")
source("5 - Script/Optimal_dissimilarity5.R")
source("5 - Script/results_dissimilarity2.R")
#### Import de la bdd ####
# bdd<-import_then_seqdef(chemin_bdd = "0 - Base/Test_csv.csv",
#                                           bdd_format = "Long",
#                                           var_id = "Donneur",
#                                           var_rep = "Hemo",
#                                           labels = c("Chomage","Emploi","Retraite"))
bdd_seq <-
  import_before_seqdef(
    chemin_bdd = "0 - Base/base_hemo_2.xlsx",
    bdd_format = "Long",
    var_id = "DONNEUR",
    var_rep = "HEMO"
  ) 
# %>%
#   slice(1:200)
seq <- convert_seqdef(bdd = bdd_seq,
                      labels = c("Négatif", "Positif"))
#### Choix des critères pour déterminer la ou les dissimilarités à tester ####
dissimilarite<-dissimilarity_choice(sequencement = TRUE)
#### Choix des bornes limites pour chaque hyperparamètre de chaque dissimilarité ####
bounds<-params_dissimilarity()
#### Choix de la meilleure dissimilarité ####
optimal_test <- optimal_dissimilarity(seq,
                                      dissimilarite = "SVRspell",
                                      bounds = bounds)
###### Export des résultats ######
result_test <- results_dissimilarity(
  bdd = seq,
  matrice_seqdist = optimal_test$seq_diss_best,
  best_param_diss = optimal_test$best_param_diss,
  path_result = paste0("3 - Résultats/",today(),"_results_workflow_sequence",  "/"),
  width = 7,
  height = 7
)

library(igraph)# Pour linux : sudo apt install libglpk-dev libgmp-dev libxml2-dev
library(targets)
#tar_script()
#tar_edit()
tar_manifest(fields = all_of("command"))
tar_make()
tar_visnetwork()

tar_meta(fields = error, complete_only = TRUE)
