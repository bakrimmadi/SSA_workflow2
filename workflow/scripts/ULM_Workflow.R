rm(list = ls())

#### Chargement des fonctions necessaires ####
source("5 - Script/install_missing_packages.R")
source("5 - Script/Import_then_seqdef.R")
source("5 - Script/dissimilarity_choice2.R")
source("5 - Script/params_dissimilarity_choice.R")
source("5 - Script/Optimal_dissimilarity5.R")
source("5 - Script/results_dissimilarity3.R")

Tab_Univ <- read_csv("../data/20240307 Tab_Univ.csv") %>%
  dplyr::select(Id,Delai,Gaz) %>%
  dplyr::mutate(Id=factor(Id))

Tab_univ_max_Delai <-Tab_Univ %>%
  dplyr::group_by(Id) %>%
  dplyr::summarise(Delai_max = max(Delai))

Tab_Univ_complete <- Tab_Univ %>%
  dplyr::mutate(Delai=factor(Delai, levels=min(Delai):max(Delai)))%>%
  tidyr::complete(Id, Delai) %>%
  dplyr::left_join(Tab_univ_max_Delai,by="Id")
  
Tab_Univ_complete_finale <- Tab_Univ_complete %>%
  dplyr::mutate(Delai=as.numeric(Delai)-1) %>%
  dplyr::filter(Delai<=Delai_max) %>%
  dplyr::arrange(Id,Delai) %>%
  tidyr::fill(Gaz) %>%
  dplyr::select(-Delai_max)
write.csv2(Tab_Univ_complete_finale,"0 - Base/20240312 Tab_Univ_complete_finale.csv", row.names = FALSE, na="")
#Workflow pour ULM

# bdd_seq <-
#   import_before_seqdef(
#     chemin_bdd = "0 - Base/20240307 Tab_Univ.csv",
#     bdd_format = "Long",
#     var_id = "Id",
#     var_rep = "Gaz"
#   )

bdd_seq <-
  import_before_seqdef(
    chemin_bdd = "0 - Base/20240312 Tab_Univ_complete_finale.csv",
    bdd_format = "Long",
    var_id = "Id",
    var_rep = "Gaz"
  )
#Modif bdd pour avoir une ligne par 
seq <- convert_seqdef(bdd = bdd_seq,
                      labels = c("AIR", "NOXBouteille","NOXCRABE","Oxygen"))


#### Choix des critères pour déterminer la ou les dissimilarités à tester ####
dissimilarite<-dissimilarity_choice(sequencement = TRUE,duree_etats = TRUE)
#### Choix des bornes limites pour chaque hyperparamètre de chaque dissimilarité ####
bounds<-params_dissimilarity_choice(dissimilarite = dissimilarite)
#### Choix de la meilleure dissimilarité ####
optimal_test <- optimal_dissimilarity(seq,
                                      dissimilarite = dissimilarite,
                                      bounds = bounds)
###### Export des résultats ######
result_test <- results_dissimilarity(
  bdd = seq,
  matrice_seqdist = optimal_test$seq_diss_best,
  best_param_diss = optimal_test$best_param_diss,
  path_result = paste0("3 - Résultats/",today(),"_results_workflow_sequence_ULM_SVR",  "/"),
  width = 7,
  height = 7
)

