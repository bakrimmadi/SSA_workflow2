##### 20250314 Workflow sequence analysis : 6 - generate_seq_yaml #####
###

##### Description de la fonction #####
# Fonction : exporte un fichier yaml comportant la structure de la table datavzrd

# Entrées
#     csv_file : Base de donnes au format séquentiel avec le nom des clusters
# Sortie
#     Création d'un fichier yaml pour utiliser datavzrd###

###### Snakemake log######
log <- file(snakemake@log[[1]], open = "wt")
sink(log)
sink(log, type="message")

library(yaml)
library(readr)
library(dplyr)
# Snakemake gère les fichiers d'entrée et de sortie
csv_file <- snakemake@input[[1]]  # Fichier CSV en entrée
output_yaml <- snakemake@output[[1]]  # Fichier YAML en sortie
delimiter <- ";"  # Ajustez si nécessaire
use_yte <- TRUE
# Charger le fichier CSV pour détecter les colonnes
df <- read_delim(csv_file, delim = delimiter) %>%
  dplyr::rename(Cluster=seq.part_dynamic_diss_best)%>%
  mutate(across(everything(), ~na_if(., "%")))

# Construire la structure du YAML
config <- list(
  "__use_yte__" = use_yte,
  "datasets" = list(
    "seq" = list(
      "path" = "?input.table",
      "separator" = delimiter
    )
  ),
  "views" = list(
    "seq" = list(
      "dataset" = "seq",
      "render-table" = list(
        "columns" = list()
      )
    )
  )
)
# Ajouter les colonnes après dataset et render-table
for (col in names(df)) {
  config$views$seq$`render-table`$columns[[col]] <- list(
    "plot" = list(
      "heatmap" = list(
        "scale" = "ordinal",
        "color-scheme" = "category10"
      )
    )
  )
}

# Enregistrer le fichier YAML
write_yaml(config, output_yaml)

cat(sprintf("File %s has been created !\n", output_yaml))
