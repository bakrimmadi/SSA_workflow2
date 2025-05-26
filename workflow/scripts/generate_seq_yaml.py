import pandas as pd
import yaml
import sys

# Snakemake gère les fichiers d'entrée et de sortie
csv_file = snakemake.input[0]  # Fichier CSV en entrée
output_yaml = snakemake.output[0]  # Fichier YAML en sortie
delimiter = ";"  # Ajustez si nécessaire

# Charger le fichier CSV pour détecter les colonnes
df = pd.read_csv(csv_file, delimiter=delimiter)

# Construire la structure du YAML
config = {
    "__use_yte__": True,  # Ensure __use_yte__ is a boolean
    "datasets": {
        "seq": {
            "path": "?input.table",
            "separator": delimiter
        }
    },
    "views": {
        "seq": {
            "dataset": "seq",
            "render-table": {
                "columns": {}
            }
        }
    }
}

# Ajouter les colonnes après dataset et render-table
for col in df.columns:
    config["views"]["seq"]["render-table"]["columns"][col] = {
        "plot": {
            "heatmap": {
                "scale": "ordinal",
                "color-scheme": "category10"
            }
        }
    }

# Enregistrer le fichier YAML
with open(output_yaml, "w") as yaml_file:
    yaml.dump(config, yaml_file, default_flow_style=False, sort_keys=False)

print(f"Fichier {output_yaml} has been created !")
