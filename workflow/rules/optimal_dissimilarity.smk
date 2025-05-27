##### Select the best dissimilarity with the best parameters#####
rule Optimal_dissimilarity5:  
    input:
        "resources/data/seq.rds",
        "resources/data/dissimilarity_vector.csv",
        "resources/data/bounds.rds"
    output:
        "resources/data/matrix_diss_best.rds",
        "resources/data/best_param_diss.csv"
    log:
        "logs/optimal_dissimilarity.log"
    params:
        min_cluster_size = config["min_cluster_size"]
    conda:
        "../envs/optimal_dissimilarity.yaml"
    script:
        "../scripts/Optimal_dissimilarity5.R"