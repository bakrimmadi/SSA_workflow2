##### Calculation and export of results for each dissimilarity matrix #####

rule results_dissimilarity:  
    input:
        "data/seq.rds",
        "data/matrix_diss_best.rds",
        "data/best_param_diss.csv"
    output:
        report(
            os.path.join(config["path_result"],"results_plots_1.pdf"),
            caption="../report/heatmap.rst",
            category="Results",
        )
    params:
        path_result = config["path_result"]
    log:
        "logs/results_dissimilarity/results_dissimilarity.log"
    conda:
        "../envs/results_dissimilarity.yaml"
    script:
        "../scripts/results_dissimilarity3.R"


