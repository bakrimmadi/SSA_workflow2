##### Calculation and export of results for each dissimilarity matrix #####

rule results_dissimilarity:  
    input:
        "resources/data/seq.rds",
        "resources/data/matrix_diss_best.rds",
        "resources/data/best_param_diss.csv"
    output:
        report(
            "results/plots/seqfplot_top10.png",
            category="Plots",
            labels={"plot": "fplot_top10", "approach": "seqfplot_top10"},
            caption="../report/seqfplot_top10.rst",
        ),
        report(
            "results/plots/seq_heatmap.png",
            category="Plots",
            labels={"plot": "heatmap", "approach": "seq_heatmap"},
            caption="../report/seq_heatmap.rst",
        ),
        report(
            "results/plots/seqIplot.png",
            category="Plots",
            labels={"plot": "Iplot", "approach": "seqIplot"},
            caption="../report/seqIplot.rst",
        ),
        report(
            "results/plots/seqdplot.png",
            category="Plots",
            labels={"plot": "dplot", "approach": "seqdplot"},
            caption="../report/seqdplot.rst",
        ),
        report(
            "results/plots/seqfplot.png",
            category="Plots",
            labels={"plot": "fplot", "approach": "seqfplot"},
            caption="../report/seqfplot.rst",
        ),
        report(
            "results/plots/seqmtplot.png",
            category="Plots",
            labels={"plot": "mtplot", "approach": "seqmtplot"},
            caption="../report/seqmtplot.rst",
        ),
        report(
            "results/plots/seqrplot.png",
            category="Plots",
            labels={"plot": "rplot", "approach": "seqrplot"},
            caption="../report/seqrplot.rst",
        ),
        report(
            "results/plots/seqHtplot.png",
            category="Plots",
            labels={"plot": "Htplot", "approach": "seqHtplot"},
            caption="../report/seqHtplot.rst",
        ),
    log:
        "logs/results_dissimilarity.log"
    conda:
        "../envs/results_dissimilarity.yaml"
    script:
        "../scripts/results_dissimilarity4.R"

