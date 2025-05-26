##### Import a csv or excel file then convert to seqdef format#####
rule Import_then_seqdef:  
    input:
        config["samples"]
    output:
        "resources/data/seq.rds"  
    log:
        "logs/import_then_seqdef.log"
    conda:
        "../envs/import_then_seqdef.yaml"
    script:
        "../scripts/import_then_seqdef.R"