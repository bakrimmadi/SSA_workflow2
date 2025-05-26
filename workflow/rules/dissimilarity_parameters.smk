##### Choice of dissimilarities according to the criteria of interest #####
rule dissimilarity_choice:  
    output:
        "resources/data/dissimilarity_vector.csv"
    log:
        "logs/dissimilarity_choice.log"
    params:
        expert = config["expert"],
        dissimilarite_expert = config["dissimilarite_expert"],
        fusion_dissimilarites = config["fusion_dissimilarites"],
        sequencement = config["choix_criteres"]["sequencement"],
        timing = config["choix_criteres"]["timing"],
        duree_etats = config["choix_criteres"]["duree_etats"],
        sequencement_perturbation = config["choix_criteres"]["sequencement_perturbation"],
        timing_etatsRares = config["choix_criteres"]["timing_etatsRares"]
    conda:
        "../envs/dissimilarity_choice.yaml"
    script:
        "../scripts/dissimilarity_choice.R"
##### Choice of parameters according to dissimilarity #####
rule params_dissimilarity_choice:  
    input:
        "resources/data/dissimilarity_vector.csv"
    output:
        "resources/data/bounds.rds"
    log:
        "logs/params_dissimilarity_choice.log"
    params:
        expert = config["expert"],
        bounds_OM_seqsubm = config["bounds_OM"]["seqsubm"],
        bounds_OMstran_seqsubm = config["bounds_OMstran"]["seqsubm"],
        bounds_OMstran_transindel = config["bounds_OMstran"]["transindel"],
        bounds_OMstran_otto = config["bounds_OMstran"]["otto"],
        bounds_OMspell_seqsubm = config["bounds_OMspell"]["seqsubm"],
        bounds_OMspell_expcost = config["bounds_OMspell"]["expcost"],
        bounds_SVRspell_a = config["bounds_SVRspell"]["a"],
        bounds_SVRspell_b = config["bounds_SVRspell"]["b"],
        bounds_SVRspell_tpow = config["bounds_SVRspell"]["tpow"]
    conda:
        "../envs/params_dissimilarity_choice.yaml"
    script:
        "../scripts/params_dissimilarity_choice.R"