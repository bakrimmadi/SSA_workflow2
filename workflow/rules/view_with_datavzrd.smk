#### generate seq.yaml for datadzrd ####
rule generate_seq_yaml:
    input:
        "resources/data/seq.csv"
    output:
        "resources/datavzrd/seq.yaml"
    log:
        "logs/generate_seq_yaml.log"
    conda:
        "../envs/generate_seq_yaml_py.yaml"
    script:
        "../scripts/generate_seq_yaml.py"

#### Table view with datadzrd ####
rule view_with_datavzrd:
    input:
        config=workflow.source_path("../resources/datavzrd/seq.yaml"),
        table="resources/data/seq.csv",
    output:
        report(
            directory("results/tables/seq"),
            htmlindex="index.html",
            caption="../report/seq.rst",
            category="Tables",
            labels={"table": "seq"},
        ),
    log:
        "logs/datavzrd.log",
    wrapper:
        "v4.7.2/utils/datavzrd"