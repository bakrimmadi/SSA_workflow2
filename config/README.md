# Configuration Guide for SSA Workflow

This document explains the structure and usage of the `config.yaml` file used in the **State Sequence Analysis (SSA)** workflow. This file contains all necessary parameters for data input, analysis configuration, and results output.

---

## 🔁 Input Data

- **`samples`**  
  Path to the CSV file containing the dataset.  
  _Example:_ `"resources/data/Tab_suivi_mvad.csv"`

- **`bdd_format`**  
  Format of the input data: either `"Long"` or `"Wide"`.  
  _Default:_ `"Wide"`

- **`var_id`**  
  Name of the column identifying individual sequences.  
  _Example:_ `"id"`

- **`var_rep`**  
  Comma-separated list of response variables (sequence states over time).  
  _Example:_ `"jul_93, aug_93, ..., jun_99"`

- **`labels`**  
  List of state labels in alphabetical order.  
  _Example:_ `"Employement, Further Educ, High Educ, Joblessness, School, Training"`

---

## 📁 Output

- **`path_result`**  
  Path where the workflow will save plots and results.  
  _Default:_ `"results/plots"`

---

## 🎯 Criteria Selection

Defined in the nested section `choix_criteres`, used for dissimilarity selection:

- **`sequencement`**: Consider the sequencing order (`"TRUE"` / `"FALSE"`)
- **`timing`**: Consider timing of transitions
- **`duree_etats`**: Consider the duration of states
- **`sequencement_perturbation`**: Consider sensitivity to small perturbations
- **`timing_etatsRares`**: Focus on rare states

---

## 🧠 Expert Mode Settings

- **`expert`**  
  Whether the expert profile is enabled:  
  - `1`: Yes  
  - `0`: No  

- **`dissimilarite_expert`**  
  Optional: List of expert-specified dissimilarities (e.g., `"HAM, CHI2, EUCLID, OM, OMspell, LCS, OMstran, SVRspell"`).  
  _Used only if `expert: 1`._

- **`fusion_dissimilarites`**  
  Whether to merge expert and automatically selected dissimilarities:  
  - `1`: Yes  
  - `0`: No
  _Used only if `expert: 1`._

---

## ⚙️ Dissimilarity Parameters (Hyperparameter Ranges)

Dissimilarities used for clustering require parameter bounds. These are specified for each dissimilarity type:
_Used only if `expert: 1`._

seqsubm	: How to generate the costs.The substitution-cost matrix when a matrix and method is one of "OM", "OMspell", "OMstran", "HAM".
One of 0L = "CONSTANT" (same cost for all substitutions), 1L = "TRATE" (derived from the observed transition rates), 2L = "FUTURE" (Chi-squared distance between conditional state distributions lag positions ahead), 3L = "FEATURES" (Gower distance between state features), 4L = "INDELSLOG" (based on estimated indel costs).

transindel	: Method for computing transition indel costs when method = "OMstran". 
One of 0L = "constant" (single indel of 1.0), 1L = "subcost" (based on substitution costs), or 2L = "prob" (based on transition probabilities).

otto  : The origin-transition trade-off weight when method = "OMstran". It must be in [0, 1].

expcost	: The cost of spell length transformation when method = "OMspell". It must be positive. The exact interpretation is distance-dependent.

tpow	: The exponential weight of spell length when method is one of "OMspell" or "SVRspell".

### 🔹 `OM` (Optimal Matching)

```yaml
bounds_OM:
  seqsubm: "c(0L,4L)"
```

### 🔹 `OMstran` (Optimal Matching with transitions)

```yaml
bounds_OMstran:
  seqsubm: "c(0L,4L)"
  transindel: "c(0L,2L)"
  otto: "c(0.0001, 1)"
```

### 🔹 `OMspell` (OM with spell lengths)

```yaml
bounds_OMspell:
  seqsubm: "c(0L,4L)"
  expcost: "c(0,3)"
```

### 🔹 `SVRspell` (Spell-based SVR dissimilarity)

```yaml
bounds_SVRspell:
  a: "c(0,3)"
  b: "c(0,3)"
  tpow: "c(0,3)"
```

---

## 📝 Notes

- All parameter bounds must be expressed in `R` vector notation (`c(x, y)`).
- If `expert: 0`, the fields under `"dissimilarite_expert"` are ignored.
- Ensure your input file matches the specified `bdd_format`.

---

## ✅ Example Minimal `config.yaml`

```yaml
samples: "resources/data/Tab_suivi_mvad.csv"
bdd_format: "Wide"
var_id: "id"
var_rep: "jul_93, aug_93, sep_93"
labels: "Employment, Joblessness, School"
path_result: "results/plots"
min_cluster_size: NULL
expert: 0
choix_criteres:
  sequencement: "TRUE"
  timing: "FALSE"
  duree_etats: "TRUE"
  sequencement_perturbation: "FALSE"
  timing_etatsRares: "FALSE"
bounds_OM:
  seqsubm: "c(0L,4L)"
bounds_OMstran:
  seqsubm: "c(0L,4L)" #Valeurs entières entre 0 et 4 
  transindel: "c(0L,2L)" #Valeurs entières entre 0 et 2
  otto: "c(0.0001, 1)"
bounds_OMspell:
  seqsubm: "c(0L,4L)" #Valeurs entières entre 0 et 4
  expcost: "c(0,3)"
bounds_SVRspell:
  a: "c(0,3)"
  b: "c(0,3)"
  tpow: "c(0, 3)"
```

---

For more information, please refer to the [SSA workflow repository](https://github.com/bakrimmadi/SSA_workflow2).

