# SSA_workflow – Snakemake Workflow for State Sequence Analysis

A scientific workflow for **State Sequence Analysis (SSA)**.  
This Snakemake workflow automates:
- State sequence data analysis
- Dissimilarity measure selection
- Clustering
- Interactive visualization using **datavzrd**

---

## 👤 Author

**Bakridine MMADI MRENDA**

---

## 🧰 Prerequisites

- [Miniconda](https://docs.conda.io/en/latest/miniconda.html) or Anaconda
- Git (optional but recommended)

---

## 🚀 Deployment

### Step 1: Install Snakemake and Snakedeploy

We recommend using [Mamba](https://github.com/mamba-org/mamba) (a faster alternative to Conda).  
To install both **Snakemake** and **Snakedeploy** in an isolated environment:

```bash
mamba create -c conda-forge -c bioconda --name snakemake snakemake snakedeploy
conda activate snakemake
```

If you don’t have Mamba, you can use Conda instead (conda install).

to install both Snakemake and Snakedeploy in an isolated environment. For all following commands ensure that this environment is activated via

```bash
conda activate snakemake
```

## Step 2: Deploy workflow
With Snakemake and Snakedeploy installed, the workflow can be deployed as follows.
First, create an appropriate project working directory on your system and enter it:

```bash
mkdir -p path/to/project-workdir
cd path/to/project-workdir
```

In all following steps, we will assume that you are inside of that directory. Then run
```bash
snakedeploy deploy-workflow https://github.com/bakrimmadi/SSA_workflow2 . --tag v2.1.2
```
Snakedeploy will create two folders, workflow and config.
The former contains the deployment of the chosen workflow as a Snakemake module, the latter contains configuration files which will be modified in the next step in order to configure the workflow to your needs.

## Step 3: Configure workflow
To configure the workflow, adapt config/config.yml to your needs.

## Step 4: Run workflow
The deployment method is controlled using the --software-deployment-method (short --sdm) argument.
To run the workflow with automatic deployment of all required software via conda/mamba, use

```bash
snakemake --cores all --sdm conda
```
Snakemake will automatically detect the main Snakefile in the workflow subfolder and execute the workflow module that has been defined by the deployment in step 2.

## Step 5: Generate report
After finalizing your data analysis, you can automatically generate an interactive visual HTML report for inspection of results together with parameters and code inside of the browser using

```bash
snakemake --sdm conda --report report.zip
unzip report.zip
```

# Configuration

## ⚙️ Configuration Tips

The file `config/config.yaml` controls:
- Input file paths
- Output directories
- Plotting options
- Parameters for dissimilarity, clustering, etc.

You can also check `config/README.md` for more guidance.
