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

## Windows Users – Use WSL (Windows Subsystem for Linux)

Some packages and workflows may not run correctly on native Windows. We recommend using WSL:
### Steps to Enable WSL
1. Go to Microsoft’s official page to install WSL:
[WSL](https://learn.microsoft.com/fr-fr/windows/wsl/install)
This will walk you through:
- Enabling required features
- Choosing a Linux distribution (e.g., Debian)
- Installing WSL 2 (the recommended version)

3. Restart your computer.
4. Open Ubuntu (or your chosen distribution) from the Start Menu.
5. Update packages:
   ```bash
   sudo apt update && sudo apt upgrade
   ```
6. Install Miniconda and Git as shown above for Linux.

### 1. [Miniconda](https://docs.conda.io/en/latest/miniconda.html)

#### macOS
- Download the installer from: https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-x86_64.sh
- Open Terminal and run:
  ```bash
  bash Miniconda3-latest-MacOSX-x86_64.sh
  ```
- Follow the instructions to complete installation.

#### Linux
- Download the installer from: https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
- Open a terminal and run:
  ```bash
  bash Miniconda3-latest-Linux-x86_64.sh
  ```

### 2. [Git](https://git-scm.com/)

#### macOS (via Homebrew)
```bash
brew install git
```

#### Linux (Debian/Ubuntu)
```bash
sudo apt update
sudo apt install git
```
---

## 🚀 Deployment

### Step 1: Install Snakemake 

We recommend using [Mamba](https://github.com/mamba-org/mamba) (a faster alternative to Conda).  
To install both **Snakemake**  in an isolated environment:

## Clone the Repository

```bash
git clone https://github.com/bakrimmadi/SSA_workflow2.git
cd SSA_workflow2/workflow
```
## Create the Conda Environment
```bash
conda create -c conda-forge -c bioconda --name snakemake snakemake
conda activate snakemake
```
to install Snakemake in an isolated environment.

## Step 2: Configure workflow
To configure the workflow, adapt config/config.yml to your needs.

## Step 3: Run workflow
To run the workflow with conda environments, use

```bash
snakemake --use-conda --cores all
```
Snakemake will automatically detect the main Snakefile in the workflow subfolder and execute the workflow.

## Step 4: Generate report
After finalizing your data analysis, you can automatically generate an interactive visual HTML report for inspection of results together with parameters and code inside of the browser using

```bash
snakemake --report report.html
sudo apt update && sudo apt install unzip
unzip report.zip
```
Open report.html with Microsoft Edge.

# Configuration

## ⚙️ Configuration Tips

The file `config/config.yaml` controls:
- Input file paths
- Output directories
- Plotting options
- Parameters for dissimilarity, clustering, etc.

You can also check `config/README.md` for more guidance.
