# Computational Statistics: Bootstrap Estimation Methods

This repository contains the implementation of advanced computational statistics techniques, focusing on various Bootstrap resampling methods to estimate parameters and construct rigorous confidence intervals. The codebase is architected with a strong emphasis on **modularization**, **encapsulation**, and **reproducibility**, adhering to modern Data Science and MLOps standards.

## 📂 Project Architecture

The project strictly separates core logical functions from execution scripts, avoiding global namespace pollution by utilizing the `box` package.

* `data/`: Contains raw empirical datasets (e.g., `salmon.dat`, `cancersurvival.dat`). 
* `R/`: Core modular functions (The "Calculators"). These files contain strictly logic and no execution code.
    * `beverton_holt_model.R`: OOP-based constructors and estimators for the Beverton-Holt model.
    * `ci_functions.R`: Mathematical implementations for advanced Confidence Intervals (BCa, Studentized).
* `scripts/`: Task-specific logic and execution scripts.
    * `task_1_salmon.R`: Bootstrap resampling strategies specifically tailored for Task 1.
    * `exe_task1.R`: The main entry point to execute Task 1.
* `report/`: Stores generated outputs.
    * `figures_plots/`: High-resolution visualizations (e.g., comparative histograms).
* `renv/`: Isolated project environment managed by the `renv` package.

## 🛠️ Prerequisites & Environment Setup

This project uses `renv` to guarantee **reproducibility** across different machines. To set up the exact environment, execute the following commands in your R Console upon cloning the repository:

```r
# 1. Install renv if you haven't already
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# 2. Restore the isolated project library from the renv.lock file
renv::restore()