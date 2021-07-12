#!/bin/bash
#SBATCH --job-name=voe      # create a short name for your job
#SBATCH --nodes=1                # node count
#SBATCH --ntasks=1               # total number of tasks across all nodes
#SBATCH --cpus-per-task=6       # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --partition=modi_short  # modi_devel 15min, modi_short 48h, modi_long 7d 

singularity exec docker://nclibz/voecode:latest Rscript ./src/merge_results.R