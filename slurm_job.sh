#!/bin/bash
#SBATCH --job-name=voe      # create a short name for your job
#SBATCH --nodes=1
#SBATCH --ntasks=1               # total number of tasks across all nodes
#SBATCH --cpus-per-task=64       # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --exclusive
#SBATCH --mail-type=FAIL,END          # send email on start, end and fault
#SBATCH --mail-user=nclibz@gmail.com
#SBATCH --partition=modi_short  # modi_devel 15min, modi_short 48h, modi_long 7d 

# args
# 1: outcome, 2: protocol, 3: n_splits, 4: split_no, 5: dev_run
singularity exec library://nclibz/default/image:latest Rscript ./src/metagen.R $1 $2 $3 $4 $5