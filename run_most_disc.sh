#!/bin/bash
sbatch slurm_job.sh func handoll 1 1 --most_disc
sbatch slurm_job.sh func none 2 1 --most_disc
sbatch slurm_job.sh func none 2 2 --most_disc
sbatch slurm_job.sh bin none 1 1 --most_disc
sbatch slurm_job.sh bin beks 1 1 --most_disc
sbatch slurm_job.sh bin skou 1 1 --most_disc