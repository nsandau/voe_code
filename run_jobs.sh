#!/bin/bash
sbatch slurm_job.sh $1 1
sleep 10s
sbatch slurm_job.sh $1 2
sleep 10s
sbatch slurm_job.sh $1 3
sleep 10s
sbatch slurm_job.sh $1 4
sleep 10s
sbatch slurm_job.sh $1 5
sleep 10s
sbatch slurm_job.sh $1 6
