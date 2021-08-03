#!/bin/bash
sbatch slurm_job.sh $1 $2 1
sleep 5s
sbatch slurm_job.sh $1 $2 2
sleep 5s
sbatch slurm_job.sh $1 $2 3
sleep 5s
sbatch slurm_job.sh $1 $2 4
sleep 5s
sbatch slurm_job.sh $1 $2 5
sleep 5s
sbatch slurm_job.sh $1 $2 6

