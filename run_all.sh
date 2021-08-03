#!/bin/bash

## FUNC NO PROTOCOL
sbatch slurm_job.sh func none 1
sleep 3s
sbatch slurm_job.sh func none 2
sleep 3s
sbatch slurm_job.sh func none 3
sleep 3s
sbatch slurm_job.sh func none 4
sleep 3s
sbatch slurm_job.sh func none 5
sleep 3s
sbatch slurm_job.sh func none 6
sleep 3s

## FUNC WITH PROTOCOLS 
sbatch slurm_job.sh func beks 1
sleep 3s
sbatch slurm_job.sh func handoll 1
sleep 3s
sbatch slurm_job.sh func skou 1
sleep 3s

## BIN NO PROTOCOL
sbatch slurm_job.sh bin none 1
sleep 3s
sbatch slurm_job.sh bin none 2
sleep 3s
sbatch slurm_job.sh bin none 3
sleep 3s
sbatch slurm_job.sh bin none 4
sleep 3s
sbatch slurm_job.sh bin none 5
sleep 3s
sbatch slurm_job.sh bin none 6

## BIN WITH PROTOCOLS
sbatch slurm_job.sh bin beks 1
sleep 3s
sbatch slurm_job.sh bin handoll 1
sleep 3s
sbatch slurm_job.sh bin skou 1
sleep 3s

## ALL QOLS
sbatch slurm_job.sh qol none 1
sleep 3s
sbatch slurm_job.sh qol beks 1
sleep 3s
sbatch slurm_job.sh qol handoll 1
sleep 3s
sbatch slurm_job.sh qol skou 1
sleep 3s


