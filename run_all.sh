#!/bin/bash

# BIN 
## NO PROTOCOL
bash run_splits.sh bin none 10 

## Handoll split
bash run_splits.sh bin handoll 6

## WITH PROTOCOLS
for protocol in beks skou
do
sbatch slurm_job.sh bin $protocol 1 1
sleep 3s
done


# FUNC 
## NO PROTOCOL
bash run_splits.sh func none 8

## HANDOLL
bash run_splits.sh func handoll 6


## WITH PROTOCOLS 
for protocol in beks skou
do
sbatch slurm_job.sh func $protocol 1 1
sleep 3s
done

# QOL 
## ALL # beks doesnt include qol
for protocol in none handoll skou
do
sbatch slurm_job.sh qol $protocol 1 1
sleep 3s
done


