#!/bin/bash

# FUNC 
## NO PROTOCOL
sh run_splits.sh func none

## HANDOLL
sh run_splits.sh func handoll


## WITH PROTOCOLS 
for protocol in beks skou
do
sbatch slurm_job.sh func $protocol 1
sleep 3s
done

# BIN 
## NO PROTOCOL
sh run_splits.sh bin none

## Handoll split
sh run_splits.sh bin handoll

## WITH PROTOCOLS
for protocol in beks skou
do
sbatch slurm_job.sh bin $protocol 1
sleep 3s
done

# QOL 
## ALL # beks doesnt include qol
for protocol in none handoll skou
do
sbatch slurm_job.sh qol $protocol 1
sleep 3s
done


