#!/bin/bash

# FUNC 
## NO PROTOCOL
for number in 1 2 3 4 5 6
do
sbatch slurm_job.sh func none $number
sleep 3s
done

## HANDOLL SPLIT
for number in 1 2 3 4 5 6
do
sbatch slurm_job.sh func handoll $number
sleep 3s
done

## WITH PROTOCOLS 
for protocol in beks skou
do
sbatch slurm_job.sh func $protocol 1
sleep 3s
done

# BIN 
## NO PROTOCOL
for number in 1 2 3 4 5 6
do
sbatch slurm_job.sh bin none $number
sleep 3s
done

## Handoll split
for number in 1 2 3 4 5 6
do
sbatch slurm_job.sh bin handoll $number
sleep 3s
done

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


