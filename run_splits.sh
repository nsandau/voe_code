#!/bin/bash
for ((split=1; split<=$3; split++)); do
sbatch slurm_job.sh $1 $2 $3 $split
sleep 3s
done

