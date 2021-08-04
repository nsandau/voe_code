#!/bin/bash
for split in 1 2 3 4 5 6
do
echo sbatch slurm_job.sh $1 $2 $split
sleep 3s
done

