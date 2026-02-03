#!/bin/bash

#SBATCH --account=def-tgleeson
#SBATCH --mail-type=ALL
#SBATCH --mail-user=xanderhuggins@uvic.ca
#SBATCH --time=0-02:00:00
#SBATCH --mem=10G
#SBATCH --array=961-1020  
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=1
#SBATCH --nodes=1
#SBATCH --job-name=som_iter
#SBATCH --output=slurm_files/som_iter_%a.out 
#SBATCH --error=slurm_files/som_iter_%a.err 

module load r/4.3.1

Rscript a2-1-1-som1-iterations-syntheticspace.R $SLURM_ARRAY_TASK_ID
