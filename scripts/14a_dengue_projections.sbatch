#!/bin/bash

#SBATCH  -p bigmem
#SBATCH --nodes=1              
#SBATCH --ntasks-per-node=1     
#SBATCH --mem=360GB 
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL

ml R/4.0.2

Rscript ./scripts/14_dengue_projections.R 
