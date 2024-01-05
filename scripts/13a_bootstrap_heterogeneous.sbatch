#!/bin/bash

#SBATCH  -p normal
#SBATCH --nodes=1              
#SBATCH --cpus-per-task=8
#SBATCH --mem=24GB 
#SBATCH --time=6:00:00
#SBATCH --mail-type=ALL

ml R/4.0.2

Rscript ./scripts/13_bootstrap_heterogeneous.R 
