#!/bin/bash

#SBATCH  -p serc
#SBATCH --nodes=1              
#SBATCH --cpus-per-task=12
#SBATCH --mem=72GB 
#SBATCH --time=6:00:00
#SBATCH --mail-type=ALL

ml R/4.0.2

Rscript ./scripts/13_bootstrap_heterogeneous.R "pop_per_km2_tercile"
