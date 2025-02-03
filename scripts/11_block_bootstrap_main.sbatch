#!/bin/bash

#SBATCH  -p serc
#SBATCH --nodes=1              
#SBATCH --ntasks-per-node=8 
#SBATCH --mem=48GB 
#SBATCH --time=3:00:00
#SBATCH --mail-type=ALL

ml R/4.0.2

Rscript ./scripts/11B_block_bootstrap_main.R 
