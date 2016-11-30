#!/bin/bash -l
#SBATCH -D /home/slingsby
#SBATCH -J renewables
#SBATCH -o /home/slingsby/out-%j.txt
#SBATCH -e /home/slingsby/error-%j.txt
#SBATCH --mail-type=All
#SBATCH --mail-user=jasper@saeon.ac.za
#SBATCH --partition=low
#SBATCH --ntasks=60
#SBATCH --time=5-0

module load R
R CMD BATCH Renewables_cluster.R