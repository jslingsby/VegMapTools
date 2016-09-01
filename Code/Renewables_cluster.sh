#!/bin/bash -l
#SBATCH -D /home/slingsby
#SBATCH -J renewables
#SBATCH -o /home/slingsby/out-%j.txt
#SBATCH -e /home/slingsby/error-%j.txt
#SBATCH --mail-type=All
#SBATCH --mail-user=jasper@saeon.ac.za
#SBATCH -p med
#SBATCH --ntasks=60

module load R
R CMD BATCH Renewables_cluster.R