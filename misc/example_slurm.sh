#!/bin/sh
# SLURM directives
#SBATCH --job-name=halo
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=4
#SBATCH --mem=120G

#!/bin/bash

	cd /home/mcavanagh/shpc
	
	module load openmpi
	
	rm time.log out.log
	date >> time.log
	time mpirun ./halo_mpionly > out.log
	date >> time.log
