#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#define N 100

//Test program to use OpenMP to determine the max and min values of an array

int main(int argc, char *argv[])
{
	//Declare variables
	int i, j, subarr_size, tid, global_min, global_max, local_min, local_max, arr[N];
	//Populate the array
	for (i = 0; i < N; i++) {
		arr[i] = i;
	}
	//Configure number of threads
	omp_set_num_threads(4);
	//Get the subsize of the array
	subarr_size = N/4;
	//We also need an array to store the found minima and maxima
	int min[4], max[4];
	
	#pragma omp parallel private(tid, j, local_min, local_max)
	{
		tid = omp_get_thread_num();
		local_min = arr[tid*subarr_size];
		local_max = local_min;
		//Find the local minima/maxima
		for (j = tid*subarr_size; j < (tid+1)*subarr_size; j++) {
			if (arr[j] > local_max) {
				local_max = arr[j];
			} else if (arr[j] < local_min) {
				local_min = arr[j];
			}
		}
		min[tid] = local_min;
		max[tid] = local_max;
		//Sync threads for global manima/maxima calc
		#pragma omp barrier
		
		if (tid == 0) {
			global_min = min[0];
			global_max = max[0];
			for (j = 0; j < 4; j++) {
				if (min[j] < global_min)
					global_min = min[j];
				if (max[j] > global_max)
					global_max = max[j];
			}
			printf("min %d \t max: %d \n", global_min, global_max);
		}
	}

}
