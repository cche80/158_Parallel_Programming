// To get started with OpenMP. You will need to include the omp header file with
// #include <omp.h>
// and you will need to add the -fopenmp option when compiling:
// g++ hw2p2.3_LaoSun_Finalized_Commented.cpp -std=c++11 -fopenmp

#include <vector>
#include <iostream>
#include <fstream>
#include <math.h>
#include <chrono>	// Deals with time, used for randomization and timing
#include <random>
#include <cstring>
#include <ctime>	// Manipulates data and time information
#include <omp.h>	// OpenMP
#include <sys/time.h>
#define MATRIX_MAX 1
#define MAX_SIZE 1400

using namespace std;

void seed(int r, int c, float *out) {   //generate a random matrix
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::knuth_b generator (seed);
    float rand_max = generator.max();
    float rand_min = generator.min();
    for (int i=0;i<r;i++)
        for (int j=0;j<c;j++)
            out[i*c+j]=(float)(1.0*MATRIX_MAX*(1.0*generator()-rand_min)/(rand_max-rand_min));
}


double get_wall_time(){
    struct timeval time;
    if (gettimeofday(&time,NULL)) return 0;
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

// n = matrix dimension; k = subimage dimension; start and end are for parallelism; pix is the pointer to the matrix.
int brightssplit (float *pix, int n, int k, float thresh, int start, int end) {
	float count = 0, check[n];
	float minus, sum;
	for (int i = 0; i < n; ++i) {	// iterate throught ALL n rows
		// Clear book keeping variables for a new round
    	   	minus = 0;
		sum = 0;
	
		//
		// Count the number of bright pixel in a single row of 1 x k
		// i * n locate row, + j locate column
		//
		for (int j = start; j < start+k; ++j) if (pix[i*n+j] > thresh) {sum++;}

		//
		// end - k + 1 basically recovers the boundary by the 1st column index of each window
		// which could be understood as the window column index
		// instead of the last column index which is end itself
		//
		// Now we run from the starting window column index to the end WCI
		// Visually, we are moving a 1 x k block across row i to track bright block
		// 
		for (int j = start; j < (end-k+1); ++j) {
			//
			// minus:
			// This is a smart way of moving the 1 x k block to the right.
			// Instead of using excessive for loops and repetitive checking,
			// we simply look at the left most pixel of that block,
			// and later we can see what we could do with sum.
			//  
			if (pix[i*n+j] > thresh) {minus = 1;} else {minus = 0;}
			
			//
			// Check List for keeping track of the number of 1 x k bright unit
			// in terms of window column index
			// Initialized when going through the first row
			//	
			if (i == 0) {if (sum >= k) {check[j] = 1;} else {check[j] = 0;}}
			
			//
			// Now we are out of the first row, that means we get to +/- to the list,
			// and hopefully hit a bring window when count >= k!
			// Keep Count++ when check[j] >= k
			// Clear it when we hit a 1 x k block with non-bright pixels.
			//
			else {
				if (sum >= k) {
					check[j] ++;
					if (check[j] >= k) {count ++;}
				}
				else {check[j] = 0;}
			}
			
			//
			// Now we are about to move the 1 x k block to the right. Rmb the minus?
			// We can now use it to change sum, together with the information of
			// the one new pixel the 1 x k block now includes.
			// Again, elegant way of avoiding repetitive checking! Good job LaoSun.
			//
			if(minus == 0 && pix[i*n+j+k] > thresh) {sum++;}
			else if(minus == 1 && pix[i*n+j+k] <= thresh) {sum--;}
		}
	}

	return count;
}

int brights (float *pix, int n, int k, float thresh) {
	if (n > 200) {
		int count = 0;
		#pragma omp paralle
		{
	       	int num = omp_get_thread_num();	// the thread number of this thread
	       	int t = omp_get_num_threads();	// the total number of threads
	       	int d = n/t;			// split work amoung t threads
		
		//
		// Common Cases: from d*num to d*num+d+(k-1)
		// Boundary must + (k - 1) to include the window at the end of the job list
		// So that we don't leave out any windows for consideration!
		//	
   	    	if(num < t) { count += brightssplit (pix, n, k, thresh, d*num, d*num+d+k-1); }
		// End Case: does the rest of the job from d*num to n
		else { count += brightssplit (pix, n, k, thresh, d*num, n); }
		}
		return count;
	}

	// Don't parallelize if n < 200
	else { return brightssplit (pix, n, k, thresh, 0, n); }

}


int main (int argc, char const *argv[])
{
	omp_set_num_threads(8);

	float A[MAX_SIZE*MAX_SIZE];	// use one dimensional array to represent the matrix
	seed(MAX_SIZE,MAX_SIZE,A);	// generate random matrix

	double time_begin=get_wall_time();
    float c = brights (A, 1000, 3, 0.2);	// RUN!
    double time_end=get_wall_time();
    printf("Total elapsed time is: %.4lf\n", (time_end - time_begin));
	

	cout<<"here "<<c<<endl;
	return 0;
}
