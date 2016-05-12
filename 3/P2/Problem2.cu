#include <vector>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <math.h>
#include <cstring>
#include <cuda.h>

#define ITER_LIMIT 500
#define MATRIX_MAX 10

using namespace std;


void nmfgpu(float *a, int r, int c, int k, int niters, float *w, float *h);
void matrix_print(float *a, int r, int c);
void nmf_seed(float *out, float *a, int r, int c, int k);
void surface_matrix_multi(float *out, float *a, float *b, int r1, int c1, int r2, int c2);


__global__ void kernel(float *a, int r, int c, int k, int niters, float *w,
 float *h, float* wt, float* ht, float* wta, float* wtw, float* wtwh, float* hdiv, float* aht,
    	float* wh, float* whht, float* wdiv, int total);


__device__ void matrix_multi(float *out, float *a, float *b, int r1, int c1, int r2, int c2, int total);
__device__ void matrix_trans(float *out, float *a, int r, int c, int total);
__device__ float matrix_findmax(float *a, int r, int c, int total);
__device__ float matrix_distance(float *a, float *b, int r, int c, int total);
__device__ void matrix_elemproduct(float* out, float *a, float *b, int r, int c, int total);
__device__ void matrix_elemdivison(float* out, float *a, float *b, int r ,int c, int total);


void surface_matrix_multi(float *out, float *a, float *b, int r1, int c1, int r2, int c2){
    if (c1!=r2){
    	return;
    }
    // int total = omp_get_thread_num();
    // int total = omp_get_num_threads();
    int nth = 0;
    int total = 1;
    int division = r1/total;
    for (int row = (division*total); row < (nth==(total-1) ? r1 : (division*nth+division)); row++) {
        for (int col = 0; col < c2; col++) {
            out[row*c2+col] = 0;
            for (int inner = 0; inner < c1; inner++) {
                out[row*c2+col] += a[row*c1+inner] * b[inner*c2+col]*1.0;
                // printf("%d %d :%.2f += %.2f\n", 
                //     row, col, out[row*c2+col]-a[row*c1+inner] * b[inner*c2+col]*1.0, a[row*c1+inner] * b[inner*c2+col]*1.0);
            }
        }
    }
}




void nmfgpu(float *a, int r, int c, int k, int niters, float *w, float *h){
	// setup cuda
    float *dm;
    float *dm_w;
    float *dm_h;
    int msize = r*c*sizeof(float);
    int msize_w = r*k*sizeof(float);
    int msize_h = k*c*sizeof(float);
    float *dm_wt = new float[r*k];
    float *dm_ht = new float[c*k];
    float *dm_wta = new float[k*c];
    float *dm_wtw = new float[k*k];
    float *dm_wtwh = new float[k*c];
    float *dm_hdiv = new float[k*c];
    float *dm_aht = new float[r*k];
    float *dm_wh = new float[r*c];
    float *dm_whht = new float[r*k];
    float *dm_wdiv = new float[r*k];
    nmf_seed(w, a, r, c, k);
    memcpy(h, w, sizeof(float)*r*k);
    cudaMalloc((void**)&dm, msize);
    cudaMalloc((void**)&dm_h, msize_h);
    cudaMalloc((void**)&dm_w, msize_w);
    cudaMalloc((void**)&dm_wt, r*k*sizeof(float));
    cudaMalloc((void**)&dm_ht, c*k*sizeof(float));
    cudaMalloc((void**)&dm_wta, k*c*sizeof(float));
    cudaMalloc((void**)&dm_wtw, k*k*sizeof(float));
    cudaMalloc((void**)&dm_wtwh, k*c*sizeof(float));
    cudaMalloc((void**)&dm_hdiv, k*c*sizeof(float));
    cudaMalloc((void**)&dm_aht, r*k*sizeof(float));
    cudaMalloc((void**)&dm_wh, r*c*sizeof(float));
    cudaMalloc((void**)&dm_whht,r*k*sizeof(float));
    cudaMalloc((void**)&dm_wdiv,r*k*sizeof(float));

    int total = 4;
    dim3 dimGrid(1,1);
    dim3 dimBlock(total, 1, 1);

    cudaMemcpy(dm, a, msize, cudaMemcpyHostToDevice);
    kernel<<<dimGrid, dimBlock>>>(dm, r, c, k, niters , dm_w, dm_h,
    	dm_wt, dm_ht, dm_wta, dm_wtw, dm_wtwh, dm_hdiv, dm_aht,
    	dm_wh, dm_whht, dm_wdiv, total);

    cudaMemcpy(w, dm_w, msize_w, cudaMemcpyDeviceToHost);
    cudaMemcpy(h, dm_h, msize_h, cudaMemcpyDeviceToHost);
    cudaFree(dm);
	cudaFree(dm_h);
	cudaFree(dm_w);
	cudaFree(dm_wt);
	cudaFree(dm_ht);
	cudaFree(dm_wta);
	cudaFree(dm_wtw);
	cudaFree(dm_wtwh);
	cudaFree(dm_hdiv);
	cudaFree(dm_aht);
	cudaFree(dm_wh);
	cudaFree(dm_whht);
	cudaFree(dm_wdiv);
}




__global__ void kernel(float *a, int r, int c, int k, int niters, float *w, float *h,
	float* wt, float* ht, float* wta, float* wtw, float* wtwh, float* hdiv, float* aht,
    	float* wh, float* whht, float* wdiv, int total){



    // calculate h
    // nominator
    // printf("NMF: 1\n");
    for (int i=0;i<niters;i++){
	    matrix_trans(wt, w, r, k, total);
	    matrix_multi(wta, wt, a, k, r, r, c, total);
	    // denominator
	    // printf("NMF: 2\n");
	    matrix_multi(wtw, wt, w, k, r, r, k, total);
	    matrix_multi(wtwh, wtw, h, k, k, k, c, total);
	    // new h
	    // printf("NMF: 3\n");
	    matrix_elemdivison(hdiv, wta, wtwh, k, c, total);
	    matrix_elemproduct(h, h, hdiv, k, c, total);
	    // calcualte w
	    // nominator
	    // printf("NMF: 4\n");
	    matrix_trans(ht, h, k, c, total);
	    matrix_multi(aht, a, ht, r, c, c, k, total);
	    // calculate w
	    // printf("NMF: 5\n");
	    matrix_multi(wh, w, h, r, k, k, c, total);
	    matrix_multi(whht, wh, ht, r, c, c, k, total);
	    matrix_elemdivison(wdiv, aht, whht, r, k, total);
	    // new w
	    // printf("NMF: 6\n");
	    matrix_elemproduct(w, w, wdiv, r, k, total);
    }
}



__device__ void matrix_multi(float *out, float *a, float *b, int r1, int c1, int r2, int c2, int total){
    if (c1!=r2){
    	return;
    }
    // int total = omp_get_thread_num();
    // int total = omp_get_num_threads();
    int nth = threadIdx.x;
    int division = r1/total;
    for (int row = (division*nth); row < (nth==(total-1) ? r1 : (division*nth+division)); row++) {
        for (int col = 0; col < c2; col++) {
            out[row*c2+col] = 0;
            for (int inner = 0; inner < c1; inner++) {
                out[row*c2+col] += a[row*c1+inner] * b[inner*c2+col]*1.0;
                // printf("%d %d :%.2f += %.2f\n", 
                //     row, col, out[row*c2+col]-a[row*c1+inner] * b[inner*c2+col]*1.0, a[row*c1+inner] * b[inner*c2+col]*1.0);
            }
        }
    }
}

__device__ void matrix_trans(float *out, float *a, int r, int c, int total){
    // This trans function can be boosted up by multi thread
    
 
    // int total = omp_get_thread_num();
    // int total = omp_get_num_threads();
    int nth = threadIdx.x;
    int division = c/total;
    for (int j=(division*nth);j<(nth==(total-1) ? c : (division*nth+division));j++){
        for (int i=0;i<r;i++){
            out[j*r+i]=a[i*c+j];
        }
    }
}

void matrix_print(float *a, int r, int c){
    for(int i=0;i<r;i++){
        for (int j=0;j<c;j++){
            printf("%.2f\t", a[i*c+j]);

        }
        printf("\n");
    }
}

__device__ float matrix_findmax(float *a, int r, int c, int total){
    // this function can be boosted by find max in row/col and compare them
    if (r==0 && c==0){
        return 0;
    }
    float max = a[0];
    for (int i = 0;i<r*c;i++){
        if (a[i]>max){
            max=a[i];
        }
    }
    return max;
}

void nmf_seed(float *out, float *a, int r, int c, int k){
    // setup generator
   	srand(time(NULL));
    for (int i=0;i<r*k;i++){
        out[i] = (float)(MATRIX_MAX*((rand()%100/100.0)));
    }

}


__device__ float matrix_distance(float *a, float *b, int r, int c, int total){
    float distance=0;
    for (int i=0;i<r*c;i++){
        distance += abs(a[i]-b[i])*abs(a[i]-b[i]);
    }
    
    return distance;
}


__device__ void matrix_elemproduct(float* out, float *a, float *b, int r, int c, int total){
    // int total = omp_get_thread_num();
    // int total = omp_get_num_threads();
    int nth = threadIdx.x;
    int division = r*c/total;
    // printf("Its %d/%d\n", total, total);
    for (int i=(division*nth);i<(nth==(total-1) ? r*c : (division*nth+division));i++){
            out[i]=1.0*a[i]*b[i];
    }
}


__device__ void matrix_elemdivison(float* out, float *a, float *b, int r ,int c, int total){
    // int total = omp_get_thread_num();
    // int total = omp_get_num_threads();
    int nth = threadIdx.x;
    int division = r*c/total;
    for (int i=(division*nth);i<(nth==(total-1) ? r*c : (division*nth+division));i++){
            if (b[i]==0){
            	return;
            }
            out[i]=1.0*a[i]/b[i];
            // printf("Now is %d\n", i);
    }
}


int main(int argc, char const *argv[])
{
    // omp_set_num_threads(8);
    /* code */
    // float a[12]={2, 3, 4, 1, 2, 10, 11, 32,
    //             3.1, 4.1, 31, 0.2};
    // float b[12]={1, 2, 1, 3, 1, 4, 1, 5,
    //             1, 6, 1, 7};


    // matrix for nmf. rxc (row, col)
    // int r = 300;
    // int c = 200;
    // int k = 150; 
    int r = 5;
    int c = 4;
    int k = 3;
    float B[20]={1, 2, 3, 4,
            5, 6, 7, 8,
            9, 10, 11, 12,
            13, 14, 15, 16,
            17, 18, 19, 20};


    

    // matrix generator
	srand(time(NULL));
    float A[r*c];
    for (int i=0;i<r;i++){
        for (int j=0;j<c;j++){
            A[i*c+j]=(float)(1.0*MATRIX_MAX*((float)(rand()%100/100.0)));
        }
    }
 



    // nmf using originall 
    float w[r*k];
    float h[k*c];


    // double time_begin=get_wall_time();
    for (int i=0;i<1;i++){
        nmfgpu(A, r, c, k, 500, w, h);
        printf("It's run: \t%d\n", i);
    }
    // double time_end=get_wall_time();
    // printf("Matrix w is: \n");
    // matrix_print(w, 5, 3);
    // printf("Matrix h is: \n");
    // matrix_print(h, 3, 4);

    printf("Matrix A is: \n");
    matrix_print(A, r, c);
    surface_matrix_multi(A, w, h, r, k, k, c);
    printf("Matrix w*h is: \n");
    matrix_print(A, r, c);


    return 0;
}