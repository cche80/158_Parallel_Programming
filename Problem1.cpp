#include <vector>
#include <iostream>
#include <fstream>
#include <math.h>
#include <chrono>
#include <random>
#include <cstring>
#include <ctime>
#include <omp.h>
#include <sys/time.h>

#define ITER_LIMIT 500
#define MATRIX_MAX 10

using namespace std;


bool init_flag=false;

// init params
float *wt;
float *ht;
// for H
float *wta;
float *wtw;
float *wtwh;
float *hdiv;
// for W
float *aht;
float *wh;
float *whht;
float *wdiv;



double time_s1=0;
double time_s2=0;
double time_s3=0;
double time_s4=0;
double time_s5=0;


void nmfomp(float *a, int r, int c, int k, int niters, float *w, float *h);
void matrix_print(float *a, int r, int c);

void matrix_multi(float *out, float *a, float *b, int r1, int c1, int r2, int c2);
void matrix_trans(float *out, float *a, int r, int c);
float matrix_findmax(float *a, int r, int c);
void nmf_seed(float *out, float *a, int r, int c, int k);
float matrix_distance(float *a, float *b, int r, int c);
void matrix_elemproduct(float* out, float *a, float *b, int r, int c);
void matrix_elemdivison(float* out, float *a, float *b, int r ,int c);
double get_wall_time();

double get_wall_time(){
    struct timeval time;
    if (gettimeofday(&time,NULL)){
        //  Handle error
        return 0;
    }
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}


void nmfomp(float *a, int r, int c, int k, int niters, float *w, float *h){
    // printf("Now its %d interators\n", niters);
    // initializing w and h when niters == 0
    if (!init_flag){
        // init
        // init params
        wt = new float[r*k];
        ht = new float[c*k];
        // for H
        wta = new float[k*c];
        wtw = new float[k*k];
        wtwh = new float[k*c];
        hdiv = new float[k*c];
        // for W
        aht = new float[r*k];
        wh = new float[r*c];
        whht = new float[r*k];
        wdiv = new float[r*k];
        // printf("Dimension is: r %d c %d k %d\n", r, c, k );
        nmf_seed(w, a, r, c, k);
        // nmf_seed(h, a, r, c, k);
        // matrix_print(w, r, k);
        memcpy(h, w, sizeof(float)*r*k);
        init_flag=true;

    }

    // matrix_print(w, r, k);



    // Check if calculate is ended
    // now just use 100 iters
    if (niters<=0){
        delete[] wt;
        delete[] ht;
        delete[] wta;
        delete[] wtw;
        delete[] wtwh;
        delete[] hdiv;
        delete[] aht;
        delete[] wh;
        delete[] whht;
        delete[] wdiv;
        return;
    }
    // calculate h
    // nominator
    // printf("NMF: 1\n");
    double time_begin=get_wall_time();
    matrix_trans(wt, w, r, k);
    matrix_multi(wta, wt, a, k, r, r, c);
    double time_end=get_wall_time();
    time_s1+=(time_end-time_begin);
    // denominator
    // printf("NMF: 2\n");
    time_begin=get_wall_time();
    matrix_multi(wtw, wt, w, k, r, r, k);
    matrix_multi(wtwh, wtw, h, k, k, k, c);
    time_end=get_wall_time();
    time_s2+=(time_end-time_begin);
    // new h
    // printf("NMF: 3\n");
    time_begin=get_wall_time();
    matrix_elemdivison(hdiv, wta, wtwh, k, c);
    matrix_elemproduct(h, h, hdiv, k, c);
    time_end=get_wall_time();
    time_s3+=(time_end-time_begin);

    // calcualte w
    // nominator
    // printf("NMF: 4\n");
    time_begin=get_wall_time();
    matrix_trans(ht, h, k, c);
    matrix_multi(aht, a, ht, r, c, c, k);
    time_end=get_wall_time();
    time_s4+=(time_end-time_begin);
    // calculate w
    // printf("NMF: 5\n");
    time_begin=get_wall_time();
    matrix_multi(wh, w, h, r, k, k, c);
    matrix_multi(whht, wh, ht, r, c, c, k);
    matrix_elemdivison(wdiv, aht, whht, r, k);
    time_end=get_wall_time();
    time_s5+=(time_end-time_begin);
    // new w
    // printf("NMF: 6\n");
    matrix_elemproduct(w, w, wdiv, r, k);

    nmfomp(a,r,c,k,niters-1,w,h);





}

void matrix_multi(float *out, float *a, float *b, int r1, int c1, int r2, int c2){
    if (c1!=r2){
        printf("Matrix_multi ERR: dimension does not match! Input: c1 %d r2 %d\n", c1, r2);
    }
    #pragma omp parallel
    {
        int nth = omp_get_thread_num();
        int total = omp_get_num_threads();
        int division = r1/total;
        int row=0;
        int col=0;
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
}

void matrix_trans(float *out, float *a, int r, int c){
    // This trans function can be boosted up by multi thread
    
    #pragma omp parallel
    {   
        int nth = omp_get_thread_num();
        int total = omp_get_num_threads();
        int division = c/total;
        for (int j=(division*nth);j<(nth==(total-1) ? c : (division*nth+division));j++){
            for (int i=0;i<r;i++){
                out[j*r+i]=a[i*c+j];
            }
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

float matrix_findmax(float *a, int r, int c){
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
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    // find max
    float max = matrix_findmax(a, r, c);
    // printf("The max in Matrix is : %.2f\n", max);
    max = sqrt(1.0*max/k);
    // printf("The max is : %.2f\n", max);
    // generate uniform distribution array
    std::knuth_b generator (seed);
    float rand_max = generator.max();
    float rand_min = generator.min();
    for (int i=0;i<r*k;i++){
        out[i] = (float)(1.0*max*(1.0*generator()-rand_min)/(rand_max-rand_min));
    }

}


float matrix_distance(float *a, float *b, int r, int c){
    float distance=0;
    for (int i=0;i<r*c;i++){
        distance += abs(a[i]-b[i])*abs(a[i]-b[i]);
    }
    
    return distance;
}


void matrix_elemproduct(float* out, float *a, float *b, int r, int c){
    #pragma omp parallel
    {
        int nth = omp_get_thread_num();
        int total = omp_get_num_threads();
        int division = r*c/total;
        // printf("Its %d/%d\n", nth, total);
        for (int i=(division*nth);i<(nth==(total-1) ? r*c : (division*nth+division));i++){
                out[i]=1.0*a[i]*b[i];
        }
    }
}


void matrix_elemdivison(float* out, float *a, float *b, int r ,int c){
    #pragma omp parallel
    {
        int nth = omp_get_thread_num();
        int total = omp_get_num_threads();
        int division = r*c/total;
        for (int i=(division*nth);i<(nth==(total-1) ? r*c : (division*nth+division));i++){
                if (b[i]==0){
                    printf("Divided by 0!\n");
                }
                out[i]=1.0*a[i]/b[i];
                // printf("Now is %d\n", i);
        }
    }
}


// int main(int argc, char const *argv[])
// {
//     omp_set_num_threads(8);
//     /* code */
//     float a[12]={2, 3, 4, 1, 2, 10, 11, 32,
//                 3.1, 4.1, 31, 0.2};
//     float b[12]={1, 2, 1, 3, 1, 4, 1, 5,
//                 1, 6, 1, 7};


//     // matrix for nmf. rxc (row, col)
//     // int r = 300;
//     // int c = 200;
//     // int k = 150; 
//     int r = 5;
//     int c = 4;
//     int k = 3;
//     float B[20]={1, 2, 3, 4,
//             5, 6, 7, 8,
//             9, 10, 11, 12,
//             13, 14, 15, 16,
//             17, 18, 19, 20};

//     // matrix generator
//     unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
//     std::knuth_b generator (seed);
//     float rand_max = generator.max();
//     float rand_min = generator.min();
//     float A[r*c];
//     for (int i=0;i<r;i++){
//         for (int j=0;j<c;j++){
//             A[i*c+j]=(float)(1.0*MATRIX_MAX*(1.0*generator()-rand_min)/(rand_max-rand_min));
//         }
//     }
 



//     // nmf using originall 
//     float w[r*k];
//     float h[k*c];


//     double time_begin=get_wall_time();
//     for (int i=0;i<1;i++){
//         nmfomp(B, r, c, k, 500, w, h);
//         printf("It's run: \t%d\n", i);
//     }
//     double time_end=get_wall_time();
//     // printf("Matrix w is: \n");
//     // matrix_print(w, 5, 3);
//     // printf("Matrix h is: \n");
//     // matrix_print(h, 3, 4);

//     printf("Matrix A is: \n");
//     matrix_print(A, r, c);
//     matrix_multi(A, w, h, r, k, k, c);
//     printf("Matrix w*h is: \n");
//     matrix_print(A, r, c);

//     printf("Total elapsed time is: %.4lf\n", (time_end - time_begin));
//     printf("Section time costs are:\n s1=%.4f \n s2=%.4f \n s3=%.4f \n s4=%.4f \n s5=%.4f\n",
//         time_s1, time_s2, time_s3, time_s4, time_s5);

//     return 0;
// }