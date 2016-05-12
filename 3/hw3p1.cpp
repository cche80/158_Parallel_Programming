#include <cstdio>
#include <ctime>
#include <omp.h>
#include <sys/time.h>

using namespace std;

double get_wall_time(){
    struct timeval time;
    if (gettimeofday(&time,NULL)) return 0;
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
}

void findpaths(int *adjm, int n, int k, int *paths, int *numpaths) {
    int count = 0;
    int times = n;
    for (int i = 0; i < k; ++i) {times = times * n;}

if (times < 5000)
{   //printf("here\n");
    int *arr = new int [k+1];
    
    for (int i = 0; i < k+1; ++i) {arr[i] = 0;}

    for (int i = 0; i < times; ++i)
    {
        int temp = i, index = k, check = 1, r;
        while(temp!=0) 
        {
            r=(temp%n);
            temp/=n;
            arr[index] = r;
            index--;
        }

        for (int i = 0; i < k; ++i) 
        {
            check *= adjm[arr[i]*n+arr[i+1]];
        }

        if(check) 
        {
            for (int i = 0; i < k+1; ++i)
            {
                paths[count*(k+1)+i] = arr[i];
                //printf("%d->", arr[i]);
            }
            //printf("\n");
            count ++;
        }
    }
}else {

#pragma omp parallel
{
    int num = omp_get_thread_num();
    int t = omp_get_num_threads();
    int d = times/t;

    int c = 0;
    int *arr = new int [k+1];
    int *p = new int [times*(k+1)/t];

    
    for (int i = 0; i < k+1; ++i) {arr[i] = 0;}
    
    for (int i=(d*num);i<(num==(t-1) ? times : (d*num+d));i++)
    {
        int temp = i, index = k, check = 1, r;
        while(temp!=0) 
        {
            r=(temp%n);
            temp/=n;
            arr[index] = r;
            index--;
        }

        for (int i = 0; i < k; ++i) 
        {
            check *= adjm[arr[i]*n+arr[i+1]];
            if (!check)
            {
                break;
            }
        }

        if(check) 
        {
            for (int i = 0; i < k+1; ++i)
            {
                p[c*(k+1)+i] = arr[i];
                //printf("%d->", p[c*(k+1)+i]);
            }
            //printf("\n");
            c ++;
        }
    }
    int subc = count;
    count += c;
#pragma omp critical 
  {
    //printf("%d %d\n", subc*(k+1), subc*(k+1)+(c*(k+1)));
    for (int i = 0; i < (c*(k+1)); ++i)
    {
      paths[subc*(k+1)+i] = p[i];
    }
    
  }
}
}
    *numpaths = count;
}

int main(int argc, char const *argv[])
{
  omp_set_num_threads(8);
    int n = 5, k = 4;
    int path[2000000], numpaths;
    int A[400] = {
    0,    1,    1,    0,    1,    0,    0,    1,    0,     0,     1,     1,     0,     1,     0,     0,     1,     0,     0,     0,
    1,    1,    0,    1,    1,    1,    0,    0,    1,     0,     1,     0,     0,     1,     1,     0,     1,     0,     1,     1,
    1,    0,    1,    1,    0,    1,    0,    0,    0,     0,     1,     0,     0,     0,     0,     1,     0,     1,     1,     0,
    1,    0,    0,    1,    1,    1,    1,    0,    0,     0,     1,     0,     0,     1,     1,     1,     1,     0,     0,     0,
    1,    0,    1,    0,    1,    1,    1,    0,    0,     0,     0,     0,     1,     0,     0,     0,     0,     0,     1,     0,
    1,    0,    0,    0,    1,    0,    0,    1,    1,     1,     1,     0,     0,     1,     1,     1,     1,     0,     1,     1,
    1,    1,    1,    1,    1,    0,    0,    0,    1,     1,     0,     0,     1,     0,     0,     0,     1,     0,     0,     0,
    0,    0,    0,    1,    1,    0,    0,    0,    1,     0,     0,     0,     0,     0,     1,     1,     1,     0,     0,     0,
    0,    1,    1,    1,    0,    1,    0,    1,    0,     1,     0,     0,     1,     1,     0,     0,     0,     0,     1,     1,
    1,    1,    1,    0,    1,    0,    0,    1,    0,     1,     0,     1,     1,     0,     0,     0,     1,     1,     1,     1,
    0,    0,    0,    1,    1,    1,    0,    0,    0,     0,     1,     1,     1,     0,     0,     0,     0,     0,     0,     0,
    1,    0,    1,    0,    0,    1,    1,    0,    0,     1,     0,     1,     1,     1,     1,     0,     1,     0,     1,     1,
    0,    1,    1,    1,    0,    1,    1,    0,    1,     0,     1,     1,     0,     0,     0,     1,     0,     0,     1,     1,
    0,    0,    1,    0,    1,    0,    1,    1,    1,     1,     0,     1,     0,     0,     1,     0,     1,     0,     0,     0,
    0,    0,    1,    1,    1,    0,    1,    1,    0,     1,     1,     0,     0,     0,     1,     1,     0,     0,     0,     0,
    1,    0,    1,    1,    1,    0,    0,    1,    0,     1,     1,     1,     0,     0,     0,     1,     0,     1,     0,     1,
    0,    0,    1,    0,    1,    1,    1,    0,    1,     0,     0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
    0,    0,    1,    0,    0,    0,    1,    0,    1,     0,     0,     0,     0,     1,     1,     1,     0,     0,     0,     1,
    1,    0,    1,    0,    0,    1,    0,    1,    1,     0,     0,     1,     1,     1,     1,     1,     0,     0,     1,     0,
    0,    0,    0,    1,    0,    0,    0,    0,    0,     1,     1,     0,     1,     1,     1,     1,     0,     0,     0,     0};

double time_begin=get_wall_time();
    findpaths(A, n, k, path, &numpaths);
double time_end=get_wall_time();
printf("Total elapsed time is: %.4lf\n", (time_end - time_begin));

    printf("%d\n",numpaths);
//    for (int i = 0; i < numpaths; ++i)
//    {
//      for (int j = 0; j < k+1; ++j)
//      {
//        printf("%d->", path[i*(k+1)+j]);
//      }
//      printf("\n");
//    }


  return 0;
}