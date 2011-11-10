/* Ezgi Cicek <ecicek@mpi-sws.org>
 * Convolution with 1-1 correspondence
 * 1 pixel for each 1 modref
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "conv.c"

pixel_t get_pixel_value( int k_sq, int loc, pixel_t** in){
    return in[loc/k_sq][loc%k_sq];
}

void put_pixel_value( int k, int loc, pixel_t** out, pixel_t value){
    out[loc/k][loc%k] = value;
}

void single_double_layout(pixel_t* in, int mod_size, int k, pixel_t** mod_in){
    int i;
    for( i =0; i < mod_size; i++ ){
        int j;
        pixel_t* k_sq = malloc( sizeof(pixel_t) *k );
        for ( j=0; j < k; j++){
            k_sq[j] = in[i*k+j];
        }
        mod_in[i] = k_sq;
    }
}

void double_single_layout(pixel_t* out, int mod_size, int k, pixel_t** mod_out){
    int i;
    for( i =0; i < mod_size; i++ ){
        int j;
        pixel_t* k_sq = mod_out[i];
        for ( j=0; j < k; j++){
            out[i*k+j] = k_sq[j];
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// Simplest 2D convolution routine. It is easy to understand how convolution
// works, but is very slow, because of no optimization.
///////////////////////////////////////////////////////////////////////////////
void convolve2DSlow(pixel_t** in, pixel_t** out, int dataSizeX, int dataSizeY,
                    kernel_t* kernel, int kernelSizeX, int kernelSizeY)
{
    int i, j, m, n, mm, nn;
    int kCenterX, kCenterY;                         // center index of kernel
    kernel_t sum;                                      // temp accumulation buffer
    int rowIndex, colIndex;

    // check validity of params
    if(!in || !out || !kernel) printf("invalid in/out/kernel");
    if(dataSizeX <= 0 || kernelSizeX <= 0) printf(" invalid size for data/kernel");

    // find center position of kernel (half of kernel size)
    kCenterX = kernelSizeX / 2;
    kCenterY = kernelSizeY / 2;

    for(i=0; i < dataSizeY; ++i)                // rows
    {
        for(j=0; j < dataSizeX; ++j)            // columns
        {
            sum = 0;                            // init to 0 before sum
            for(m=0; m < kernelSizeY; ++m)      // kernel rows
            {
                mm = kernelSizeY - 1 - m;       // row index of flipped kernel

                for(n=0; n < kernelSizeX; ++n)  // kernel columns
                {
                    nn = kernelSizeX - 1 - n;   // column index of flipped kernel

                    // index of input signal, used for checking boundary
                    rowIndex = i + m - kCenterY;
                    colIndex = j + n - kCenterX;

                    // ignore input samples which are out of bound
                    if(rowIndex >= 0 && rowIndex < dataSizeY && colIndex >= 0 && colIndex < dataSizeX)
                    {
                        pixel_t input = get_pixel_value( K*K, dataSizeX * rowIndex + colIndex, in);
                        sum += input* kernel[kernelSizeX * mm + nn];
                    }
                }
            }
            put_pixel_value( K*K, dataSizeX * i + j, out, (pixel_t)((kernel_t)fabs(sum) + 0.5f));
        }
    }

}


int main(int argc, char **argv)
{
    int mod_size = (ROW * COL ) / ( K * K);

    pixel_t* in = malloc( sizeof(pixel_t) * ROW * COL);
    pixel_t* out =  malloc( sizeof( pixel_t) * ROW * COL);
    pixel_t** mod_out = malloc( sizeof(pixel_t*) * mod_size);
    pixel_t** mod_in = malloc( sizeof(pixel_t*) *  mod_size);

    //load raw image
    loadRawImage( "lena.raw" , ROW, COL, in);

    single_double_layout(in, mod_size, K*K, mod_in);
    single_double_layout(out, mod_size, K*K, mod_out);

    // define 5x5 Gaussian kernel
    int kernelRow = 5, kernelCol = 5;
    kernel_t kernel[25] = { 1/256.0f,  4/256.0f,  6/256.0f,  4/256.0f, 1/256.0f,
                         4/256.0f, 16/256.0f, 24/256.0f, 16/256.0f, 4/256.0f,
                         6/256.0f, 24/256.0f, 36/256.0f, 24/256.0f, 6/256.0f,
                         4/256.0f, 16/256.0f, 24/256.0f, 16/256.0f, 4/256.0f,
                         1/256.0f,  4/256.0f,  6/256.0f,  4/256.0f, 1/256.0f };

    // convolve the image stored in 'in' -> 'out' with 'kernel'
    convolve2DSlow( mod_in, mod_out,ROW, COL, kernel, kernelRow, kernelCol);

    //write the colvolved image;
    double_single_layout(out, mod_size, K*K, mod_out);
    writeRawImage( "lena_conv_single.raw", ROW, COL, out);
    return 0;
}
