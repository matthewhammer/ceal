/* Ezgi Cicek <ecicek@mpi-sws.org>
 * Convolution with 1-1 correspondence
 * 1 pixel for each 1 modref
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "conv.c"

///////////////////////////////////////////////////////////////////////////////
// Simplest 2D convolution routine. It is easy to understand how convolution
// works, but is very slow, because of no optimization.
///////////////////////////////////////////////////////////////////////////////
void convolve2DSlow(pixel_t* in, pixel_t* out,long dataSizeX, long  dataSizeY,
                    kernel_t* kernel, long kernelSizeX, long kernelSizeY)
{
    long i, j, m, n, mm, nn;
    long kCenterX, kCenterY;                         // center index of kernel
    kernel_t sum;                                      // temp accumulation buffer
    long rowIndex, colIndex;

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
                        long address = dataSizeX * rowIndex + colIndex;
                        pixel_t input =  in[address];
                        sum += input* kernel[kernelSizeX * mm + nn];
                    }
                }
            }
            out[dataSizeX * i + j] = (pixel_t)((kernel_t)fabs(sum) + 0.5f);
        }
    }

}

#if CONV_NORMAL
int main(int argc, char **argv)
{
    pixel_t* in = malloc( sizeof(pixel_t) * ROW * COL);
    pixel_t* out =  malloc( sizeof( pixel_t) * ROW * COL);

    //load raw image
    loadRawImage( "lena.raw" , ROW, COL, in);

    // define 5x5 Gaussian kernel
    long kernelRow = 5, kernelCol = 5;
    kernel_t kernel[25] = { 1/256.0f,  4/256.0f,  6/256.0f,  4/256.0f, 1/256.0f,
                         4/256.0f, 16/256.0f, 24/256.0f, 16/256.0f, 4/256.0f,
                         6/256.0f, 24/256.0f, 36/256.0f, 24/256.0f, 6/256.0f,
                         4/256.0f, 16/256.0f, 24/256.0f, 16/256.0f, 4/256.0f,
                         1/256.0f,  4/256.0f,  6/256.0f,  4/256.0f, 1/256.0f };

    // convolve the image stored in 'in' -> 'out' with 'kernel'
    convolve2DSlow( in, out,ROW, COL, kernel, kernelRow, kernelCol);

    //write the colvolved image;
    writeRawImage( "lena_conv_1_1.raw", ROW, COL, out);
    return 0;
}
#endif
