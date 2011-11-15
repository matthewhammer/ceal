/* Ezgi Cicek <ecicek@mpi-sws.org>
 * Convolution with 1-1 correspondence
 * 1 pixel for each 1 modref
 */
#ifndef __CONV_C__
#define __CONV_C__

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

long ROW = 256;
long COL = 256;
long K =2;
typedef long pixel_t;
typedef double kernel_t;
typedef unsigned char raw_t;

raw_t fread_raw(FILE* stream) foreign_c {
  raw_t data;
  fread(&data, sizeof(raw_t), 1, stream);
  return data;
}

void fwrite_raw(FILE* stream, raw_t data) foreign_c {
  fwrite(&data, sizeof(raw_t), 1, stream);
}

///////////////////////////////////////////////////////////////////////////////
// load 8-bit RAW image
///////////////////////////////////////////////////////////////////////////////
void loadRawImage(char *fileName, int x, int y, pixel_t *data)
{
    // check params
    if(!fileName || !data)
        printf(" either file does not exist or data is empty");

    FILE *fp;
    if((fp = fopen(fileName, "r")) == NULL)
    {
        printf("Cannot open %s.\n", fileName);
    }

    // read pixel data
    int i, j;
    for ( i = 0; i < ROW*COL; i++){
      data[i] = (pixel_t) fread_raw(fp);
      //fread(data+i, sizeof(raw_t), 1, fp);
    }
    //fread(data, sizeof(pixel_t), x*y, fp);
    fclose(fp);

}

///////////////////////////////////////////////////////////////////////////////
// write 8-bit RAW image
///////////////////////////////////////////////////////////////////////////////
void writeRawImage(char *fileName, int x, int y, pixel_t *data)
{
    // check params
    if(!fileName || !data)
        printf(" either file does not exist or data is empty");

    FILE *fp;
    if((fp = fopen(fileName, "wa+")) == NULL)
    {
        printf("Cannot open %s.\n", fileName);
    }

    int i, j;
    for ( i = 0; i < ROW*COL; i++){
      fwrite_raw(fp, data[i]);
    }
    // write pixel data
    //fwrite(data, sizeof(pixel_t), x*y, fp);
    fclose(fp);

}
#endif
