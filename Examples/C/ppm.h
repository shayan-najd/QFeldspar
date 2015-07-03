#include <stdio.h>
#include <stdlib.h>

#ifndef PPM
#define PPM

typedef struct
{ unsigned int  sizeX;
  unsigned int  sizeY;
  unsigned int  type;
  unsigned int* data;
} Image;

unsigned int size(Image img)
{
  if (img.type == 3)
    {
      return (img.sizeX) * (img.sizeY) * 3;
    }
  else
    {
      return (img.sizeX) * (img.sizeY);
    }
}

Image readImage (const char* filename)
{
  FILE* fp = fopen(filename, "rb");
  Image img;
  fscanf(fp , "P%d\n%d %d\n255\n" , &img.type , &img.sizeX , &img.sizeY);
  img.data = malloc(size(img) * sizeof(unsigned int));
  for (unsigned int i = 0; i < size(img); i++)
    fscanf(fp , "%d\n" , &(img.data[i]));
  fclose(fp);
  return img;
}

void writeImage (const char* filename, Image img)
{
  FILE* fp = fopen(filename,"wb");
  fprintf(fp , "P%d\n%d %d\n255\n" , img.type , img.sizeX , img.sizeY);
  for(unsigned int i = 0; i < size(img); i++)
    fprintf(fp , "%d\n" , img.data[i]);
  fclose(fp);
}

#endif
