module Experiment.Common where

import Prelude

header :: String
header = "#include\"ppm.h\"\n"

-- expects 1377979715
loaderCRC :: String
loaderCRC = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryWrd  aryIn = newAry(Wrd,size(imgIn));\
\\n  for (unsigned int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAry(aryIn , i , imgIn.data[i]);\
\\n  unsigned int out;\
\\n  out = func(aryIn);\
\\n  printf(\"%u\",out);\
\\n  return 0;\
\\n}"

loaderFFT :: String
loaderFFT = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryCmx  aryIn = newAry(Cmx,size(imgIn)); \
\\n  for (unsigned int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAry(aryIn , i , cmx(i2f(imgIn.data[i]),0.0));\
\\n  AryCmx aryOut;\
\\n  aryOut = func(aryIn);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(len(aryOut) * sizeof(unsigned int))}; \
\\n  for(unsigned int i = 0; i < len(aryOut); i++)\
\\n    imgOut.data[i] = (unsigned int)(floorf(cabsf(ind(aryOut , i))));\
\\n  writeImage (\"Experiment/ImageFFT.pgm\" , imgOut);\
\\n  return 0;\
\\n}"

loaderIPBW :: String
loaderIPBW = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryWrd aryIn = newAry(Wrd,size(imgIn)); \
\\n  for (unsigned int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAry(aryIn , i , imgIn.data[i]);\
\\n  AryWrd aryOut;\
\\n  aryOut = func(aryIn);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 1,\
\\n                  .data  = malloc(len(aryOut) * sizeof(unsigned int))}; \
\\n  for(unsigned int i = 0; i < len(aryOut); i++)\
\\n    imgOut.data[i] = ind(aryOut , i);\
\\n  writeImage (\"Experiment/ImageIPBW.pbm\" , imgOut);\
\\n  return 0;\
\\n}"

loaderIPGray :: String
loaderIPGray = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/Image.ppm\");\
\\n  AryWrd  aryIn = newAry(Wrd,size(imgIn)); \
\\n  for (unsigned int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAry(aryIn , i , imgIn.data[i]);\
\\n  AryWrd aryOut;\
\\n  aryOut = func(aryIn);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(len(aryOut) * sizeof(unsigned int))}; \
\\n  for(unsigned int i = 0; i < len(aryOut); i++)\
\\n    imgOut.data[i] = ind(aryOut , i);\
\\n  writeImage (\"Experiment/ImageIPGray.pgm\" , imgOut);\
\\n  return 0;\
\\n}"


loaderWindowing :: String
loaderWindowing = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryCmx  aryIn = newAry(Cmx,size(imgIn)); \
\\n  for (unsigned int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAry(aryIn , i , cmx(i2f(imgIn.data[i]),0.0));\
\\n  AryCmx aryOut;\
\\n  aryOut = func(aryIn);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(len(aryOut) * sizeof(unsigned int))}; \
\\n  for(unsigned int i = 0; i < len(aryOut); i++)\
\\n    imgOut.data[i] = (unsigned int)(floorf(cabsf(ind(aryOut , i))));\
\\n  writeImage (\"Experiment/ImageWindowing.pgm\" , imgOut);\
\\n  return 0;\
\\n}"
