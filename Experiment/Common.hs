module Experiment.Common where

import Prelude

header :: String
header = "#include\"ppm.h\"\n"

loaderCRC :: String
loaderCRC = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryInt  aryIn = newAryInt(size(imgIn));\
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryInt(aryIn , i , imgIn.data[i]);\
\\n  Int out;\
\\n  func(aryIn , &out);\
\\n  printf(\"%u\",out);\
\\n  return 0;\
\\n}"

loaderFFT :: String
loaderFFT = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryCmx  aryIn = newAryCmx(size(imgIn)); \
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryCmx(aryIn , i , cmx(i2f(imgIn.data[i]),0.0));\
\\n  AryCmx aryOut;\
\\n  func(aryIn , &aryOut);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(lenAryCmx(aryOut) * sizeof(Int))}; \
\\n  for(Int i = 0; i < lenAryCmx(aryOut); i++)\
\\n    imgOut.data[i] = (Int)(floorf(cabsf(indAryCmx(aryOut , i))));\
\\n  writeImage (\"Experiment/ImageFFT.pgm\" , imgOut);\
\\n  return 0;\
\\n}"

loaderIPBW :: String
loaderIPBW = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryInt  aryIn = newAryInt(size(imgIn)); \
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryInt(aryIn , i , imgIn.data[i]);\
\\n  AryInt aryOut;\
\\n  func(aryIn , &aryOut);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 1,\
\\n                  .data  = malloc(lenAryInt(aryOut) * sizeof(Int))}; \
\\n  for(Int i = 0; i < lenAryInt(aryOut); i++)\
\\n    imgOut.data[i] = indAryInt(aryOut , i);\
\\n  writeImage (\"Experiment/ImageIPBW.pbm\" , imgOut);\
\\n  return 0;\
\\n}"

loaderIPGray :: String
loaderIPGray = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/Image.ppm\");\
\\n  AryInt  aryIn = newAryInt(size(imgIn)); \
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryInt(aryIn , i , imgIn.data[i]);\
\\n  AryInt aryOut;\
\\n  func(aryIn , &aryOut);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(lenAryInt(aryOut) * sizeof(Int))}; \
\\n  for(Int i = 0; i < lenAryInt(aryOut); i++)\
\\n    imgOut.data[i] = indAryInt(aryOut , i);\
\\n  writeImage (\"Experiment/ImageIPGray.pgm\" , imgOut);\
\\n  return 0;\
\\n}"


loaderWindowing :: String
loaderWindowing = "\nint main()\
\\n{\
\\n  Image   imgIn = readImage(\"Experiment/ImageBig.pgm\");\
\\n  AryCmx  aryIn = newAryCmx(size(imgIn)); \
\\n  for (Int i = 0; i < size(imgIn); i++)\
\\n    aryIn = setAryCmx(aryIn , i , cmx(i2f(imgIn.data[i]),0.0));\
\\n  AryCmx aryOut;\
\\n  func(aryIn , &aryOut);\
\\n  Image imgOut = {.sizeX = imgIn.sizeX, \
\\n                  .sizeY = imgIn.sizeY,\
\\n                  .type  = 2,\
\\n                  .data  = malloc(lenAryCmx(aryOut) * sizeof(Int))}; \
\\n  for(Int i = 0; i < lenAryCmx(aryOut); i++)\
\\n    imgOut.data[i] = (Int)(floorf(cabsf(indAryCmx(aryOut , i))));\
\\n  writeImage (\"Experiment/ImageWindowing.pgm\" , imgOut);\
\\n  return 0;\
\\n}"