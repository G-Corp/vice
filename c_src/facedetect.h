#ifndef __EVIC_FACEDETECT__
#define __EVIC_FACEDETECT__

typedef struct face {
  int x;
  int y;
  int width;
  int height;
} s_face;

int detect(char *, char *, s_face *);

#endif
