#include <stdio.h>
#include "facedetect.h"

int main (int argc, char *argv[]) {
  s_face f;
  if(detect(argv[1], "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt2.xml", &f) == 0) {
    printf("X = %d\n", f.x);
    printf("Y = %d\n", f.y);
    printf("WIDTH = %d\n", f.width);
    printf("HEIGTH = %d\n", f.height);
  } else {
    printf("Missing param\n");
  }
  return 0;
}

