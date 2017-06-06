#include <stdio.h>
#include "facedetect.h"

int main (int argc, char *argv[]) {
  s_detect f;
  size_t i, j;
  if(detect(argv[1],
        "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt2.xml",
        "/usr/share/opencv/haarcascades/haarcascade_eye_tree_eyeglasses.xml",
        &f) == 0) {
    for(i = 0; i < f.nb_faces; i++) {
      printf("FACE #%ld\n", i);
      printf("X = %d\n", f.faces[i].x);
      printf("Y = %d\n", f.faces[i].y);
      printf("WIDTH = %d\n", f.faces[i].width);
      printf("HEIGTH = %d\n", f.faces[i].height);
      for(j = 0; j < f.faces[i].nb_eyes; j++) {
        printf("--EYE #%ld\n", j);
        printf("  X = %d\n", f.faces[i].eyes[j].x);
        printf("  Y = %d\n", f.faces[i].eyes[j].y);
        printf("  WIDTH = %d\n", f.faces[i].eyes[j].width);
        printf("  HEIGTH = %d\n", f.faces[i].eyes[j].height);
      }
    }
    free_detect(f);
  } else {
    printf("Missing param\n");
  }
  return 0;
}

