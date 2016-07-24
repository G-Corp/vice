#ifndef __EVIC_FACEDETECT__
#define __EVIC_FACEDETECT__

#define OK 0
#define EMPTY_IMAGE 1
#define LOAD_EYES_CASCADE_ERROR 2
#define LOAD_FACE_CASCADE_ERROR 3
#define NO_FACE 4

typedef struct {
  int x;
  int y;
  int width;
  int height;
} s_eye;

typedef struct {
  int x;
  int y;
  int width;
  int height;
  size_t nb_eyes;
  s_eye *eyes;
} s_face;

typedef struct {
  size_t nb_faces;
  s_face *faces;
} s_detect;

int detect(char *, char *, char *, s_detect *);
void free_detect(s_detect);

#endif
