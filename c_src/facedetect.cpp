#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <stdio.h>
extern "C" {
#include "facedetect.h"
  void free_detect(s_detect data) {
    if(data.nb_faces > 0) {
      for(size_t i = 0; i < data.nb_faces; i++) {
        if(data.faces[i].nb_eyes > 0) {
          free(data.faces[i].eyes);
        }
      }
      free(data.faces);
    }
    return;
  }

  int detect(char *img, char *face, char *eyes, s_detect *result) {
    std::vector<cv::Rect> faces;
    cv::Mat image;
    cv::CascadeClassifier face_cascade;
    cv::CascadeClassifier eyes_cascade;
    const float size_factor = 1.1;
    const std::size_t num_buffers = 2;
    const cv::Size cascade_size(30, 30);
    
    image = cv::imread(img, CV_LOAD_IMAGE_COLOR);  
    if(image.empty()) {
      return EMPTY_IMAGE;
    }

    // Load Face cascade (.xml file)
    if(!face_cascade.load(face)) { return LOAD_FACE_CASCADE_ERROR; }
    if(!eyes_cascade.load(eyes)) { return LOAD_EYES_CASCADE_ERROR; }

    // Detect faces
    face_cascade.detectMultiScale(
        image, 
        faces, 
        size_factor, 
        num_buffers, 
        0|CV_HAAR_SCALE_IMAGE, 
        cascade_size);

    // Draw circles on the detected faces
    if(faces.size() > 0) {
      result->nb_faces = faces.size();
      result->faces = (s_face*)malloc(sizeof(s_face)*faces.size());

      for(size_t i = 0; i < faces.size(); i++) {
        (result->faces)[i].x = faces[i].x;
        (result->faces)[i].y = faces[i].y;
        (result->faces)[i].width = faces[i].width;
        (result->faces)[i].height = faces[i].height;

        cv::Mat face = image(faces[i]);
        std::vector<cv::Rect> eyes;

        eyes_cascade.detectMultiScale(
            face, 
            eyes, 
            size_factor,
            num_buffers,
            0|CV_HAAR_SCALE_IMAGE,
            cascade_size);

        (result->faces)[i].nb_eyes = eyes.size();
        if(eyes.size() > 0) {
          (result->faces)[i].eyes = (s_eye*)malloc(sizeof(s_eye)*eyes.size());
          for(size_t j = 0; j < eyes.size(); j++) {
            (result->faces)[i].eyes[j].x = eyes[j].x;
            (result->faces)[i].eyes[j].y = eyes[j].y;
            (result->faces)[i].eyes[j].width = eyes[j].width;
            (result->faces)[i].eyes[j].height = eyes[j].height;
          }
        }
      }
      return OK;
    }

    return NO_FACE;
  }
}
