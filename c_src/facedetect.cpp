#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <stdio.h>
extern "C" {
#include "facedetect.h"

  int detect(char *img, char *xml, s_face *f) {
    std::vector<cv::Rect> faces;
    cv::Mat image;
    cv::CascadeClassifier face_cascade;
    const float size_factor = 1.1;
    const std::size_t num_buffers = 2;
    const cv::Size face_size(30, 30);
    
    image = cv::imread(img, CV_LOAD_IMAGE_COLOR);  
    if(image.empty()) {
      return 1;
    }

    // Load Face cascade (.xml file)
    face_cascade.load(xml);

    // Detect faces
    face_cascade.detectMultiScale(
        image, 
        faces, 
        size_factor, 
        num_buffers, 
        0|CV_HAAR_SCALE_IMAGE, 
        face_size);

    // Draw circles on the detected faces
    if(faces.size() > 0) {
      f->x = faces[0].x;
      f->y = faces[0].y;
      f->width = faces[0].width;
      f->height = faces[0].height;
      return 0;
    }

    return 2;
  }
}
