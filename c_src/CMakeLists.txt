cmake_minimum_required(VERSION 3.5)
project(EVIC_FACEDETECT)

# Load modules
set(CMAKE_MODULE_PATH ${EVIC_FACEDETECT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

# Output in `priv` dir
set(
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  ${EVIC_FACEDETECT_SOURCE_DIR}/../priv
  )

find_package(Erlang REQUIRED)
find_package(OpenCV REQUIRED)

INCLUDE_DIRECTORIES(
  ${ERLANG_EI_INCLUDE_PATH}
  ${ERLANG_ERTS_INCLUDE_PATH}
  )

LINK_DIRECTORIES(
  ${ERLANG_EI_LIBRARY_PATH}
  ${Boost_LIBRARY_DIRS}
  )

add_library(evic_facedetect SHARED facedetect.cpp evic_facedetect.c)
SET_TARGET_PROPERTIES(evic_facedetect PROPERTIES PREFIX "")
target_link_libraries(evic_facedetect erl_interface ei ${OpenCV_LIBS})

add_executable(evic_test facedetect.cpp main.c)
target_link_libraries(evic_test ${OpenCV_LIBS})
