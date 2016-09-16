cmake_minimum_required(VERSION 3.5)
project(VICE_FACEDETECT)

# Load modules
set(CMAKE_MODULE_PATH ${VICE_FACEDETECT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

# Output in `priv` dir
set(
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  ${VICE_FACEDETECT_SOURCE_DIR}/../priv
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

add_library(vice_facedetect SHARED facedetect.cpp vice_facedetect.c)
SET_TARGET_PROPERTIES(vice_facedetect PROPERTIES PREFIX "")
target_link_libraries(vice_facedetect erl_interface ei ${OpenCV_LIBS})

add_executable(vice_test facedetect.cpp main.c)
target_link_libraries(vice_test ${OpenCV_LIBS})
