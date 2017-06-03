cmake_minimum_required(VERSION 2.8)
project(VICE_FACEDETECT)

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(OSX_LDFLAGS "-flat_namespace -undefined suppress")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
  set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
endif()

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
