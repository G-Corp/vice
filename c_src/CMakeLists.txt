cmake_minimum_required(VERSION 2.8)
project(VICE_FACEDETECT)
if(NOT WIN32)
  string(ASCII 27 Esc)
  set(ColourReset "${Esc}[m")
  set(ColourBold  "${Esc}[1m")
  set(Red         "${Esc}[31m")
  set(Green       "${Esc}[32m")
  set(Yellow      "${Esc}[33m")
  set(Blue        "${Esc}[34m")
  set(Magenta     "${Esc}[35m")
  set(Cyan        "${Esc}[36m")
  set(White       "${Esc}[37m")
  set(BoldRed     "${Esc}[1;31m")
  set(BoldGreen   "${Esc}[1;32m")
  set(BoldYellow  "${Esc}[1;33m")
  set(BoldBlue    "${Esc}[1;34m")
  set(BoldMagenta "${Esc}[1;35m")
  set(BoldCyan    "${Esc}[1;36m")
  set(BoldWhite   "${Esc}[1;37m")
endif()

function(message)
  list(GET ARGV 0 MessageType)
  if(MessageType STREQUAL FATAL_ERROR OR MessageType STREQUAL SEND_ERROR)
    list(REMOVE_AT ARGV 0)
    _message(${MessageType} "${BoldRed}${ARGV}${ColourReset}")
  elseif(MessageType STREQUAL WARNING)
    list(REMOVE_AT ARGV 0)
    _message(${MessageType} "${BoldYellow}${ARGV}${ColourReset}")
  elseif(MessageType STREQUAL AUTHOR_WARNING)
    list(REMOVE_AT ARGV 0)
    _message(${MessageType} "${BoldCyan}${ARGV}${ColourReset}")
  elseif(MessageType STREQUAL STATUS)
    list(REMOVE_AT ARGV 0)
    _message(${MessageType} "${Green}${ARGV}${ColourReset}")
  else()
    _message("${ARGV}")
  endif()
endfunction()

IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(OSX_LDFLAGS "-flat_namespace -undefined suppress")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
  set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${OSX_LDFLAGS}")
ENDIF()

# Load modules
set(CMAKE_MODULE_PATH ${VICE_FACEDETECT_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

# Output in `priv` dir
set(
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  ${VICE_FACEDETECT_SOURCE_DIR}/../priv
  )

find_package(Erlang QUIET)
find_package(OpenCV QUIET)

IF(ERLANG_EI_LIBRARY_PATH AND ERLANG_ERTS_INCLUDE_PATH AND ERLANG_EI_INCLUDE_PATH AND OpenCV_LIBS)
  INCLUDE_DIRECTORIES(
    ${ERLANG_EI_INCLUDE_PATH}
    ${ERLANG_ERTS_INCLUDE_PATH}
    )

  LINK_DIRECTORIES(
    ${ERLANG_EI_LIBRARY_PATH}
    )

  add_library(vice_facedetect SHARED facedetect.cpp vice_facedetect.c)
  SET_TARGET_PROPERTIES(vice_facedetect PROPERTIES PREFIX "")
  target_link_libraries(vice_facedetect erl_interface ei ${OpenCV_LIBS})

  #add_executable(vice_test facedetect.cpp main.c)
  #target_link_libraries(vice_test ${OpenCV_LIBS})
ELSE()
  MESSAGE(WARNING "Missing libraries to compile vice_facedetect. This module will not be available.")
ENDIF()
