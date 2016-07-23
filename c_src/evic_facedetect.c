#include <stdio.h>
#include <string.h>
#include "facedetect.h"
#include "erl_nif.h"

ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom) {
  ERL_NIF_TERM ret;

  if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
    return enif_make_atom(env, atom);
  }

  return ret;
}

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg) {
  return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

char * bin2str(ErlNifBinary bin) {
  char *str = (char*)malloc(sizeof(char)*(bin.size + 1));
  memset(str, 0, bin.size + 1);
  strncpy(str, bin.data, bin.size);
  return str;
}

static ERL_NIF_TERM face(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int rcod;
  s_face face;
  ErlNifBinary img;
  ErlNifBinary xml;
  char *cimg;
  char *cxml;
  ERL_NIF_TERM result;

  if(argc != 2 
      || !enif_inspect_binary(env, argv[0], &img)
      || !enif_inspect_binary(env, argv[1], &xml)) {
    return enif_make_badarg(env);
  }

  cimg = bin2str(img);
  cxml = bin2str(xml); 

  rcod = detect(cimg, cxml, &face); 
  if(rcod == 0) {
    result = enif_make_tuple2(
        env, 
        mk_atom(env, "ok"),
        enif_make_list4(
          env,
          enif_make_tuple2(
            env, 
            mk_atom(env, "x"), 
            enif_make_int(env, face.x)),
          enif_make_tuple2(
            env, 
            mk_atom(env, "y"), 
            enif_make_int(env, face.y)),
          enif_make_tuple2(
            env, 
            mk_atom(env, "width"), 
            enif_make_int(env, face.width)),
          enif_make_tuple2(
            env, 
            mk_atom(env, "height"), 
            enif_make_int(env, face.height))));
  } else if(rcod == 1) {
    result = mk_error(env, "image_error");
  } else {
    result = mk_error(env, "detect_error");
  }

  free(cimg);
  free(cxml);

  return result;
}

static ErlNifFunc nif_funcs[] = {
  {"face", 2, face}
};

ERL_NIF_INIT(evic_facedetect, nif_funcs, NULL, NULL, NULL, NULL);
