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

ERL_NIF_TERM maps_put(
    ErlNifEnv* env, 
    ERL_NIF_TERM map, 
    char *key, 
    int value) {
  ERL_NIF_TERM out = enif_make_new_map(env);
  enif_make_map_put(env, map, mk_atom(env, key), enif_make_int(env, value), &out);
  return out;
}

ERL_NIF_TERM mk_eyes(ErlNifEnv *env, int x, int y, int width, int height) {
  return enif_make_list4(
      env,
      enif_make_tuple2(
        env, 
        mk_atom(env, "x"), 
        enif_make_int(env, x)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "y"), 
        enif_make_int(env, y)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "width"), 
        enif_make_int(env, width)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "height"), 
        enif_make_int(env, height)));
}

ERL_NIF_TERM mk_face(ErlNifEnv *env, int x, int y, int width, int height, const ERL_NIF_TERM eyes[], unsigned nb_eyes) {
  return enif_make_list5(
      env,
      enif_make_tuple2(
        env, 
        mk_atom(env, "x"), 
        enif_make_int(env, x)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "y"), 
        enif_make_int(env, y)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "width"), 
        enif_make_int(env, width)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "height"), 
        enif_make_int(env, height)),
      enif_make_tuple2(
        env, 
        mk_atom(env, "eyes"), 
        enif_make_list_from_array(env, eyes, nb_eyes))
      );
}

static ERL_NIF_TERM faces(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int rcod;
  s_detect faces;
  ErlNifBinary img;
  ErlNifBinary face_xml;
  ErlNifBinary eyes_xml;
  char *cimg;
  char *cface_xml;
  char *ceyes_xml;
  ERL_NIF_TERM result;

  if(argc != 3 
      || !enif_inspect_binary(env, argv[0], &img)
      || !enif_inspect_binary(env, argv[1], &face_xml)
      || !enif_inspect_binary(env, argv[2], &eyes_xml)) {
    return enif_make_badarg(env);
  }

  cimg = bin2str(img);
  cface_xml = bin2str(face_xml); 
  ceyes_xml = bin2str(eyes_xml); 

  rcod = detect(cimg, cface_xml, ceyes_xml, &faces); 
  if(rcod == OK) {
    ERL_NIF_TERM *erl_faces = (ERL_NIF_TERM*)malloc(sizeof(ERL_NIF_TERM)*faces.nb_faces);

    for(size_t i = 0; i < faces.nb_faces; i++) {
      ERL_NIF_TERM *erl_eyes = NULL;

      if(faces.faces[i].nb_eyes > 0) {
        erl_eyes = (ERL_NIF_TERM*)malloc(sizeof(ERL_NIF_TERM)*faces.faces[i].nb_eyes);

        for(size_t j = 0; j < faces.faces[i].nb_eyes; j++) {
          erl_eyes[j] = mk_eyes(env, 
              faces.faces[i].eyes[j].x, 
              faces.faces[i].eyes[j].y, 
              faces.faces[i].eyes[j].width, 
              faces.faces[i].eyes[j].height);
        }
      }
      erl_faces[i] = mk_face(env,
          faces.faces[i].x,
          faces.faces[i].y,
          faces.faces[i].width,
          faces.faces[i].height,
          erl_eyes,
          faces.faces[i].nb_eyes);
    }
    result = enif_make_tuple2(
        env, 
        mk_atom(env, "ok"),
        enif_make_list_from_array(env, erl_faces, faces.nb_faces));
  } else if(rcod == EMPTY_IMAGE) {
    result = mk_error(env, "invalid_image");
  } else if(rcod == NO_FACE) {
    result = mk_error(env, "no_face_found");
  } else {
    result = mk_error(env, "detect_error");
  }

  free(cimg);
  free(cface_xml);
  free(ceyes_xml);
  free_detect(faces);

  return result;
}

static ErlNifFunc nif_funcs[] = {
  {"faces", 3, faces}
};

ERL_NIF_INIT(evic_facedetect, nif_funcs, NULL, NULL, NULL, NULL);
