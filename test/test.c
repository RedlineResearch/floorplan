#include "hotspot.hpp"
#include <stdio.h>
#include <stdlib.h>

typedef long long unsigned int ShadowAddr;

void PSet(int map, ShadowAddr addr, unsigned int type, size_t size) {
  printf("ShadowSet: %d, %p, %s, %zu\n", map, addr, __FLP_TYPES[type], size);
}

int main(int argc, char *argv[]) {
  size_t obj_size = 8 * sizeof(char*);
  void *obj = malloc(obj_size);
  printf("Uncompressed object:\n");
  { int UseCompressedOops = 0;
    int is_arr = 0;
    transition_FLP_Evacuated_Object_Header(obj);
  }
  printf("Uncompressed array:\n");
  { int UseCompressedOops = 0;
    int is_arr = 1;
    transition_FLP_Evacuated_Object_Header(obj);
  }
  printf("Compressed object:\n");
  { int UseCompressedOops = 1;
    int is_arr = 0;
    transition_FLP_Evacuated_Object_Header(obj);
  }
  printf("Compressed array:\n");
  { int UseCompressedOops = 1;
    int is_arr = 1;
    transition_FLP_Evacuated_Object_Header(obj);
  }
  
  printf("Uncompressed object v2:\n");
  { int UseCompressedOops = 0;
    int is_arr = 0;
    transition_FLP_Evacuated_Object(obj, obj_size);
  }
  printf("Uncompressed array v2:\n");
  { int UseCompressedOops = 0;
    int is_arr = 1;
    transition_FLP_Evacuated_Object(obj, obj_size);
  }
  printf("Compressed object v2:\n");
  { int UseCompressedOops = 1;
    int is_arr = 0;
    transition_FLP_Evacuated_Object(obj, obj_size);
  }
  printf("Compressed array v2:\n");
  { int UseCompressedOops = 1;
    int is_arr = 1;
    transition_FLP_Evacuated_Object(obj, obj_size);
  }
}

