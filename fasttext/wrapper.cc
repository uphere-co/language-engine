#include <vectorwrapper.h>
#include <fastTextType.h>

extern "C" {
    float* get_vector(Vector_t* vec);
}

using namespace fasttext;

float* get_vector(Vector_t* vec) {
    return ((Vector*)vec) -> data_; 
}
