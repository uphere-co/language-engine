#include </home/modori/repo/srcc/fastText/src/vector.h>
#include </home/modori/repo/src/semantic-role-labeler/fasttext/fastText/csrc/fastTextType.h>

extern "C" {
    float* get_vector(Vector_t* vec);
}

using namespace fasttext;

float* get_vector(Vector_t* vec) {
    return ((Vector*)vec) -> data_; 
}
