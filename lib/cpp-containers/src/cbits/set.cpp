#include "set.h"

ptrset_t *to_set(void *ptr) {
    return static_cast<ptrset_t *>(ptr);
}

void *to_void(ptrset_t *s) {
    return static_cast<void *>(s);
}

extern "C" {

void *c_set_create() {
    return to_void(new ptrset_t());
}

void c_set_insert(elem_t elem, void *ptr) {
    to_set(ptr)->insert(elem);
}

long c_set_member(elem_t elem, void *ptr) {
    auto s = to_set(ptr);
    return (s->find(elem) != s->end()) ? 1 : 0;
}

void c_set_delete(elem_t elem, void *ptr) {
    to_set(ptr)->erase(elem);
}

void c_set_to_list(elem_t *arr, void *ptr) {
    auto s = to_set(ptr);
    long idx = 0;
    for (const elem_t& el : *s)
        arr[idx++] = el;
}

void c_set_delete_set(void *ptr) {
    auto s = to_set(ptr);
    delete s;
}

long c_set_size(void *ptr) {
    return to_set(ptr)->size();
}

long c_set_null(void *ptr) {
    return (to_set(ptr)->empty()) ? 1 : 0;
}

}
