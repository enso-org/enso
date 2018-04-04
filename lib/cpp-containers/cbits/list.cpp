#include "list.h"

ptrlist_t *to_list(void *ptr) {
    return static_cast<ptrlist_t *>(ptr);
}

void *to_void(ptrlist_t *s) {
    return static_cast<void *>(s);
}

extern "C" {

void *c_list_create() {
    return to_void(new ptrlist_t());
}

void c_list_push_front(elem_t elem, void *ptr) {
    to_list(ptr)->push_front(elem);
}

void c_list_push_back(elem_t elem, void *ptr) {
    to_list(ptr)->push_back(elem);
}

elem_t c_list_pop_front(void *ptr) {
    auto l = to_list(ptr);
    elem_t res = l->front();
    to_list(ptr)->pop_front();
    return res;
}

elem_t c_list_pop_back(void *ptr) {
    auto l = to_list(ptr);
    elem_t res = l->back();
    to_list(ptr)->pop_back();
    return res;
}

elem_t c_list_at(long idx, void *ptr) {
    return to_list(ptr)->at(idx);
}

elem_t c_list_front(void *ptr) {
    return to_list(ptr)->front();
}

elem_t c_list_back(void *ptr) {
    return to_list(ptr)->back();
}

void c_list_to_list(elem_t *arr, void *ptr) {
    auto l = to_list(ptr);
    long idx = 0;
    for (const elem_t& el : *l)
        arr[idx++] = el;
}

void c_list_delete_list(void *ptr) {
    auto l = to_list(ptr);
    delete l;
}

long c_list_size(void *ptr) {
    return to_list(ptr)->size();
}

long c_list_null(void *ptr) {
    return (to_list(ptr)->empty()) ? 1 : 0;
}

}

