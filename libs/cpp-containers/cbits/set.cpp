#include "set.h"

std::set<long> *to_set(void *ptr) {
    return static_cast<std::set<long> *>(ptr);
}

void *to_void(std::set<long> *s) {
    return static_cast<std::set<long> *>(s);
}

extern "C" {

void *c_create_std_set() {
    return to_void(new std::set<long>());
}

void c_insert(long elem, void *ptr) {
    to_set(ptr)->insert(elem);
}

void c_insert_many(long *elems, long n, void *ptr) {
    auto s = to_set(ptr);
    for (long i = 0; i < n; ++i)
        s->insert(elems[i]);
}

long c_member(long elem, void *ptr) {
    auto s = to_set(ptr);
    return (s->find(elem) != s->end()) ? 1 : 0;
}

void c_delete(long elem, void *ptr) {
    to_set(ptr)->erase(elem);
}

void c_to_list(long *arr, void *ptr) {
    auto s = to_set(ptr);
    long idx = 0;
    for (const long& el : *s)
        arr[idx++] = el;
}

void c_delete_set(void *ptr) {
    auto s = to_set(ptr);
    delete s;
}

long c_size(void *ptr) {
    return to_set(ptr)->size();
}

bool c_null(void *ptr) {
    return to_set(ptr)->empty();
}

}
