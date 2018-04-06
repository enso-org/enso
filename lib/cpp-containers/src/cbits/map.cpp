#include "map.h"
#include <iostream>

ptrmap_t *to_map(void *ptr) {
    return static_cast<ptrmap_t *>(ptr);
}

void *to_void(ptrmap_t *s) {
    return static_cast<void *>(s);
}

extern "C" {

void *c_map_create() {
    return to_void(new ptrmap_t());
}

void c_map_insert(elem_t key, elem_t elem, void *ptr) {
    to_map(ptr)->insert(std::pair<elem_t, elem_t>(key, elem));
}

long c_map_member(elem_t elem, void *ptr) {
    auto m = to_map(ptr);
    return (m->find(elem) != m->end()) ? 1 : 0;
}

long c_map_lookup(elem_t elem, elem_t *found, void *ptr) {
    auto m = to_map(ptr);
    auto res = m->find(elem);
    if (res != m->end()) {
        *found = res->second;
        return 1;
    } else return 0;
}

void c_map_delete(elem_t elem, void *ptr) {
    to_map(ptr)->erase(elem);
}

void c_map_to_list(elem_t *keys, elem_t *vals, void *ptr) {
    auto m = *to_map(ptr);
    long idx = 0;
    for (std::pair<elem_t, elem_t> p : m) {
        keys[idx] = p.first;
        vals[idx] = p.second;
        idx++;
    }
}

void c_map_delete_map(void *ptr) {
    auto m = to_map(ptr);
    delete m;
}

long c_map_size(void *ptr) {
    return to_map(ptr)->size();
}

long c_map_null(void *ptr) {
    return (to_map(ptr)->empty()) ? 1 : 0;
}

}
