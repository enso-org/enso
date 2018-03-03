#include "map.h"
#include <iostream>

std::map<long, long> *to_map(void *ptr) {
    return static_cast<std::map<long, long> *>(ptr);
}

void *to_void(std::map<long, long> *s) {
    return static_cast<void *>(s);
}

extern "C" {

void *c_map_create() {
    return to_void(new std::map<long, long>());
}

void c_map_insert(long key, long elem, void *ptr) {
    to_map(ptr)->insert(std::pair<long, long>(key, elem));
}

long c_map_member(long elem, void *ptr) {
    auto m = to_map(ptr);
    return (m->find(elem) != m->end()) ? 1 : 0;
}

long c_map_lookup(long elem, long *found, void *ptr) {
    auto m = to_map(ptr);
    auto res = m->find(elem);
    if (res != m->end()) {
        *found = res->second;
        return 1;
    } else return 0;
}

void c_map_delete(long elem, void *ptr) {
    to_map(ptr)->erase(elem);
}

void c_map_to_list(long *keys, long *vals, void *ptr) {
    auto m = *to_map(ptr);
    long idx = 0;
    for (std::pair<long, long> p : m) {
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
