#include "list.h"

std::deque<long> *to_list(void *ptr) {
    return static_cast<std::deque<long> *>(ptr);
}

void *to_void(std::deque<long> *s) {
    return static_cast<std::deque<long> *>(s);
}

extern "C" {

void *c_list_create() {
    return to_void(new std::deque<long>());
}

void c_list_push_front(long elem, void *ptr) {
    to_list(ptr)->push_front(elem);
}

void c_list_push_back(long elem, void *ptr) {
    to_list(ptr)->push_back(elem);
}

long c_list_pop_front(void *ptr) {
    auto l = to_list(ptr);
    long res = l->front();
    to_list(ptr)->pop_front();
    return res;
}

long c_list_pop_back(void *ptr) {
    auto l = to_list(ptr);
    long res = l->back();
    to_list(ptr)->pop_back();
    return res;
}

long c_list_at(long idx, void *ptr) {
    return to_list(ptr)->at(idx);
}

long c_list_front(void *ptr) {
    to_list(ptr)->front();
}

long c_list_back(void *ptr) {
    to_list(ptr)->back();
}

void c_list_to_list(long *arr, void *ptr) {
    auto l = to_list(ptr);
    long idx = 0;
    for (const long& el : *l)
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

