#include <set>
#include "common.h"

typedef std::set<elem_t> ptrset_t;

extern "C" {
    void *c_set_create();
    void c_set_insert(elem_t, void *);
    long c_set_member(elem_t, void *);
    void c_set_delete(elem_t, void *);
    void c_set_delete_set(void *);
    void c_set_to_list(elem_t *, void *);
    long c_set_size(void *);
    long c_set_null(void *);
}
