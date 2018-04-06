#include <map>
#include "common.h"

typedef std::map<elem_t, elem_t> ptrmap_t;

extern "C" {
    void *c_map_create();
    void c_map_insert(elem_t, elem_t, void *);
    long c_map_member(elem_t, void *);
    long c_map_lookup(elem_t, elem_t *, void *);
    void c_map_delete(elem_t, void *);
    void c_map_delete_map(void *);
    void c_map_to_list(elem_t *, elem_t *, void *);
    long c_map_size(void *);
    long c_map_null(void *);
}
