#include <map>

extern "C" {
    void *c_map_create();
    void c_map_insert(long, long, void *);
    void c_map_insert_many(long *, long *, long, void *);
    long c_map_member(long, void *);
    long c_map_lookup(long, long *, void *);
    void c_map_delete(long, void *);
    void c_map_delete_map(void *);
    void c_map_to_list(long *, long *, void *);
    long c_map_size(void *);
    long c_map_null(void *);
}
