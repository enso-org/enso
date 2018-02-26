#include <set>

extern "C" {
    void *c_set_create();
    void c_set_insert(long, void *);
    void c_set_insert_many(long *, long, void *);
    long c_set_member(long, void *);
    void c_set_delete(long, void *);
    void c_set_delete_set(void *);
    void c_set_to_list(long *, void *);
    long c_set_size(void *);
    long c_set_null(void *);
}
