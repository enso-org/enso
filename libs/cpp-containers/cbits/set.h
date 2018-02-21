#include <set>
#include <iostream>

extern "C" {
    void *c_create_std_set();
    void c_insert(long, void *);
    void c_insert_many(long *, long, void *);
    long c_member(long, void *);
    void c_delete(long, void *);
    void c_delete_set(void *);
    void c_to_list(long *, void *);
    long c_size(void *);
    bool c_null(void *);
}
