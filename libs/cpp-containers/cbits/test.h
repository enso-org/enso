#include "map.h"
#include "set.h"

extern "C" {

long c_set_test_insert_and_lookup(elem_t *, long, elem_t *, long, void *);

long c_set_test_insert_lookup_ordered(long n);

long c_set_identity(elem_t *, long);

}
