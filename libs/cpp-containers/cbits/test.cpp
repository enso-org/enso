#include "test.h"

extern "C" {

// TODO the tests in this file are outdated, since they were created for intsets.

long c_set_test_insert_and_lookup(elem_t *ptr_insert, long n, elem_t *ptr_idx, long m, void *ptr_set) {
//     for (long i = 0; i < n; ++i)
//       c_set_insert(ptr_insert[i], ptr_set);
//
//     long sum = 0;
//     for (long i = 0; i < m; ++i)
//         sum += c_set_member(ptr_idx[i], ptr_set);

    return 0;
}

long c_set_test_insert_lookup_ordered(long n) {
//     auto s = c_set_create();
//     for (long i = 0; i < n; ++i)
//         c_set_insert(i, s);
//
//     long sum = 0;
//     for (long i = 0; i < n; ++i)
//         sum += (c_set_member(n - i, s) == 1) ? 1 : 0;

    return 0;
}


long c_set_identity(elem_t *ptr, long n) {
    return 42;
}

}
