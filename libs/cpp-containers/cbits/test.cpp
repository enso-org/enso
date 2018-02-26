#include "test.h"

extern "C" {

long c_set_test_insert_and_lookup(long *ptr_insert, long n, long *ptr_idx, long m, void *ptr_set) {
    for (long i = 0; i < n; ++i)
      c_set_insert(ptr_insert[i], ptr_set);

    long sum = 0;
    for (long i = 0; i < m; ++i)
        sum += c_set_member(ptr_idx[i], ptr_set);

    return sum;
}

long c_set_test_insert_lookup_ordered(long n) {
    auto s = c_set_create();
    for (long i = 0; i < n; ++i)
        c_set_insert(i, s);

    long sum = 0;
    for (long i = 0; i < n; ++i)
        sum = c_set_member(n - i, s) ? 1 : 0;

    return sum;
}


long c_set_identity(long *ptr, long n) {
    return 42;
}

}
