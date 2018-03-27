#include <deque>
#include "common.h"

typedef std::deque<elem_t> ptrlist_t;

extern "C" {

void *c_list_create();
void c_list_push_front(elem_t , void *);
void c_list_push_back(elem_t, void *);
elem_t c_list_pop_front(void *);
elem_t c_list_pop_back(void *);
elem_t c_list_at(long, void *);
elem_t c_list_front(void *);
elem_t c_list_back(void *);
void c_list_to_list(elem_t *, void *);
void c_list_delete_list(void *);
long c_list_size(void *);
long c_list_null(void *);

}
