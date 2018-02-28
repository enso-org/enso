#include <deque>

extern "C" {

void *c_list_create();
void c_list_push_front(long , void *);
void c_list_push_back(long, void *);
long c_list_pop_front(void *);
long c_list_pop_back(void *);
long c_list_at(long, void *);
long c_list_front(void *);
long c_list_back(void *);
void c_list_to_list(long *, void *);
void c_list_delete_list(void *);
long c_list_size(void *);
long c_list_null(void *);

}
