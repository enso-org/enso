#include <stddef.h>

extern "C"
{
	void *newManager(size_t itemSize, size_t itemsPerBlock);
	void deleteManager(void *manager);
	void *newItem(void *manager);
	void *newItems(void *manager, size_t count);
	void deleteItem(void *manager, void *item);

	void** acquireItemList(void *manager, size_t *outCount);
	void releaseItemList(void **items);
}