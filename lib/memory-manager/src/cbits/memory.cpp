#define _SILENCE_CXX17_OLD_ALLOCATOR_MEMBERS_DEPRECATION_WARNING

#include "memory.h"


extern "C"
{
    void *newManager(size_t itemSize, size_t itemsPerBlock)
    {
        return new MemoryManagerToUse(itemSize, itemsPerBlock);
    }
    void deleteManager(void *manager)
    {
        delete static_cast<MemoryManagerToUse*>(manager);
    }
    void *newItem(void *manager)
    {
        return static_cast<MemoryManagerToUse*>(manager)->newItem();
    }
    void *newItems(void *manager, size_t count)
    {
        return static_cast<MemoryManagerToUse*>(manager)->newItems(count);
    }
    void deleteItem(void *manager, void *item)
    {
        static_cast<MemoryManagerToUse*>(manager)->deleteItem(item);
    }

    void** acquireItemList(void *manager, size_t *outCount)
    {
        const auto items = static_cast<MemoryManagerToUse*>(manager)->allocatedItems();
        const auto count = items.size();

        void** ret = static_cast<void**>(std::malloc(sizeof(void*) * count));
        if(!ret)
        return nullptr;
        *outCount = count;
        void **itr = ret;
        for(auto item : items)
        *itr++ = item;

        return ret;
    }

    void releaseItemList(void **items)
    {
        std::free(items);
    }
}
