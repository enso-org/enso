#pragma once

#include <stddef.h>

#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_set>
#include <utility>
#include <vector>

#ifdef _MSC_VER
#define NOMINMAX
#include <wrl/wrappers/corewrappers.h>
#endif

namespace locking_policy
{
    struct NoLocks
    {
        struct Guard {};
        Guard lock() { return {}; };
    };

    struct StdMutex
    {
        std::mutex mx;
        auto lock() { return std::unique_lock<std::mutex>(mx); }
    };

    // TODO implement directly on WinAPI for MinGW
    #ifdef _MSC_VER
    struct SRWLock
    {
        Microsoft::WRL::Wrappers::SRWLock mx;
        auto lock() { return mx.LockExclusive(); }
    };
    #endif

    struct Spinlock
    {
    private:
        enum LockState { Locked, Unlocked };
        std::atomic<LockState> state{ Unlocked };

        struct Guardian
        {
            Spinlock *s{};
            Guardian(Spinlock *s) : s(s) {}
            Guardian(const Guardian &) = delete;
            Guardian(Guardian &&rhs)
                : s(rhs.s)
            {
                rhs.s = nullptr;
            }
            ~Guardian()
            {
                if(s)
                    s->state.store(Unlocked, std::memory_order_release);
            }
        };
    public:

        auto lock()
        {
            while(state.exchange(Locked, std::memory_order_acquire) == Locked)
            { /* busy-wait */ }
            return Guardian{this};
        }
    };
}

template<typename LockPolicy>
class MemoryManager
{
    struct Block
    {
        size_t itemCount = 0;
        size_t unitializedItems = 0;
        void *memory = nullptr;

        Block(size_t itemSize, size_t itemsCount)
            : itemCount(itemsCount)
            , unitializedItems(itemsCount)
            , memory(std::malloc(sizeInBytes(itemSize)))
        {
            if(!memory)
                throw std::runtime_error("out of memory");
        }

        Block(const Block &) = delete;
        Block(Block &&rhs)
            : itemCount(rhs.itemCount)
            , unitializedItems(rhs.unitializedItems)
            , memory(rhs.memory)
        {
            rhs.memory = nullptr;
        }

        Block& operator=(const Block &) = delete;

        void *itemAtIndex(size_t itemSize, size_t index) const
        {
            return static_cast<char*>(memory) + index * itemSize;
        }

        bool contains(void *item, size_t itemSize) const
        {
            return memory <= item && static_cast<char*>(memory) + sizeInBytes(itemSize) > item;
        }

        size_t sizeInBytes(size_t itemSize) const
        {
            return itemSize * itemCount;
        }

        ~Block()
        {
            std::free(memory);
        }
    };

    const size_t itemSize;
    const size_t defaultItemsPerBlock;

    std::vector<Block> blocks;
    void * head = nullptr; // points to the last free, initialized element

    LockPolicy mx;


    Block &addBlock()
    {
        return addBlock(defaultItemsPerBlock);
    }

    Block &addBlock(size_t itemsCount)
    {
        blocks.emplace_back(itemSize, itemsCount);
        return blocks.back();
    }

    void *&storedPtr(void *item) const
    {
        return *static_cast<void**>(item);
    }

    Block &getBlock(void *item)
    {
        for(auto &block : blocks)
            if(block.contains(item, itemSize))
                return block;

        throw std::runtime_error("cannot find block for item");
    }

    void *obtainUnitializedItem(Block &block)
    {
        return obtainUnitializedItems(block, 1);
    }

    void *obtainUnitializedItems(Block &block, size_t count)
    {
        assert(block.unitializedItems >= count);
        const auto ret = block.itemAtIndex(itemSize, block.itemCount - block.unitializedItems);
        block.unitializedItems -= count;
        return ret;
    }

public:
    MemoryManager(size_t itemSize, size_t defaultItemsPerBlock)
        : itemSize(itemSize)
        , defaultItemsPerBlock(defaultItemsPerBlock)
    {
        if(itemSize < sizeof(void*))
            throw std::runtime_error("item size must be at least of size of a pointer");

        addBlock();
    }

    void *newItem()
    {
        const auto lockGuard = mx.lock();
        if(head != nullptr)
        {
            const auto ret = head;
            head = storedPtr(head);
            return ret;
        }
        else
        {
            auto &lastBlock = blocks.back();
            if(lastBlock.unitializedItems > 0)
                return obtainUnitializedItem(lastBlock);
            else
                return obtainUnitializedItem(addBlock());
        }
    }

    void *newItems(size_t count)
    {
        const auto lockGuard = mx.lock();
        for(auto &block : blocks)
            if(block.unitializedItems >= count)
                return obtainUnitializedItems(block, count);

        // If user requested more elements than default block size, prepare a bigger block.
        const auto countToAllocate = std::max(count, defaultItemsPerBlock);
        auto &newBlock = addBlock(countToAllocate);
        return obtainUnitializedItems(newBlock, count);
    }

    void deleteItem(void *item)
    {
        const auto lockGuard = mx.lock();
         storedPtr(item) = head;
         head = item;
    }

    auto allocatedItems()
    {
        const auto lockGuard = mx.lock();
        const auto addItemsFromBlock = [this] (auto &out, const Block &block) -> void
        {
            const auto perhapsUsedInBlock = block.itemCount - block.unitializedItems;
            for(auto i = 0u; i < perhapsUsedInBlock; ++i)
                out.insert(out.end(), block.itemAtIndex(itemSize, i));
        };

        std::unordered_set<void*> ret;

        // add all allocated
        for(const Block &block : blocks)
        addItemsFromBlock(ret, block);

        // remove allocated and then freed:
        // iterate over the free pointers list
        auto itr = this->head;
        while(itr != nullptr)
        {
            ret.erase(itr);
            itr = storedPtr(itr);
        }

        return ret;
    }
};

// Note: rough performance results (for alloc N elements, then free N elements benchmark)
//       of different locking policies across platforms:
//
//               | std::mutex | spinlock
//---------------+------------+-----------
// Windows VS    | 443 ms     | 107 ms
// Windows MinGW | 248 ms     | 144 ms
// Linux GCC     | 12 ms      | 220 ms
//---------------+------------+-----------
// Therefore, Windows uses spinlock, non-Windows uses std::mutex
#ifdef _WIN32
using LockingPolicyToUse = locking_policy::Spinlock;
#else
using LockingPolicyToUse = locking_policy::StdMutex;
#endif

using MemoryManagerToUse = MemoryManager<LockingPolicyToUse>;