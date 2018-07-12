#pragma once

#include <stddef.h>

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
        size_t unitializedItems = 0;
        void *memory = nullptr;

        Block(size_t itemSize, size_t itemsPerBlock)
            : memory(std::malloc(itemSize * itemsPerBlock))
            , unitializedItems(itemsPerBlock)
        {
            if(!memory)
                throw std::runtime_error("out of memory");
        }

        Block(const Block &) = delete;
        Block(Block &&rhs)
            : unitializedItems(rhs.unitializedItems)
            , memory(rhs.memory)
        {
            rhs.memory = nullptr;
        }

        Block& operator=(const Block &) = delete;

        void *itemAtIndex(size_t itemSize, size_t index) const
        {
            return static_cast<char*>(memory) + index * itemSize;
        }

        ~Block()
        {
            std::free(memory);
        }
    };

    const size_t itemSize;
    const size_t itemsPerBlock;
    const size_t blockSize = itemSize * itemsPerBlock;

    //boost::container::small_vector<Block, 200> blocks;
    std::vector<Block> blocks;
    void * head = nullptr; // points to the last free, initialized element
    //std::vector<void*> freed;

    LockPolicy mx;
    //boost::mutex mx;


    decltype(auto) addBlock()
    {
        blocks.emplace_back(itemSize, itemsPerBlock);
        return blocks.back();
    }

    void *&storedPtr(void *item) const
    {
        return *static_cast<void**>(item);
    }

    Block &getBlock(void *item)
    {
        for(auto &block : blocks)
            if(belongsTo(block, item))
                return block;

        throw std::runtime_error("cannot find block for item");
    }

    bool belongsTo(const Block &block, void *item)
    {
        return block.memory <= item && static_cast<char*>(block.memory) + blockSize > item;
    }

    void *obtainUnitializedItem(Block &block)
    {
        return obtainUnitializedItems(block, 1);
    }

    void *obtainUnitializedItems(Block &block, size_t count)
    {
        assert(block.unitializedItems >= count);
        const auto ret = block.itemAtIndex(itemSize, itemsPerBlock - block.unitializedItems);
        block.unitializedItems -= count;
        return ret;
    }

public:
    MemoryManager(size_t itemSize, size_t itemsPerBlock)
        : itemSize(itemSize)
        , itemsPerBlock(itemsPerBlock)
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
        assert(count <= itemsPerBlock);
        for(auto &block : blocks)
            if(block.unitializedItems >= count)
                return obtainUnitializedItems(block, count);

        return obtainUnitializedItems(addBlock(), count);
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
            const auto perhapsUsedInBlock = itemsPerBlock - block.unitializedItems;
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