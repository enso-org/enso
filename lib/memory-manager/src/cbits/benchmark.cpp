#define _SILENCE_CXX17_OLD_ALLOCATOR_MEMBERS_DEPRECATION_WARNING

#include "memory.h"

#include <iostream>
#include <random>

// Define when using C++ benchmark - so root API calls don't get inlined
// (similarly like Haskell FFI prevents inlining)
#ifdef PREVENT_INLINE
#ifdef _MSC_VER
#define NOINLINE  __declspec(noinline)
#else
#define NOINLINE  __attribute__ ((noinline))
#endif
#else
#define NOINLINE
#endif

// Naive malloc-based manager for comparison in benchmarks
class MemoryManagerC
{
    const size_t itemSize;
public:
    MemoryManagerC(size_t itemSize) : itemSize(itemSize) {}

    void *newItem()             noexcept { return std::malloc(itemSize); }
    void deleteItem(void *item) noexcept { return std::free(item);       }
};

template<typename F, typename ...Args>
static auto duration(F&& func, Args&&... args)
{
    const auto start = std::chrono::steady_clock::now();
    std::invoke(std::forward<F>(func), std::forward<Args>(args)...);
    return std::chrono::steady_clock::now() - start;
}

template<typename F, typename ...Args>
static auto measure(std::string text, F&& func, Args&&... args)
{
    const auto t = duration(std::forward<F>(func), std::forward<Args>(args)...);
    std::cout << text << " took " << std::chrono::duration_cast<std::chrono::milliseconds>(t).count() << " ms" << std::endl;
    return t;
}

using Action = int; // index of item to delete or -1 to create an item

static std::vector<Action> generateRandomizedActions(int N, double createProbability)
{
    int toCreate = N;
    int existingItems = 0;
    std::vector<Action> actions; // -1 means obtain new item, positive value means delete by index
    actions.reserve(N * 2);

    std::default_random_engine generator; // TODO allow passing seed so values are repeatable (at least for a given stdlib implementation)
    std::bernoulli_distribution distribution(createProbability);

    while(toCreate || existingItems)
    {
        if(toCreate && (distribution(generator) || !existingItems))
        {
            actions.emplace_back(-1);
            --toCreate;
            ++existingItems;
        }
        else
        {
            // pick random index
            const auto index = std::uniform_int_distribution<>(0, existingItems - 1)(generator);
            actions.push_back(index);
            --existingItems;
        }
    }
    return actions;
}

template<typename Manager>
static void execute(Manager &mgr, std::vector<void*> &items, const std::vector<Action> &actions)
{
    for(auto action : actions)
    {
        if(action < 0)
            items.push_back(mgr.newItem());
        else
        {
            mgr.deleteItem(items[action]);
            std::swap(items[action], items.back());
            items.pop_back();
        }
    }
}

template<typename Manager>
struct Test
{
private:
    NOINLINE static void *newManager(size_t itemSize, size_t itemsPerBlock)
    {
        return new Manager(itemSize, itemsPerBlock);
    }
    NOINLINE static void deleteManager(void *manager)
    {
        delete static_cast<Manager*>(manager);
    }

    static void *newItem(void *manager)
    {
        return static_cast<Manager*>(manager)->newItem();
    }
    static void deleteItem(void *manager, void *item)
    {
        static_cast<Manager*>(manager)->deleteItem(item);
    }

    static void execute(void *manager, std::vector<void*> &items, const std::vector<Action> &actions)
    {
        ::execute(*static_cast<Manager*>(manager), items, actions);
    }

public:
    void test(std::string text, int N, size_t size, size_t itemsPerBlock)
    {
        const auto actions = generateRandomizedActions(N, 0.7);

        std::vector<void*> items(N);
        for(int i = 0; i < 10; i++)
        {
            measure(text + " alloc+free sequence", [&]
            {
                const auto mgr = newManager(size, itemsPerBlock);
                for(int i = 0; i < N; i++)
                {
                    const auto item = newItem(mgr);
                    //std::memset(item, 0, size);
                    deleteItem(mgr, item);
                }
                deleteManager(mgr);
            });

            items.resize(N);
            measure(text + " all allocs; all frees", [&]() mutable
            {
                const auto mgr = newManager(size, itemsPerBlock);
                for(auto &ptr : items)
                    ptr = newItem(mgr);
                for(auto &ptr : items)
                    deleteItem(mgr, ptr);
                deleteManager(mgr);
            });

            items.clear();
            measure(text + " random", [&]() mutable
            {
                const auto mgr = newManager(size, itemsPerBlock);
                execute(mgr, items, actions);
                deleteManager(mgr);
            });
        }
    }

    void threadedTest(std::string text, int N, size_t size)
    {
        auto threadActions = generateRandomizedActions(N, 0.7);
        for(int i = 0; i < 10; i++)
        {
            const auto mgr = newManager(size, 1024);
            std::atomic_int readyThreads{ 0 };
            std::vector<std::thread> threads;
            for(int j = 0; j < 4; j++)
            {
                threads.emplace_back([&]
                {
                    readyThreads++;
                    while(readyThreads.load() != 4)
                        ; // busy wait

                    std::vector<void*> threadItems;
                    threadItems.reserve(N);
                    measure(text + " random", [&]
                    {
                        execute(mgr, threadItems, threadActions);
                    });
                });
            }

            for(auto &t : threads)
                t.join();

            deleteManager(mgr);
        }
    }
};

extern "C"
{
    // BELOW APIs are for tests / benchmarks only //////////////////
    void benchmark(size_t N, size_t itemSize, size_t itemsPerBlock)
    {
        Test<MemoryManagerToUse> test;
        test.test("c++ memory manager", N, itemSize, itemsPerBlock);
    }
    void randomizedBenchmark(size_t N, size_t itemSize, double probability)
    {
        auto actions = generateRandomizedActions(N, probability);
        std::vector<void*> items;
        items.reserve(N);

        MemoryManagerToUse mgr{itemSize, 1024};
        //measure("inner", ::execute<MemoryManagerToUse>, mgr, items, actions);
        ::execute(mgr, items, actions);

    }
    void *justReturn(size_t id)
    {
        return reinterpret_cast<void*>(id); // pure evil
    }
}

int main()
{
    // std::cout << "Will run benchmark (unless optimized away)\n";
    // benchmark(10'000'000, 64);
    for(int i = 0; i < 10; i++)
        measure("randomized pattern", randomizedBenchmark, 10000000, 50, 0.7);
}
