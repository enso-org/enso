
#include <chrono>
#include <cstdlib>
#include <functional>
#include <iomanip>
#include <iostream>
#include <list>
#include <numeric>
#include <vector>

// BENCHMARK RUNNER ===========================================================

template <typename R, typename T> void run_benchmark(
        std::string bench_name,
        int n_runs,
        std::function<R(T)> benchmark,
        T function_arg
        ) {

    std::vector<double> times;

    for (auto i = 0; i < n_runs; ++i) {
        auto start = std::chrono::high_resolution_clock::now();
        R result = benchmark(function_arg);
        auto end = std::chrono::high_resolution_clock::now();

        auto difference = end - start;

        times.push_back(std::chrono::duration_cast<
                std::chrono::nanoseconds
                >(difference).count());
    }

    std::int64_t result_nanoseconds =
        std::accumulate(
                times.begin(),
                times.end(),
                0,
                [](auto a, auto b) {return a + b;}
                ) / n_runs;

    double result = result_nanoseconds / 1e9;

    std::cout << std::setprecision(10)
        << "BENCHMARK: " << bench_name
        << " RUNS: " << n_runs
        << " RESULT: " << result << " secs"
        << std::endl;
}

// LINKED LIST ================================================================

struct linked_list {
    std::int64_t node_value;
    linked_list* next;

    static linked_list* generate(std::int64_t num_elements) {
        linked_list* result = nullptr;
        while (num_elements >= 0) {
            result = new linked_list(num_elements, result);
            --num_elements;
        }

        return result;
    }

    using fold_fn_t = std::int64_t(std::int64_t, std::int64_t);

    static std::int64_t fold(
            std::function<fold_fn_t> f,
            std::int64_t initial_val,
            linked_list* xs
            ) {
        std::int64_t accumulator = initial_val;

        linked_list* position = xs;

        while (position->next != nullptr) {
            accumulator = f(accumulator, position->node_value);
            position = position->next;
        }
        accumulator += position->node_value;

        return accumulator;
    }

    ~linked_list() {}

    void operator delete(void* ptr) {
        auto real_ptr = static_cast<linked_list*>(ptr);

        while (real_ptr->next != nullptr) {
            auto next_ptr = real_ptr->next;
            ::operator delete(real_ptr);
            real_ptr = next_ptr;
        }
    }

    linked_list(std::int64_t a, linked_list* next) : node_value(a), next(next) {};
    linked_list(std::int64_t a) : node_value(a), next(nullptr) {};
};

// FIXTURES ===================================================================

std::int64_t sum_tco(std::int64_t sum_to) {
    std::int64_t result = 0;

    for (int64_t i = 0; i <= sum_to; ++i) {
        result += i;
    }

    return result;
}

std::int64_t sum_list(linked_list* list) {
    std::int64_t accumulator = 0;
    auto current_pos = list;

    while (current_pos->next != nullptr) {
        accumulator += current_pos->node_value;
        current_pos = current_pos->next;
    }

    accumulator += current_pos->node_value;

    return accumulator;
}

linked_list* reverse_list(linked_list* list) {
    linked_list* result = nullptr;
    linked_list* current = list;

    while (current != nullptr) {
        result = new linked_list(current->node_value, result);
        current = current->next;
    }

    return result;
}

std::int64_t sum_list_left_fold(linked_list* list) {
    return linked_list::fold(
            [](std::int64_t a, std::int64_t b) { return a + b; },
            0,
            list
            );
}

// MAIN =======================================================================

int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cout << "Please run as `benchmark [sum_to] [num_elements]`"
                  << std::endl;

        return 1;
    }

    std::int64_t sum_to = atoll(argv[1]);
    std::int64_t num_elements = atoll(argv[2]);

    auto n_element_list = linked_list::generate(num_elements);

    run_benchmark<std::int64_t, std::int64_t>(
            "sumTCO",
            10,
            sum_tco,
            sum_to
            );

    run_benchmark<std::int64_t, linked_list*>(
            "sumList",
            10,
            sum_list,
            n_element_list
            );

    run_benchmark<linked_list*, linked_list*>(
            "reverseList",
            10,
            reverse_list,
            n_element_list
            );

    run_benchmark<std::int64_t, linked_list*>(
            "sumListLeftFold",
            10,
            sum_list_left_fold,
            n_element_list
            );

    delete n_element_list;

    return 0;
}

