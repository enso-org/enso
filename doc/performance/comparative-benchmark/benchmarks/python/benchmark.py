
from functools import reduce

# Constants ===================================================================
million = 1000000
hundred_million = 100000000

# Linked List Impl ============================================================

class linked_list:
    def __init__(self, value, rest):
        self.value = value
        self.rest = rest

    @staticmethod
    def foldl(fn, init_val, xs):
        accumulator = init_val

        position = xs

        while position.rest != None:
            accumulator = fn(accumulator, position.value)
            position = position.rest

        accumulator += position.value

        return accumulator

    @staticmethod
    def gen_list(num_elements):
        result = None

        while num_elements >= 0:
            result = linked_list(num_elements, result)
            num_elements -= 1

        return result

# Fixtures ====================================================================

def sum_tco(sum_to):
    accumulator = 0

    for i in range(0, sum_to + 1):
        accumulator += i

    return accumulator

def sum_list(xs):
    accumulator = 0
    current_pos = xs

    while current_pos.rest != None:
        accumulator += current_pos.value
        current_pos = current_pos.rest

    accumulator += current_pos.value

    return accumulator

def reverse_list(xs):
    result = None
    current = xs

    while current != None:
        result = linked_list(current.value, result)
        current = current.rest

    return current

def sum_list_left_fold(xs):
    return linked_list.foldl((lambda x, y: x + y), 0, xs)


# Runner ======================================================================

def run_benchmark(fn_string, setup_string, bench_name, num_runs):
    time_results_raw = timeit.repeat(
        fn_string,
        setup_string,
        number=1,
        repeat=num_runs
    )

    time_result_raw = reduce((lambda x,y: x + y), time_results_raw) / num_runs
    time_result = round(time_result_raw, ndigits=5)

    print(f"NAME: {bench_name}, RUNS: {num_runs} RESULT: {time_result} secs")

if __name__ == "__main__":
    import timeit
    num_runs = 10
    million_element_list = linked_list.gen_list(million)

    run_benchmark("sum_tco(hundred_million)",
                  "from __main__ import sum_tco, hundred_million",
                  "sumTCO",
                  num_runs)

    run_benchmark("sum_list(million_element_list)",
                  "from __main__ import sum_list, million_element_list",
                  "sumList",
                  num_runs)

    run_benchmark("reverse_list(million_element_list)",
                  "from __main__ import reverse_list, million_element_list",
                  "reverselist",
                  num_runs)

    run_benchmark("sum_list_left_fold(million_element_list)",
                  "from __main__ import sum_list_left_fold, million_element_list",
                  "sumListLeftFold",
                  num_runs)

