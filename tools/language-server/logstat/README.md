# logstat

The tool for analyzing performance from the logs output. It takes the log file
and the specification file containing the log entries to look for. Then it can
extract the operations from logs and calculate the durations and averages of
each operation.

Logstat is supposed to be used together with `wstest` tool. Take a look at
`wstest/benchmarks` directory of example usages.
