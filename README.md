# luna-core

## Development

Standard development cycle:

1. Write code, tests and benchmarks.
2. Use `stylish-haskell` to apply automatic formatting.
3. Use `hlint` to improve yours code quality:
   ```bash
   stack install hlint
   hlint . --report
   # view generated report.html in your browser
   ```
4. Use `weeder` to discover unused imports and libraries:
   ```bash
   stack install weeder
   stack build # remember to build the project before using weeder!
   weeder .
   ```
