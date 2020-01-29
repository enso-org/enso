You can run the tests by calling:
```
sbt syntaxJS/fullOptJS
wasm-pack test --headless --firefox --chrome --safari common/rust/parser
```

Note that --safari wasn't tested and there is also an external 
[issue](https://github.com/rustwasm/wasm-pack/issues/611) with --chrome. 