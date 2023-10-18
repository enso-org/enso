This project shows how to execute parser (e.g. `TreeToIr`) in the browser.
Use following steps to reproduce:
```bash
enso$ sbt runtime-parser/assembly
enso$ mvn install:install-file
                -Dfile=engine/runtime-parser/target/scala-2.13/runtime-parser-assembly-0.0.0-dev.jar
                -DgroupId=org.enso.engine -DartifactId=runtime-parser
                -Dpackaging=jar -Dversion=1.0-SNAPSHOT
enso$ (cd lib/rust/parser/jni-js; wasm-pack build --target web)
enso$ mvn -q -f tools/parsinginjs package exec:exec
```
and as a final step (which takes a while as transpiling `runtime-parser` takes a minute),
let's run the same code in the browser:
```bash
enso$ mvn -f tools/parsinginjs package bck2brwsr:aot bck2brwsr:show
```
A page with a text area opens and one can write in any Enso code and parse
it with the _Parse!_ button. Pretty printed `IR` is then shown below.
