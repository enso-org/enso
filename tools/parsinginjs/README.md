This project shows how to execute parser (e.g. `TreeToIr`) in the browser.
Use following steps to reproduce:
```bash
enso$ sbt runtime-parser/assembly
enso$ mvn install:install-file
                -Dfile=engine/runtime-parser/target/scala-2.13/runtime-parser-assembly-0.0.0-dev.jar
                -DgroupId=org.enso.engine -DartifactId=runtime-parser
                -Dpackaging=jar -Dversion=1.0-SNAPSHOT
enso$ mvn -q -f tools/parsinginjs package exec:exec
```
and as a final step (which takes a while as transpiling `runtime-parser` takes a minute),
let's run the same code in the browser:
```bash
enso$ mvn -f tools/parsinginjs package bck2brwsr:aot bck2brwsr:show
```
As can be seen, the output of regular `exec:exec` and `bck2brwsr:show` is the same. Moreover
(once one opens browser console), one case see the output of transpiled code in the browser
console as well.
