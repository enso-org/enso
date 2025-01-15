# Enso Compiler IR

Enso IR, currently implemented in Scala, with base class
`org.enso.compiler.core.IR`, is created from the output of the native
[parser](../parser/README.md). The IR is an immutable annotated AST subjected to
multiple passes. Every pass is a class implementing the
`org.enso.compiler.pass.IRPass` interface.

See [Runtime roadmap - static analysis](../runtime-roadmap.md#static-analysis)
for future goals.

## Visualization

The IR can be visualized using the `enso.compiler.dumpIr` system property. The
value of the property is a class name of the IR dumper to use. The IR dumper is
a class implementation of `org.enso.compiler.dump.service.IRDumpService`.

Usage example:

```
$ ./built-distribution/*/bin/enso --vm.D enso.compiler.dumpIr=org.enso.compiler.dump.igv.IGVDumper --run tmp.enso
```

There are currently two implementations:

### org.enso.compiler.dump.graphviz.GraphVizDumper

Dumps the IR in the [GraphViz](www.graphviz.org) format. The _dot_ file format
is a minimal textual format, that can be converted to a graphical representation
using the `dot` command from the GraphViz package. For example, on Ubuntu,
install `dot` with `sudo apt install graphviz`. Then, convert the `.dot` file to
a `.svg` image with `dot -Tsvg -o <output>.svg <input>.dot`. An example is:
![image.svg](https://github.com/user-attachments/assets/26ab8415-72cf-46da-bc63-f475e9fa628e)

### org.enso.compiler.dump.igv.IGVDumper

Dumps the IR in the
[BGV](https://www.graalvm.org/graphio/javadoc/jdk/graal/compiler/graphio/package-summary.html)
format. The generated files can be opened with the
[IGV](https://www.graalvm.org/latest/tools/igv/) tool.
