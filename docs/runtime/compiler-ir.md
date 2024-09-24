# Enso Compiler IR

Enso IR, currently implemented in Scala, with base class
`org.enso.compiler.core.IR`, is created from the output of the native
[parser](../parser/README.md). The IR is an immutable annotated AST subjected to
multiple passes. Every pass is a class implementing the
`org.enso.compiler.pass.IRPass` interface.

See [Runtime roadmap - static analysis](../runtime-roadmap.md#static-analysis)
for future goals.

## Visualization

The IR can be visualized using `--vm.D=enso.compiler.dumpIr` system property.
This will output a `.dot` file in [GraphViz](www.graphviz.org) format in the
`ir-dumps` directory for each IR in the program. The _dot_ file format is a
minimal textual format, that can be converted to a graphical representation
using the `dot` command from the GraphViz package. For example, on Ubuntu,
install `dot` with `sudo apt install graphviz`. Then, convert the `.dot` file to
a `.svg` image with `dot -Tsvg -o <output>.svg <input>.dot`. An example is:
![image.svg](https://github.com/user-attachments/assets/26ab8415-72cf-46da-bc63-f475e9fa628e)

See `org.enso.compiler.dump.IRDumper`.
