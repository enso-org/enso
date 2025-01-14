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
This will output a standalone `.html` file that visualizes the graph with the
[vis.js](https://visjs.org/) library in the `ir-dumps` directory for each IR in
the program .

When the system property is set, HTML files will be printed in the stdout like
so:

```
IR dumped to file:///home/user/enso/ir-dumps/Standard.Base.Data.Numeric.html
```

It should be enough to just, e.g., ctrl+click on the link to open the file in
your browser.

See `org.enso.compiler.dump.IRDumper`.
